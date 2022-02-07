use std::io::{self};
use std::os::unix::prelude::{AsRawFd, CommandExt, FromRawFd};
use std::pin::Pin;
use std::process::{abort, Command};
use std::task::{Context, Poll};

use eyre::{bail, Result};
use futures::Future;
use nix::pty::{forkpty, Winsize};
use nix::sys::termios::Termios;
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
use nix::unistd::{ForkResult, Pid};
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncWrite};
use tokio::signal::unix::{signal, Signal, SignalKind};
use tokio::task::spawn_blocking;

mod ioctl {
    use super::Winsize;
    use libc::TIOCSWINSZ;
    use nix::ioctl_write_ptr_bad;

    ioctl_write_ptr_bad!(tiocswinsz, TIOCSWINSZ, Winsize);
}

async fn asyncify<F, T>(f: F) -> Result<T>
where
    F: FnOnce() -> Result<T> + Send + 'static,
    T: Send + 'static,
{
    match spawn_blocking(f).await {
        Ok(res) => res,
        Err(_) => bail!("background task failed",),
    }
}

pub struct Child {
    pub tty: File,
    pub pid: Pid,
}

pub struct ChildHandle {
    pub tty: File,
}

pub struct WaitPid {
    pid: Pid,
    signal: Signal,
}

impl WaitPid {
    pub fn new(pid: Pid) -> Self {
        Self {
            pid,
            signal: signal(SignalKind::child()).unwrap(),
        }
    }
}

impl Future for WaitPid {
    type Output = nix::Result<WaitStatus>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let _ = self.signal.poll_recv(cx);
        match waitpid(self.pid, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::StillAlive) => Poll::Pending,
            result => Poll::Ready(result),
        }
    }
}

impl Child {
    pub async fn handle(&self) -> io::Result<ChildHandle> {
        Ok(ChildHandle {
            tty: self.tty.try_clone().await?,
        })
    }
}

impl ChildHandle {
    pub async fn resize_window(&mut self, winsize: Winsize) -> Result<()> {
        let fd = self.tty.as_raw_fd();
        asyncify(move || unsafe {
            ioctl::tiocswinsz(fd, &winsize as *const Winsize)?;
            Ok(())
        })
        .await
    }
}

pub async fn spawn(
    mut cmd: Command,
    winsize: Option<Winsize>,
    termios: Option<Termios>,
) -> Result<Child> {
    asyncify(move || unsafe {
        let res = forkpty(winsize.as_ref(), termios.as_ref())?;
        match res.fork_result {
            ForkResult::Parent { child } => Ok(Child {
                pid: child,
                tty: File::from_raw_fd(res.master),
            }),
            ForkResult::Child => {
                cmd.exec();
                abort();
            }
        }
    })
    .await
}

impl AsyncRead for Child {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        Pin::new(&mut self.tty).poll_read(cx, buf)
    }
}

impl AsyncWrite for Child {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, io::Error>> {
        Pin::new(&mut self.tty).poll_write(cx, buf)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        Pin::new(&mut self.tty).poll_flush(cx)
    }

    fn poll_shutdown(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<(), io::Error>> {
        Pin::new(&mut self.tty).poll_shutdown(cx)
    }
}

impl AsyncRead for ChildHandle {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        Pin::new(&mut self.tty).poll_read(cx, buf)
    }
}

impl AsyncWrite for ChildHandle {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, io::Error>> {
        Pin::new(&mut self.tty).poll_write(cx, buf)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        Pin::new(&mut self.tty).poll_flush(cx)
    }

    fn poll_shutdown(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<(), io::Error>> {
        Pin::new(&mut self.tty).poll_shutdown(cx)
    }
}

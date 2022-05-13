use std::net::SocketAddr;
use std::path::PathBuf;
use std::pin::Pin;
use std::process::Command;
use std::str;
use std::sync::Arc;

use clap::Parser;
use color_eyre::eyre::Result;
use eyre::{bail, Context};
use futures::future::{ready, Ready};
use futures::Future;
use metrics_exporter_prometheus::PrometheusBuilder;
use nix::pty::Winsize;
use pty::ChildHandle;
use thrussh::server::{self, Auth, Session};
use thrussh::{ChannelId, CryptoVec};
use thrussh_keys::decode_secret_key;
use thrussh_keys::key::KeyPair;
use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;
use tokio::select;
use tokio::time::Instant;
use tracing::{debug, error, info, info_span, trace, warn, Instrument};
use tracing_subscriber::EnvFilter;

use crate::pty::WaitPid;

mod metrics;
mod pty;

use crate::metrics::reported::*;
use crate::metrics::{decrement_gauge, histogram, increment_counter, increment_gauge};

/// SSH-compatible server for playing Xanthous
#[derive(Parser, Debug)]
struct Opts {
    /// Address to bind to
    #[clap(long, short = 'a', default_value = "0.0.0.0:22")]
    address: String,

    /// Address to listen to for metrics
    #[clap(long, default_value = "0.0.0.0:9000")]
    metrics_address: SocketAddr,

    /// Format to use when emitting log events
    #[clap(
        long,
        env = "LOG_FORMAT",
        default_value = "full",
        possible_values = &["compact", "full", "pretty", "json"]
    )]
    log_format: String,

    /// Full path to the xanthous binary
    #[clap(long, env = "XANTHOUS_BINARY_PATH")]
    xanthous_binary_path: String,

    /// Path to a file containing the ed25519 secret key for the server
    #[clap(long, env = "SECRET_KEY_FILE")]
    secret_key_file: PathBuf,

    /// Level to log at
    #[clap(long, env = "LOG_LEVEL", default_value = "info")]
    log_level: String,
}

impl Opts {
    async fn read_secret_key(&self) -> Result<KeyPair> {
        let mut file = File::open(&self.secret_key_file)
            .await
            .context("Reading secret key file")?;
        let mut secret_key = Vec::with_capacity(464);
        file.read_to_end(&mut secret_key).await?;
        Ok(decode_secret_key(str::from_utf8(&secret_key)?, None)?)
    }

    async fn ssh_server_config(&self) -> Result<server::Config> {
        let key_pair = self.read_secret_key().await?;

        Ok(server::Config {
            server_id: "SSH-2.0-xanthous".to_owned(),
            keys: vec![key_pair],
            ..Default::default()
        })
    }

    fn init_logging(&self) -> Result<()> {
        let filter = EnvFilter::try_new(&self.log_level)?;
        let s = tracing_subscriber::fmt().with_env_filter(filter);

        match self.log_format.as_str() {
            "compact" => s.compact().init(),
            "full" => s.init(),
            "pretty" => s.pretty().init(),
            "json" => s.json().with_current_span(true).init(),
            f => bail!("Invalid log format `{}`", f),
        }

        Ok(())
    }
}

struct Handler {
    address: SocketAddr,
    xanthous_binary_path: &'static str,
    username: Option<String>,
    child: Option<ChildHandle>,
}

async fn run_child(
    mut child: pty::Child,
    mut server_handle: server::Handle,
    channel_id: ChannelId,
) -> Result<()> {
    let mut buf = [0; 2048];
    loop {
        select! {
            r = child.tty.read(&mut buf)  => {
                let read_bytes = r?;
                if read_bytes == 0 {
                    info!("EOF received from process");
                    let _ = server_handle.close(channel_id).await;
                    return Ok(())
                } else {
                    trace!(?read_bytes, "read bytes from child");
                    let _ = server_handle.data(channel_id, CryptoVec::from_slice(&buf[..read_bytes])).await;
                }
            }
            status = WaitPid::new(child.pid) => {
                match status {
                    Ok(_status) => info!("Child exited"),
                    Err(error) => error!(%error, "Child failed"),
                }
                let _ = server_handle.close(channel_id).await;
                return Ok(())
            }
        }
    }
}

impl Handler {
    async fn spawn_shell(
        &mut self,
        mut handle: server::Handle,
        channel_id: ChannelId,
        term: String,
        winsize: Winsize,
    ) -> Result<()> {
        let mut cmd = Command::new(self.xanthous_binary_path);
        cmd.env("TERM", term);
        if let Some(username) = &self.username {
            cmd.args(["--name", username]);
        }
        cmd.arg("--disable-saving");

        let child = pty::spawn(cmd, Some(winsize), None).await?;
        info!(pid = %child.pid, "Spawned child");
        increment_gauge!(RUNNING_PROCESSES, 1.0);
        self.child = Some(child.handle().await?);
        tokio::spawn(
            async move {
                let span = info_span!("child", pid = %child.pid);
                if let Err(error) = run_child(child, handle.clone(), channel_id)
                    .instrument(span.clone())
                    .await
                {
                    span.in_scope(|| error!(%error, "Error running child"));
                    let _ = handle.close(channel_id).await;
                }
                decrement_gauge!(RUNNING_PROCESSES, 1.0);
            }
            .in_current_span(),
        );
        Ok(())
    }
}

#[allow(clippy::type_complexity)]
impl server::Handler for Handler {
    type Error = eyre::Error;
    type FutureAuth = Ready<Result<(Self, Auth)>>;
    type FutureUnit = Pin<Box<dyn Future<Output = Result<(Self, Session)>> + Send + 'static>>;
    type FutureBool = Ready<Result<(Self, Session, bool)>>;

    fn finished_auth(self, auth: Auth) -> Self::FutureAuth {
        ready(Ok((self, auth)))
    }

    fn finished_bool(self, b: bool, session: Session) -> Self::FutureBool {
        ready(Ok((self, session, b)))
    }

    fn finished(self, session: Session) -> Self::FutureUnit {
        Box::pin(ready(Ok((self, session))))
    }

    fn auth_none(mut self, username: &str) -> Self::FutureAuth {
        info!(%username, "Accepted new connection");
        self.username = Some(username.to_owned());
        self.finished_auth(Auth::Accept)
    }

    fn auth_password(mut self, username: &str, _password: &str) -> Self::FutureAuth {
        info!(%username, "Accepted new connection");
        self.username = Some(username.to_owned());
        self.finished_auth(Auth::Accept)
    }

    fn auth_publickey(
        mut self,
        username: &str,
        _: &thrussh_keys::key::PublicKey,
    ) -> Self::FutureAuth {
        info!(%username, "Accepted new connection");
        self.username = Some(username.to_owned());
        self.finished_auth(Auth::Accept)
    }

    fn pty_request(
        mut self,
        channel: thrussh::ChannelId,
        term: &str,
        col_width: u32,
        row_height: u32,
        pix_width: u32,
        pix_height: u32,
        modes: &[(thrussh::Pty, u32)],
        session: Session,
    ) -> Self::FutureUnit {
        let term = term.to_owned();
        let modes = modes.to_vec();
        Box::pin(async move {
            debug!(
                %term,
                %col_width,
                %row_height,
                %pix_width,
                %pix_height,
                ?modes,
                "PTY Requested"
            );

            self.spawn_shell(
                session.handle(),
                channel,
                term,
                Winsize {
                    ws_row: row_height as _,
                    ws_col: col_width as _,
                    ws_xpixel: pix_width as _,
                    ws_ypixel: pix_height as _,
                },
            )
            .await?;

            Ok((self, session))
        })
    }

    fn window_change_request(
        mut self,
        _channel: ChannelId,
        col_width: u32,
        row_height: u32,
        pix_width: u32,
        pix_height: u32,
        session: Session,
    ) -> Self::FutureUnit {
        Box::pin(async move {
            if let Some(child) = self.child.as_mut() {
                trace!(%row_height, %col_width, "Window resize request received");
                child
                    .resize_window(Winsize {
                        ws_row: row_height as _,
                        ws_col: col_width as _,
                        ws_xpixel: pix_width as _,
                        ws_ypixel: pix_height as _,
                    })
                    .await?;
            } else {
                warn!("Resize request received without child process; ignoring");
            }

            Ok((self, session))
        })
    }

    fn data(
        mut self,
        _channel: thrussh::ChannelId,
        data: &[u8],
        session: Session,
    ) -> Self::FutureUnit {
        trace!(data = %String::from_utf8_lossy(data), raw_data = ?data);
        let data = data.to_owned();
        Box::pin(async move {
            if let Some(child) = self.child.as_mut() {
                child.write_all(&data).await?;
            } else {
                warn!("Data received without child process; ignoring");
            }

            Ok((self, session))
        })
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;
    let opts = Box::leak::<'static>(Box::new(Opts::parse()));
    opts.init_logging()?;
    PrometheusBuilder::new()
        .listen_address(opts.metrics_address)
        .install()?;
    metrics::register();

    let config = Arc::new(opts.ssh_server_config().await?);
    info!(address = %opts.address, "Listening for new SSH connections");
    let listener = TcpListener::bind(&opts.address).await?;

    loop {
        let (stream, address) = listener.accept().await?;
        increment_counter!(CONNECTIONS_ACCEPTED);
        increment_gauge!(ACTIVE_CONNECTIONS, 1.0);
        let config = config.clone();
        let handler = Handler {
            xanthous_binary_path: &opts.xanthous_binary_path,
            address,
            username: None,
            child: None,
        };
        tokio::spawn(async move {
            let span = info_span!("client", address = %handler.address);
            let start = Instant::now();
            if let Err(error) = server::run_stream(config, stream, handler)
                .instrument(span.clone())
                .await
            {
                span.in_scope(|| error!(%error));
            }
            let duration = start.elapsed();
            span.in_scope(|| info!(duration_ms = %duration.as_millis(), "Client disconnected"));
            histogram!(CONNECTION_DURATION, duration);
            decrement_gauge!(ACTIVE_CONNECTIONS, 1.0);
        });
    }
}

#[cfg(test)]
mod tests {
    use tempfile::NamedTempFile;

    use super::*;

    #[tokio::test]
    async fn read_secret_key() {
        use std::io::Write;

        let mut file = NamedTempFile::new().unwrap();
        file.write_all(
            b"
-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACAYz80xcK7jYxZMAl6apIHKRtB0Z2U78gG39c1QaIhgMwAAAJB9vxK9fb8S
vQAAAAtzc2gtZWQyNTUxOQAAACAYz80xcK7jYxZMAl6apIHKRtB0Z2U78gG39c1QaIhgMw
AAAEDNZ0d3lLNBGU6Im4JOpr490TOjm+cB7kMVXjVg3iCowBjPzTFwruNjFkwCXpqkgcpG
0HRnZTvyAbf1zVBoiGAzAAAACHRlc3Qta2V5AQIDBAU=
-----END OPENSSH PRIVATE KEY-----
",
        )
        .unwrap();

        let opts: Opts = Opts::parse_from(&[
            "xanthous-server".as_ref(),
            "--xanthous-binary-path".as_ref(),
            "/bin/xanthous".as_ref(),
            "--secret-key-file".as_ref(),
            file.path().as_os_str(),
        ]);
        opts.read_secret_key().await.unwrap();
    }
}

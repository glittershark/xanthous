use std::future::Future;
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::{Arc, RwLock};
use std::task::{Context, Poll, Waker};

type Waiter<Env, T> = Box<dyn Fn(&mut Env, &T)>;

pub struct Promise<Env, T> {
    inner: Arc<RwLock<Inner<T>>>,
    waiters: Arc<RwLock<Vec<Waiter<Env, T>>>>,
}

pub struct Complete<T> {
    inner: Arc<RwLock<Inner<T>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cancelled;

struct Inner<T> {
    value: Option<Arc<T>>,
    waker: Option<Waker>,
}

pub fn promise<Env, T>() -> (Complete<T>, Promise<Env, T>) {
    let inner = Arc::new(RwLock::new(Inner {
        value: None,
        waker: None,
    }));
    let promise = Promise {
        inner: inner.clone(),
        waiters: Arc::new(RwLock::new(Vec::new())),
    };
    let complete = Complete { inner };
    (complete, promise)
}

pub trait Fulfillable {
    type Result;

    fn fulfill(&self, val: Self::Result);
}

ref_impl! {
    impl<T: Fulfillable> Fulfillable for &T {
        type Result = T::Result;
        fn fulfill(&self, val: Self::Result) {
            (**self).fulfill(val)
        }
    }
}

impl<T> Fulfillable for Complete<T> {
    type Result = T;
    fn fulfill(&self, val: T) {
        let mut inner = self.inner.write().unwrap();
        inner.value = Some(Arc::new(val));
        if let Some(waker) = inner.waker.take() {
            waker.wake()
        }
    }
}

pub struct CompleteContramap<A, B, F> {
    complete: Complete<B>,
    f: F,
    marker: PhantomData<A>,
}

impl<A, B, F: FnOnce(A) -> B + Copy> Fulfillable
    for CompleteContramap<A, B, F>
{
    type Result = A;
    fn fulfill(&self, val: A) {
        let b = (self.f)(val);
        self.complete.fulfill(b)
    }
}

pub struct CompleteContramapOpt<A, B, F> {
    complete: Complete<B>,
    f: F,
    marker: PhantomData<A>,
}

impl<A, B, F: FnOnce(A) -> Option<B> + Copy> Fulfillable
    for CompleteContramapOpt<A, B, F>
{
    type Result = A;
    fn fulfill(&self, val: A) {
        match (self.f)(val) {
            Some(b) => self.complete.fulfill(b),
            None => (),
        }
    }
}

impl<T> Complete<T> {
    pub fn contramap<A, F: FnOnce(A) -> T>(
        self,
        f: F,
    ) -> CompleteContramap<A, T, F> {
        CompleteContramap {
            complete: self,
            f,
            marker: PhantomData,
        }
    }

    pub fn contramap_opt<A, F: Fn(A) -> Option<T>>(
        self,
        f: F,
    ) -> CompleteContramapOpt<A, T, F> {
        CompleteContramapOpt {
            complete: self,
            f,
            marker: PhantomData,
        }
    }
}

pub trait Cancellable {
    fn cancel(&mut self);
}

impl<T, A: Fulfillable<Result = Result<T, Cancelled>>> Cancellable for A {
    fn cancel(&mut self) {
        self.fulfill(Err(Cancelled))
    }
}

pub trait ResultFulfillable: Fulfillable {
    type Okay;
    type Error;
    fn ok(&mut self, val: Self::Okay);
    fn err(&mut self, e: Self::Error);
}

impl<E, T, A: Fulfillable<Result = Result<T, E>>> ResultFulfillable for A {
    type Okay = T;
    type Error = E;

    fn ok(&mut self, val: T) {
        self.fulfill(Ok(val))
    }

    fn err(&mut self, e: E) {
        self.fulfill(Err(e))
    }
}

impl<Env, T> Promise<Env, T> {
    pub fn on_fulfill<F: Fn(&mut Env, &T) + 'static>(&mut self, f: F) {
        let mut waiters = self.waiters.write().unwrap();
        waiters.push(Box::new(f));
    }
}

impl<Env, T> Promise<Env, Result<T, Cancelled>> {
    pub fn on_cancel<F: Fn(&mut Env) + 'static>(&mut self, f: F) {
        self.on_err(move |env, _| f(env))
    }
}

impl<Env, E, T> Promise<Env, Result<T, E>> {
    pub fn on_ok<F: Fn(&mut Env, &T) + 'static>(&mut self, f: F) {
        self.on_fulfill(move |env, r| {
            if let Ok(val) = r {
                f(env, val)
            }
        })
    }

    pub fn on_err<F: Fn(&mut Env, &E) + 'static>(&mut self, f: F) {
        self.on_fulfill(move |env, r| {
            if let Err(e) = r {
                f(env, e)
            }
        })
    }
}

pub trait Give<Env> {
    fn give(&self, env: &mut Env) -> bool;
}

impl<Env, T> Give<Env> for Promise<Env, T> {
    fn give(&self, env: &mut Env) -> bool {
        let inner = self.inner.read().unwrap();
        if let Some(value) = &inner.value {
            let mut waiters = self.waiters.write().unwrap();
            for waiter in waiters.iter() {
                waiter(env, value);
            }
            waiters.clear();
            true
        } else {
            false
        }
    }
}

impl<Env, T> Clone for Promise<Env, T> {
    fn clone(&self) -> Self {
        Promise {
            inner: self.inner.clone(),
            waiters: self.waiters.clone(),
        }
    }
}

impl<Env, P: Give<Env>> Give<Env> for &P {
    fn give(&self, env: &mut Env) -> bool {
        (*self).give(env)
    }
}

impl<Env, T> Future for Promise<Env, T> {
    type Output = Arc<T>;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut inner = self.inner.write().unwrap();
        match inner.value {
            Some(ref v) => Poll::Ready(v.clone()),
            None => {
                inner.waker = Some(cx.waker().clone());
                Poll::Pending
            }
        }
    }
}

pub struct Promises<'a, Env> {
    ps: Vec<Box<dyn Give<Env> + 'a>>,
}

impl<'a, Env> Promises<'a, Env> {
    pub fn new() -> Self {
        Promises { ps: Vec::new() }
    }

    pub fn push(&mut self, p: Box<dyn Give<Env> + 'a>) {
        self.ps.push(p);
    }

    pub fn give_all(&mut self, env: &mut Env) {
        self.ps.retain(|p| !p.give(env));
    }
}

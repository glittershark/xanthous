use std::iter::FromIterator;

pub fn times<A: Copy, B: FromIterator<A>>(elem: A, n: u16) -> B {
    (0..n).map(|_| elem).collect()
}

pub fn clone_times<A: Clone, B: FromIterator<A>>(elem: A, n: u16) -> B {
    (0..n).map(|_| elem.clone()).collect()
}

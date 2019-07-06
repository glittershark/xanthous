use proptest_derive::Arbitrary;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
pub enum Direction {
    Left,
    Up,
    Down,
    Right,
}

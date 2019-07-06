use std::cmp::Ordering;
use std::ops;
pub mod command;
pub mod direction;
pub use direction::Direction;
pub use direction::Direction::{Down, Left, Right, Up};
use proptest_derive::Arbitrary;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
pub struct Dimensions {
    #[proptest(strategy = "std::ops::Range::<u16>::from(0..100)")]
    pub w: u16,

    #[proptest(strategy = "std::ops::Range::<u16>::from(0..100)")]
    pub h: u16,
}

pub const ZERO_DIMENSIONS: Dimensions = Dimensions { w: 0, h: 0 };
pub const UNIT_DIMENSIONS: Dimensions = Dimensions { w: 1, h: 1 };

impl ops::Sub<Dimensions> for Dimensions {
    type Output = Dimensions;
    fn sub(self, dims: Dimensions) -> Dimensions {
        Dimensions {
            w: self.w - dims.w,
            h: self.h - dims.h,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
pub struct BoundingBox {
    pub dimensions: Dimensions,
    pub position: Position,
}

impl BoundingBox {
    pub fn at_origin(dimensions: Dimensions) -> BoundingBox {
        BoundingBox {
            dimensions,
            position: ORIGIN,
        }
    }

    pub fn lr_corner(self) -> Position {
        self.position
            + (Position {
                x: self.dimensions.w,
                y: self.dimensions.h,
            })
    }

    /// Returns a bounding box representing the *inside* of this box if it was
    /// drawn on the screen.
    pub fn inner(self) -> BoundingBox {
        self + UNIT_POSITION - UNIT_DIMENSIONS - UNIT_DIMENSIONS
    }
}

impl ops::Add<Position> for BoundingBox {
    type Output = BoundingBox;
    fn add(self, pos: Position) -> BoundingBox {
        BoundingBox {
            position: self.position + pos,
            ..self
        }
    }
}

impl ops::Sub<Dimensions> for BoundingBox {
    type Output = BoundingBox;
    fn sub(self, dims: Dimensions) -> BoundingBox {
        BoundingBox {
            dimensions: self.dimensions - dims,
            ..self
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
pub struct Position {
    /// x (horizontal) position
    #[proptest(strategy = "std::ops::Range::<u16>::from(0..100)")]
    pub x: u16,

    #[proptest(strategy = "std::ops::Range::<u16>::from(0..100)")]
    /// y (vertical) position
    pub y: u16,
}

pub const ORIGIN: Position = Position { x: 0, y: 0 };
pub const UNIT_POSITION: Position = Position { x: 1, y: 1 };

impl Position {
    /// Returns true if this position exists within the bounds of the given box,
    /// inclusive
    pub fn within(self, b: BoundingBox) -> bool {
        (self > b.position - UNIT_POSITION) && self < (b.lr_corner())
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Position) -> Option<Ordering> {
        if self.x == other.x && self.y == other.y {
            Some(Ordering::Equal)
        } else if self.x > other.x && self.y > other.y {
            Some(Ordering::Greater)
        } else if self.x < other.x && self.y < other.y {
            Some(Ordering::Less)
        } else {
            None
        }
    }
}

/// Implements (bounded) addition of a Dimension to a position.
///
/// # Examples
///
/// ```
/// let pos = Position { x: 1, y: 10 }
///
/// let left_pos = pos + Direction::Left
/// assert_eq!(left, Position { x: 0, y: 10 })
///
/// let right_pos = pos + Direction::Right
/// assert_eq!(right_pos, Position { x: 0, y: 10 })
/// ```
impl ops::Add<Direction> for Position {
    type Output = Position;
    fn add(self, dir: Direction) -> Position {
        match dir {
            Left => {
                if self.x > 0 {
                    Position {
                        x: self.x - 1,
                        ..self
                    }
                } else {
                    self
                }
            }
            Right => {
                if self.x < std::u16::MAX {
                    Position {
                        x: self.x + 1,
                        ..self
                    }
                } else {
                    self
                }
            }
            Up => {
                if self.y > 0 {
                    Position {
                        y: self.y - 1,
                        ..self
                    }
                } else {
                    self
                }
            }
            Down => {
                if self.y < std::u16::MAX {
                    Position {
                        y: self.y + 1,
                        ..self
                    }
                } else {
                    self
                }
            }
        }
    }
}

impl ops::Add<Position> for Position {
    type Output = Position;
    fn add(self, pos: Position) -> Position {
        Position {
            x: self.x + pos.x,
            y: self.y + pos.y,
        }
    }
}

impl ops::Sub<Position> for Position {
    type Output = Position;
    fn sub(self, pos: Position) -> Position {
        Position {
            x: self.x - pos.x,
            y: self.y - pos.y,
        }
    }
}

pub trait Positioned {
    fn x(&self) -> u16 {
        self.position().x
    }

    fn y(&self) -> u16 {
        self.position().y
    }

    fn position(&self) -> Position {
        Position {
            x: self.x(),
            y: self.y(),
        }
    }
}

macro_rules! positioned {
    ($name:ident) => {
        positioned!($name, position);
    };
    ($name:ident, $attr:ident) => {
        impl crate::types::Positioned for $name {
            fn position(&self) -> Position {
                self.$attr
            }
        }
    };
}

/// A number of ticks
#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
pub struct Ticks(pub u16);

/// A number of tiles
///
/// Expressed in terms of a float to allow moving partial tiles in a number of
/// ticks
#[derive(Clone, Copy, Debug, PartialEq, Arbitrary)]
pub struct Tiles(pub f32);

/// The speed of an entity, expressed in ticks per tile
#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
pub struct Speed(pub u32);

impl Speed {
    pub fn ticks_to_tiles(self, ticks: Ticks) -> Tiles {
        Tiles(ticks.0 as f32 / self.0 as f32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn position_partialord_lt_transitive(
            a: Position,
            b: Position,
            c: Position
        ) {
            if a < b && b < c {
                assert!(a < c)
            }
        }

        #[test]
        fn position_partialord_eq_transitive(
            a: Position,
            b: Position,
            c: Position
        ) {
            if a == b && b == c {
                assert!(a == c)
            }
        }

        #[test]
        fn position_partialord_gt_transitive(
            a: Position,
            b: Position,
            c: Position,
        ) {
            if a > b && b > c {
                assert!(a > c)
            }
        }

        #[test]
        fn position_partialord_antisymmetric(a: Position, b: Position) {
            if a < b {
                assert!(!(a > b))
            } else if a > b {
                assert!(!(a < b))
            }
        }
    }
}

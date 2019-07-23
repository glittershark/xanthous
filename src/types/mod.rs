use std::cmp::max;
use std::cmp::Ordering;
use std::ops;
use std::rc::Rc;
pub mod collision;
pub mod command;
pub mod direction;
pub mod entity_map;
pub use collision::Collision;
pub use direction::Direction;
pub use direction::Direction::*;
use proptest_derive::Arbitrary;
use termion::cursor;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
pub struct Dimensions {
    #[proptest(strategy = "std::ops::Range::<u16>::from(0..100)")]
    pub w: u16,

    #[proptest(strategy = "std::ops::Range::<u16>::from(0..100)")]
    pub h: u16,
}

pub const ZERO_DIMENSIONS: Dimensions = Dimensions { w: 0, h: 0 };
pub const UNIT_DIMENSIONS: Dimensions = Dimensions { w: 1, h: 1 };

impl Default for Dimensions {
    fn default() -> Self {
        Dimensions { w: 80, h: 20 }
    }
}

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

    pub fn from_corners(
        top_left: Position,
        lower_right: Position,
    ) -> BoundingBox {
        BoundingBox {
            position: top_left,
            dimensions: Dimensions {
                w: (lower_right.x - top_left.x) as u16,
                h: (lower_right.y - top_left.y) as u16,
            },
        }
    }

    pub fn lr_corner(self) -> Position {
        self.position
            + (Position {
                x: self.dimensions.w as i16,
                y: self.dimensions.h as i16,
            })
    }

    /// Returns a bounding box representing the *inside* of this box if it was
    /// drawn on the screen.
    pub fn inner(self) -> BoundingBox {
        self + UNIT_POSITION - UNIT_DIMENSIONS - UNIT_DIMENSIONS
    }

    /// Moves the top right corner of the bounding box by the offset specified
    /// by the given position, keeping the lower right corner in place
    pub fn move_tr_corner(self, offset: Position) -> BoundingBox {
        self + offset
            - Dimensions {
                w: offset.x as u16,
                h: offset.y as u16,
            }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary, Hash, Ord)]
pub struct Position {
    /// x (horizontal) position
    #[proptest(strategy = "std::ops::Range::<i16>::from(0..100)")]
    pub x: i16,

    #[proptest(strategy = "std::ops::Range::<i16>::from(0..100)")]
    /// y (vertical) position
    pub y: i16,
}

pub fn pos(x: i16, y: i16) -> Position {
    Position { x, y }
}

pub const ORIGIN: Position = Position { x: 0, y: 0 };
pub const UNIT_POSITION: Position = Position { x: 1, y: 1 };

impl Position {
    /// Returns true if this position exists within the bounds of the given box,
    /// inclusive
    pub fn within(self, b: BoundingBox) -> bool {
        (self > b.position - UNIT_POSITION) && self < (b.lr_corner())
    }

    /// Returns a sequence of ASCII escape characters for moving the cursor to
    /// this Position
    pub fn cursor_goto(&self) -> cursor::Goto {
        // + 1 because Goto is 1-based, but position is 0-based
        cursor::Goto(self.x as u16 + 1, self.y as u16 + 1)
    }

    /// Converts this position to the number of `Tiles` away from the origin it
    /// represents. Usually done after subtracting two positions. Gives distance
    /// as the crow flies
    pub fn as_tiles(&self) -> Tiles {
        Tiles(max(self.x.abs(), self.y.abs()).into())
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
                if self.x > std::i16::MIN {
                    Position {
                        x: self.x - 1,
                        ..self
                    }
                } else {
                    self
                }
            }
            Right => {
                if self.x < std::i16::MAX {
                    Position {
                        x: self.x + 1,
                        ..self
                    }
                } else {
                    self
                }
            }
            Up => {
                if self.y > std::i16::MIN {
                    Position {
                        y: self.y - 1,
                        ..self
                    }
                } else {
                    self
                }
            }
            Down => {
                if self.y < std::i16::MAX {
                    Position {
                        y: self.y + 1,
                        ..self
                    }
                } else {
                    self
                }
            }
            UpLeft => self + Up + Left,
            UpRight => self + Up + Right,
            DownLeft => self + Down + Left,
            DownRight => self + Down + Right,
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

impl Positioned for Position {
    fn position(&self) -> Position {
        *self
    }
}

pub trait Positioned {
    fn x(&self) -> i16 {
        self.position().x
    }

    fn y(&self) -> i16 {
        self.position().y
    }

    fn position(&self) -> Position {
        Position {
            x: self.x(),
            y: self.y(),
        }
    }
}

pub trait PositionedMut: Positioned {
    fn set_position(&mut self, pos: Position);
}

// impl<A, I> Positioned for A where A : Deref<Target = I>, I: Positioned {
//     fn position(&self) -> Position {
//         self.position()
//     }
// }

impl<T: Positioned> Positioned for Box<T> {
    fn position(&self) -> Position {
        (**self).position()
    }
}

impl<'a, T: Positioned> Positioned for &'a T {
    fn position(&self) -> Position {
        (**self).position()
    }
}

impl<'a, T: Positioned> Positioned for &'a mut T {
    fn position(&self) -> Position {
        (**self).position()
    }
}

impl<'a, T: Positioned> Positioned for Rc<T> {
    fn position(&self) -> Position {
        (**self).position()
    }
}

impl<'a, T: PositionedMut> PositionedMut for &'a mut T {
    fn set_position(&mut self, pos: Position) {
        (**self).set_position(pos)
    }
}

#[macro_export]
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

#[macro_export]
macro_rules! positioned_mut {
    ($name:ident) => {
        positioned_mut!($name, position);
    };
    ($name:ident, $attr:ident) => {
        impl crate::types::PositionedMut for $name {
            fn set_position(&mut self, pos: Position) {
                self.$attr = pos;
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary, Deserialize)]
#[serde(transparent)]
pub struct Speed(pub u32);

impl Speed {
    /// Returns the number of tiles that would be moved in the given number of
    /// ticks at this speed
    pub fn ticks_to_tiles(self, ticks: Ticks) -> Tiles {
        Tiles(ticks.0 as f32 / self.0 as f32)
    }

    /// Returns the number of ticks required to move the given number of tiles
    /// at this speed
    pub fn tiles_to_ticks(self, tiles: Tiles) -> Ticks {
        Ticks(tiles.0 as u16 * self.0 as u16)
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

        #[test]
        fn test_position_plus_dimension_as_tiles_monoid_action(
            pos: Position,
            dir: Direction,
        ) {
            prop_assume!(pos.y > 0 && pos.x > 0);
            assert_eq!(((pos + dir) - pos).as_tiles(), Tiles(1.0));
        }
    }

    #[test]
    fn test_position_as_tiles() {
        assert_eq!(pos(0, 0).as_tiles(), Tiles(0.0));
        assert_eq!(pos(1, 1).as_tiles(), Tiles(1.0));
        assert_eq!(pos(1, 2).as_tiles(), Tiles(2.0));
    }
}

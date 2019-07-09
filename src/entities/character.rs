use crate::display;
use crate::entities::Entity;
use crate::types::{Position, Speed};
use proptest_derive::Arbitrary;
use std::io::{self, Write};
use termion::cursor;

const DEFAULT_SPEED: Speed = Speed(100);

#[derive(Debug, PartialEq, Eq, Arbitrary, Clone)]
pub struct Character {
    /// The position of the character, relative to the game
    pub position: Position,
}

impl Character {
    pub fn new() -> Character {
        Character {
            position: Position { x: 0, y: 0 },
        }
    }

    pub fn speed(&self) -> Speed {
        Speed(100)
    }
}

positioned!(Character);
positioned_mut!(Character);

impl Entity for Character {}

impl display::Draw for Character {
    fn do_draw(&self, out: &mut Write) -> io::Result<()> {
        write!(out, "@{}", cursor::Left(1),)
    }
}

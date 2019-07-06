use proptest_derive::Arbitrary;
use std::io::{self, Write};
use termion::cursor;

use crate::display;
use crate::types::{Position, Speed};

const DEFAULT_SPEED: Speed = Speed(100);

#[derive(Debug, PartialEq, Eq, Arbitrary)]
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

impl display::Draw for Character {
    fn do_draw<W: Write>(&self, out: &mut W) -> io::Result<()> {
        write!(
            out,
            "@{}",
            cursor::Left(1),
        )
    }
}

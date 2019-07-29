use crate::display;
use crate::entities::EntityID;
use crate::types::{Position, Speed};
use proptest_derive::Arbitrary;
use std::io::{self, Write};
use termion::cursor;

const DEFAULT_SPEED: Speed = Speed(100);

#[derive(Debug, PartialEq, Eq, Arbitrary, Clone)]
pub struct Character {
    pub id: Option<EntityID>,

    /// The position of the character, relative to the game
    pub position: Position,

    pub o_name: Option<String>,
}

impl Character {
    pub fn new() -> Character {
        Character {
            id: None,
            position: Position { x: 0, y: 0 },
            o_name: None,
        }
    }

    pub fn speed(&self) -> Speed {
        Speed(100)
    }

    pub fn damage(&self) -> u16 {
        // TODO
        1
    }

    pub fn name<'a>(&'a self) -> &'a str {
        self.o_name
            .as_ref()
            .expect("Character name not initialized")
    }

    pub fn set_name(&mut self, name: String) {
        self.o_name = Some(name);
    }
}

entity!(Character);

impl display::Draw for Character {
    fn do_draw(&self, out: &mut Write) -> io::Result<()> {
        write!(out, "@")
    }
}

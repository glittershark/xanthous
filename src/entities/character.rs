use crate::display;
use crate::types::{Position, Speed};
use std::io::{self, Write};

const DEFAULT_SPEED: Speed = Speed(100);

entity! {
    pub struct Character {
        pub o_name: Option<String>,
    }
}

static_description!(Character, "yourself");

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

    pub fn name(&self) -> &str {
        self.o_name
            .as_ref()
            .expect("Character name not initialized")
    }

    pub fn set_name(&mut self, name: String) {
        self.o_name = Some(name);
    }
}

impl display::Draw for Character {
    fn do_draw(&self, out: &mut Write) -> io::Result<()> {
        write!(out, "@")
    }
}

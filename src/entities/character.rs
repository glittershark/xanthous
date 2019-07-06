use crate::types::{Position, Speed};

const DEFAULT_SPEED: Speed = Speed(100);

pub struct Character {
    position: Position,
}

impl Character {
    pub fn speed(&self) -> Speed {
        Speed(100)
    }
}

positioned!(Character);

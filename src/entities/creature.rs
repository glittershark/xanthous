use crate::display;
use crate::entities::raws::CreatureType;
use crate::entities::raws::EntityRaw;
use crate::entities::{raw, Entity};
use crate::types::Position;
use std::io::{self, Write};

pub struct Creature {
    pub typ: &'static CreatureType<'static>,
    pub position: Position,
    pub hitpoints: u16,
}

impl Creature {
    pub fn new_from_raw(name: &'static str, position: Position) -> Self {
        match raw(name) {
            EntityRaw::Creature(typ) => Self::new_with_type(typ, position),
            _ => panic!("Invalid raw type for {:?}, expected Creature", name),
        }
    }

    pub fn new_with_type(
        typ: &'static CreatureType<'static>,
        position: Position,
    ) -> Self {
        Creature {
            typ,
            position,
            hitpoints: typ.max_hitpoints,
        }
    }
}

positioned!(Creature);
positioned_mut!(Creature);

impl Entity for Creature {}

impl display::Draw for Creature {
    fn do_draw(&self, out: &mut Write) -> io::Result<()> {
        write!(out, "{}", self.typ.chr)
    }
}

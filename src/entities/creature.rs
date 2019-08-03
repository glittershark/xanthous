use crate::display;
use crate::entities::raws::CreatureType;
use crate::entities::raws::EntityRaw;
use crate::entities::{raw, Describe, EntityID};
use crate::types::Position;
use std::io::{self, Write};

#[derive(Debug, Clone)]
pub struct Creature {
    pub id: Option<EntityID>,
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
            id: None,
            typ,
            position,
            hitpoints: typ.max_hitpoints,
        }
    }

    /// Damage the given creature by the given amount
    pub fn damage(&mut self, amount: u16) {
        if self.hitpoints <= amount {
            self.hitpoints = 0;
        } else {
            self.hitpoints -= amount;
        }
    }

    /// Returns true if this creature has died
    pub fn dead(&self) -> bool {
        self.hitpoints == 0
    }
}

entity!(Creature);

impl Describe for Creature {
    fn description(&self) -> String {
        self.typ.description.to_string()
    }
}

impl display::Draw for Creature {
    fn do_draw(&self, out: &mut dyn Write) -> io::Result<()> {
        write!(out, "{}", self.typ.chr)
    }
}

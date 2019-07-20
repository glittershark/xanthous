use crate::display;
use crate::entities::raws::{raw, EntityRaw, ItemType};
use crate::entities::EntityID;
use crate::types::Position;
use std::io::{self, Write};

#[derive(Debug, Clone)]
pub struct Item {
    pub id: Option<EntityID>,
    pub typ: &'static ItemType<'static>,
    pub position: Position,
}

impl Item {
    pub fn new_from_raw(name: &'static str, position: Position) -> Self {
        match raw(name) {
            EntityRaw::Item(typ) => Self::new_with_type(typ, position),
            _ => panic!("Invalid raw type for {:?}, expected Item", name),
        }
    }

    pub fn new_with_type(
        typ: &'static ItemType<'static>,
        position: Position,
    ) -> Self {
        Item {
            id: None,
            typ,
            position,
        }
    }

    pub fn is_edible(&self) -> bool {
        self.typ.is_edible()
    }
}

entity!(Item);

impl display::Draw for Item {
    fn do_draw(&self, out: &mut Write) -> io::Result<()> {
        write!(out, "{}", self.typ.chr)
    }
}

use crate::display::Draw;
use crate::entities::EntityID;
use crate::types::{Positioned, PositionedMut};
use downcast_rs::Downcast;
use std::fmt::Debug;
use std::io::{self, Write};

pub trait Identified<ID>: Debug {
    fn opt_id(&self) -> Option<ID>;
    fn set_id(&mut self, id: ID);

    fn id(&self) -> ID {
        self.opt_id()
            .expect(format!("Entity ({:?}) is not in the game", self).as_str())
    }
}

impl<'a, A, ID> Identified<ID> for &'a mut A
where
    A: Identified<ID>,
{
    fn opt_id(&self) -> Option<ID> {
        (**self).opt_id()
    }
    fn set_id(&mut self, id: ID) {
        (**self).set_id(id);
    }
}

impl<ID, A: Identified<ID>> Identified<ID> for Box<A> {
    fn opt_id(&self) -> Option<ID> {
        (**self).opt_id()
    }
    fn set_id(&mut self, id: ID) {
        (**self).set_id(id);
    }
}

pub trait Entity:
    Positioned + PositionedMut + Identified<EntityID> + Draw + Downcast
{
}

impl Identified<EntityID> for Box<dyn Entity> {
    fn opt_id(&self) -> Option<EntityID> {
        (**self).opt_id()
    }
    fn set_id(&mut self, id: EntityID) {
        (**self).set_id(id);
    }
}

#[macro_export]
macro_rules! identified {
    ($name: ident, $typ: ident) => {
        identified!($name, $typ, id);
    };
    ($name: ident, $typ: ident, $attr: ident) => {
        impl crate::entities::entity::Identified<$typ> for $name {
            fn opt_id(&self) -> Option<$typ> {
                self.$attr
            }

            fn set_id(&mut self, id: $typ) {
                self.$attr = Some(id)
            }
        }
    };
}

#[macro_export]
macro_rules! entity {
    ($name: ident) => {
        positioned!($name);
        positioned_mut!($name);
        identified!($name, EntityID);
        impl crate::entities::entity::Entity for $name {}
    };
}

impl_downcast!(Entity);

impl Draw for Box<dyn Entity> {
    fn do_draw(&self, out: &mut Write) -> io::Result<()> {
        (**self).do_draw(out)
    }
}

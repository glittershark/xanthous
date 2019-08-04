use crate::display::DrawWithNeighbors;
use crate::entities::EntityID;
use crate::types::Neighbors;
use crate::types::Position;
use crate::types::{Positioned, PositionedMut};
use downcast_rs::Downcast;
use std::fmt::{self, Debug, Display};
use std::io::{self, Write};
use std::ops::Deref;
use std::rc::Rc;

pub trait Identified<ID>: Debug {
    fn opt_id(&self) -> Option<ID>;
    fn set_id(&mut self, id: ID);

    fn id(&self) -> ID {
        self.opt_id()
            .unwrap_or_else(|| panic!("Entity ({:?}) is not in the game", self))
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

pub trait Describe {
    fn description(&self) -> String;
}

ref_impl! {
    impl<T: Describe> Describe for &T {
        fn description(&self) -> String {
            (**self).description()
        }
    }
}

#[macro_export]
macro_rules! static_description {
    ($name: ident, $description: expr) => {
        impl $crate::entities::entity::Describe for $name {
            fn description(&self) -> String {
                $description.to_string()
            }
        }
    };
}

pub trait Entity:
    Positioned
    + PositionedMut
    + Identified<EntityID>
    + DrawWithNeighbors
    + Downcast
    + Describe
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
    ($name: ident, $typ: path) => {
        identified!($name, $typ, id);
    };
    ($name: ident, $typ: path, $attr: ident) => {
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

impl_downcast!(Entity);

impl DrawWithNeighbors for Box<dyn Entity> {
    fn do_draw_with_neighbors<'a, 'b>(
        &'a self,
        out: &'b mut dyn Write,
        neighbors: &'a Neighbors<Vec<Rc<Box<dyn Entity>>>>,
    ) -> io::Result<()> {
        (**self).do_draw_with_neighbors(out, neighbors)
    }
}

pub type AnEntity = Box<dyn Entity>;

impl Positioned for AnEntity {
    fn position(&self) -> Position {
        (**self).position()
    }
}

impl PositionedMut for AnEntity {
    fn set_position(&mut self, pos: Position) {
        (**self).set_position(pos)
    }
}

/// A menu option for an entity.
///
/// Customizes the Display impl to access the entity's Description
pub struct MenuOption<T>(pub T);

impl<T> MenuOption<T> {
    pub fn new(val: T) -> Self {
        MenuOption(val)
    }
}

impl<T> Deref for MenuOption<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Describe> Display for MenuOption<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.description())
    }
}

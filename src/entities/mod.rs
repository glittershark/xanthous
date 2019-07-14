pub mod character;
pub mod creature;
pub mod entity_char;
pub mod raws;

pub use character::Character;
pub use creature::Creature;
pub use entity_char::EntityChar;
pub use raws::raw;

use crate::display::Draw;
use crate::types::{Positioned, PositionedMut};
use downcast_rs::Downcast;
use std::io::{self, Write};

pub trait Entity: Positioned + PositionedMut + Draw + Downcast {}

impl_downcast!(Entity);

impl Draw for Box<dyn Entity> {
    fn do_draw(&self, out: &mut Write) -> io::Result<()> {
        (**self).do_draw(out)
    }
}

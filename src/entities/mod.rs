pub mod character;
use crate::display::Draw;
use crate::types::{Positioned, PositionedMut};
pub use character::Character;
use downcast_rs::Downcast;
use std::io::{self, Write};

pub trait Entity: Positioned + PositionedMut + Draw + Downcast {}

impl_downcast!(Entity);

impl Draw for Box<dyn Entity> {
    fn do_draw(&self, out: &mut Write) -> io::Result<()> {
        (**self).do_draw(out)
    }
}

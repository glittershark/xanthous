pub mod draw_box;
pub mod utils;
pub mod viewport;
use crate::types::Positioned;
pub use draw_box::{make_box, BoxStyle};
use std::io::{self, Write};
use termion::{clear, cursor, style};
pub use viewport::Viewport;

pub fn clear<T: Write>(out: &mut T) -> io::Result<()> {
    write!(out, "{}{}{}", clear::All, style::Reset, cursor::Goto(1, 1))
}

pub trait Draw: Positioned {
    /// Draw this entity, assuming the character is already at the correct
    /// position
    fn do_draw<W: Write>(&self, out: &mut W) -> io::Result<()>;
}

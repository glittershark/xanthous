pub mod draw_box;
pub mod utils;
pub use draw_box::{make_box, BoxStyle};
use std::io::{self, Write};
use termion::{clear, cursor, style};

pub fn clear<T: Write>(out: &mut T) -> io::Result<()> {
    write!(out, "{}{}{}", clear::All, style::Reset, cursor::Goto(1, 1))
}

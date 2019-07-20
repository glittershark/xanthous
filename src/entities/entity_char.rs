use crate::display::color::Color;
use std::fmt::{self, Display, Formatter};
use termion::color;

#[derive(Debug, Deserialize)]
pub struct EntityChar {
    #[serde(default)]
    color: Color,

    #[serde(rename = "char")]
    chr: char,
}

impl Display for EntityChar {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            color::Fg(&self.color),
            self.chr,
            color::Fg(color::Reset)
        )
    }
}

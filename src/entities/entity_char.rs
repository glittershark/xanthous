use crate::display::color::Color;
use std::fmt::{self, Display, Formatter};
use termion::color;

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub struct EntityChar {
    #[serde(default)]
    color: Color,

    #[serde(rename = "char")]
    chr: char,
}

impl Display for EntityChar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            color::Fg(&self.color),
            self.chr,
            color::Fg(color::Reset)
        )
    }
}

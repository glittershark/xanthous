use serde::de::{self, Unexpected, Visitor};
use std::fmt;
use std::marker::PhantomData;
use termion::color;

#[derive(Debug)]
pub struct Color(Box<dyn color::Color>);

unsafe impl Sync for Color {}
unsafe impl Send for Color {}

impl Color {
    pub fn new<C: color::Color + 'static>(c: C) -> Self {
        Color(Box::new(c))
    }
}

impl PartialEq for Color {
    fn eq(&self, other: &Self) -> bool {
        format!("{}{}", color::Fg(self), color::Bg(self))
            == format!("{}{}", color::Fg(other), color::Bg(other))
    }
}

impl Eq for Color {}

impl color::Color for Color {
    fn write_fg(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.write_fg(f)
    }

    fn write_bg(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.write_bg(f)
    }
}

impl<'a> color::Color for &'a Color {
    fn write_fg(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.write_fg(f)
    }

    fn write_bg(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.write_bg(f)
    }
}

impl Default for Color {
    fn default() -> Self {
        Color::new(color::Reset)
    }
}

pub struct ColorVisitor {
    marker: PhantomData<fn() -> Color>,
}

impl ColorVisitor {
    fn new() -> Self {
        ColorVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de> Visitor<'de> for ColorVisitor {
    type Value = Color;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("A color")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match v.to_lowercase().as_ref() {
            "black" => Ok(Color(Box::new(color::Black))),
            "blue" => Ok(Color(Box::new(color::Blue))),
            "cyan" => Ok(Color(Box::new(color::Cyan))),
            "green" => Ok(Color(Box::new(color::Green))),
            "light black" | "light_black" => {
                Ok(Color(Box::new(color::LightBlack)))
            }
            "light blue" | "light_blue" => {
                Ok(Color(Box::new(color::LightBlue)))
            }
            "light cyan" | "light_cyan" => {
                Ok(Color(Box::new(color::LightCyan)))
            }
            "light green" | "light_green" => {
                Ok(Color(Box::new(color::LightGreen)))
            }
            "light magenta" | "light_magenta" => {
                Ok(Color(Box::new(color::LightMagenta)))
            }
            "light red" | "light_red" => Ok(Color(Box::new(color::LightRed))),
            "light white" | "light_white" => {
                Ok(Color(Box::new(color::LightWhite)))
            }
            "light yellow" | "light_yellow" => {
                Ok(Color(Box::new(color::LightYellow)))
            }
            "magenta" => Ok(Color(Box::new(color::Magenta))),
            "red" => Ok(Color(Box::new(color::Red))),
            "white" => Ok(Color(Box::new(color::White))),
            "yellow" => Ok(Color(Box::new(color::Yellow))),
            _ => Err(de::Error::invalid_value(
                Unexpected::Str(v),
                &"a valid color",
            )),
        }
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        let mut red = None;
        let mut green = None;
        let mut blue = None;
        while let Some((k, v)) = map.next_entry()? {
            match k {
                "red" => {
                    red = Some(v);
                }
                "green" => {
                    green = Some(v);
                }
                "blue" => {
                    blue = Some(v);
                }
                _ => {
                    return Err(de::Error::unknown_field(
                        k,
                        &["red", "green", "blue"],
                    ));
                }
            }
        }

        match (red, green, blue) {
            (Some(r), Some(g), Some(b)) => {
                Ok(Color(Box::new(color::Rgb(r, g, b))))
            }
            (None, _, _) => Err(de::Error::missing_field("red")),
            (_, None, _) => Err(de::Error::missing_field("green")),
            (_, _, None) => Err(de::Error::missing_field("blue")),
        }
    }

    fn visit_u8<E: de::Error>(self, v: u8) -> Result<Self::Value, E> {
        Ok(Color(Box::new(color::AnsiValue(v))))
    }
}

impl<'de> serde::Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(ColorVisitor::new())
    }
}

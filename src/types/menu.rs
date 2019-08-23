use crate::types::Dimensions;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Menu<T> {
    pub prompt: String,
    pub options: Vec<T>,
}

impl<T> Menu<T> {
    pub fn new(prompt: String, options: Vec<T>) -> Self {
        Menu { prompt, options }
    }
}

impl<T: Display> Menu<T> {
    /// Returns the inner dimensions of a box necessary to draw this menu. Will
    /// not trim either dimension to the size of the terminal
    pub fn dimensions(&self) -> Dimensions {
        Dimensions {
            w: self
                .options
                .iter()
                .map(|s| format!("{}", s).len())
                .max()
                .unwrap_or(0)
                .max(self.prompt.len()) as u16
                + 4,
            h: self.options.len() as u16
                + if self.prompt.is_empty() { 0 } else { 2 }
                + 4,
        }
    }
}

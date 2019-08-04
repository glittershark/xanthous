use crate::types::Dimensions;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MenuInfo {
    pub prompt: String,
    pub options: Vec<String>,
}

impl MenuInfo {
    pub fn new(prompt: String, options: Vec<String>) -> Self {
        MenuInfo { prompt, options }
    }

    /// Returns the inner dimensions of a box necessary to draw this menu. Will
    /// not trim either dimension to the size of the terminal
    pub fn dimensions(&self) -> Dimensions {
        Dimensions {
            w: self
                .options
                .iter()
                .map(|s| s.len())
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

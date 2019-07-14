#[macro_use]
pub mod entity;
pub mod character;
pub mod creature;
pub mod entity_char;
pub mod raws;

pub use character::Character;
pub use creature::Creature;
pub use entity::{Entity, Identified};
pub use entity_char::EntityChar;
pub use raws::raw;

pub type EntityID = u32;

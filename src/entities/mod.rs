#[macro_use]
pub mod entity;
#[macro_use]
pub mod util;
pub mod character;
pub mod creature;
pub mod entity_char;
pub mod environment;
pub mod item;
pub mod raw_types;
pub mod raws;

pub use character::Character;
pub use creature::Creature;
pub use entity::{AnEntity, Describe, Entity, Identified};
pub use entity_char::EntityChar;
pub use item::Item;
pub use raws::raw;

pub type EntityID = u32;

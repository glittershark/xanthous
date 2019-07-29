/// Describes a kind of game collision
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Collision {
    /// Stop moving - you can't move there!
    Stop,

    /// Moving into an entity at the given position indicates combat
    Combat,
}

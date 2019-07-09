/// Describes a kind of game collision
pub enum Collision {
    /// Stop moving - you can't move there!
    Stop,

    /// Moving into an entity at the given position indicates combat
    Combat,
}

use super::Direction;
use super::Direction::*;
use termion::event::Key;
use termion::event::Key::{Char, Ctrl};

pub enum Command {
    /// Quit the game
    Quit,

    /// Move the character in a direction
    Move(Direction),

    /// Pick up any item(s) at the current position
    PickUp,

    /// Display the previous message
    PreviousMessage,
}

impl Command {
    pub fn from_key(k: Key) -> Option<Command> {
        use Command::*;
        match k {
            Char('q') => Some(Quit),

            Char('h') | Char('a') | Key::Left => Some(Move(Left)),
            Char('k') | Char('w') | Key::Up => Some(Move(Up)),
            Char('j') | Char('s') | Key::Down => Some(Move(Down)),
            Char('l') | Char('d') | Key::Right => Some(Move(Right)),
            Char('y') => Some(Move(UpLeft)),
            Char('u') => Some(Move(UpRight)),
            Char('b') => Some(Move(DownLeft)),
            Char('n') => Some(Move(DownRight)),

            Ctrl('p') => Some(PreviousMessage),
            Char(',') => Some(PickUp),

            _ => None,
        }
    }
}

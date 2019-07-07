use super::Direction;
use super::Direction::*;
use termion::event::Key;
use termion::event::Key::{Char, Ctrl};

pub enum Command {
    /// Quit the game
    Quit,

    /// Move the character in a direction
    Move(Direction),

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
            Ctrl('p') => Some(PreviousMessage),
            _ => None,
        }
    }
}

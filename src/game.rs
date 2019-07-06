use std::thread;
use crate::settings::Settings;
use crate::types::{BoundingBox, Dimensions, Position};
use std::io::{self, StdinLock, StdoutLock, Write};
use termion::cursor;
use termion::input::Keys;
use termion::input::TermRead;
use termion::raw::RawTerminal;

use crate::display;
use crate::types::command::Command;

/// The full state of a running Game
pub struct Game<'a> {
    settings: Settings,

    /// The box describing the viewport. Generally the size of the terminal, and
    /// positioned at 0, 0
    viewport: BoundingBox,

    /// An iterator on keypresses from the user
    keys: Keys<StdinLock<'a>>,

    stdout: RawTerminal<StdoutLock<'a>>,

    /// The position of the character
    character: Position,
}

impl<'a> Game<'a> {
    pub fn new(
        settings: Settings,
        stdout: RawTerminal<StdoutLock<'a>>,
        stdin: StdinLock<'a>,
        w: u16,
        h: u16,
    ) -> Game<'a> {
        Game {
            settings: settings,
            viewport: BoundingBox::at_origin(Dimensions { w, h }),
            keys: stdin.keys(),
            stdout: stdout,
            character: Position { x: 1, y: 1 },
        }
    }

    /// Returns true if there's a collision in the game at the given Position
    fn collision_at(&self, pos: Position) -> bool {
        !pos.within(self.viewport.inner())
    }

    /// Run the game
    pub fn run(mut self) {
        info!("Running game");
        write!(
            self,
            "{}{}@{}",
            display::make_box(
                display::BoxStyle::Thin,
                self.viewport.dimensions
            ),
            cursor::Goto(2, 2),
            cursor::Left(1),
        )
        .unwrap();
        self.flush().unwrap();
        loop {
            let mut character_moved = false;
            match Command::from_key(self.keys.next().unwrap().unwrap()) {
                Some(Command::Quit) => {
                    info!("Quitting game due to user request");
                    break;
                }

                Some(Command::Move(direction)) => {
                    let new_pos = self.character + direction;
                    if !self.collision_at(new_pos) {
                        self.character = new_pos;
                        character_moved = true;
                    }
                }
                _ => (),
            }

            if character_moved {
                debug!("char: {:?}", self.character);
                write!(
                    self,
                    " {}@{}",
                    cursor::Goto(self.character.x + 1, self.character.y + 1,),
                    cursor::Left(1)
                )
                .unwrap();
            }
            self.flush().unwrap();
        }
    }
}

impl<'a> Drop for Game<'a> {
    fn drop(&mut self) {
        display::clear(self).unwrap();
    }
}

impl<'a> Write for Game<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stdout.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.stdout.write_all(buf)
    }
}

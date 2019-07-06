use crate::settings::Settings;
use crate::types::Positioned;
use crate::types::{BoundingBox, Dimensions, Position};
use std::io::{self, StdinLock, StdoutLock, Write};
use termion::input::Keys;
use termion::input::TermRead;
use termion::raw::RawTerminal;

use crate::display::{self, Viewport};
use crate::entities::Character;
use crate::types::command::Command;

type Stdout<'a> = RawTerminal<StdoutLock<'a>>;

/// The full state of a running Game
pub struct Game<'a> {
    settings: Settings,

    viewport: Viewport<Stdout<'a>>,

    /// An iterator on keypresses from the user
    keys: Keys<StdinLock<'a>>,

    /// The player character
    character: Character,
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
            viewport: Viewport {
                outer: BoundingBox::at_origin(Dimensions { w, h }),
                inner: BoundingBox::at_origin(Dimensions {
                    w: w - 2,
                    h: h - 2,
                }),
                out: stdout,
            },
            keys: stdin.keys(),
            character: Character::new(),
        }
    }

    /// Returns true if there's a collision in the game at the given Position
    fn collision_at(&self, pos: Position) -> bool {
        !pos.within(self.viewport.inner)
    }

    fn draw_entities(&mut self) -> io::Result<()> {
        self.viewport.draw(&self.character)
    }

    /// Run the game
    pub fn run(mut self) -> io::Result<()> {
        info!("Running game");
        self.viewport.init()?;
        self.draw_entities()?;
        self.flush()?;
        loop {
            let mut old_position = None;
            match Command::from_key(self.keys.next().unwrap().unwrap()) {
                Some(Command::Quit) => {
                    info!("Quitting game due to user request");
                    break;
                }

                Some(Command::Move(direction)) => {
                    let new_pos = self.character.position + direction;
                    if !self.collision_at(new_pos) {
                        old_position = Some(self.character.position);
                        self.character.position = new_pos;
                    }
                }
                _ => (),
            }

            match old_position {
                Some(old_pos) => {
                    self.viewport.clear(old_pos)?;
                    self.viewport.draw(&self.character)?;
                }
                None => ()
            }
            self.flush()?;
            debug!("{:?}", self.character);
        }
        Ok(())
    }
}

impl<'a> Drop for Game<'a> {
    fn drop(&mut self) {
        display::clear(self).unwrap_or(());
    }
}

impl<'a> Write for Game<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.viewport.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.viewport.flush()
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.viewport.write_all(buf)
    }
}

impl<'a> Positioned for Game<'a> {
    fn position(&self) -> Position {
        Position { x: 0, y: 0 }
    }
}

use crate::display::{self, Viewport};
use crate::entities::Character;
use crate::messages::message;
use crate::settings::Settings;
use crate::types::command::Command;
use crate::types::Positioned;
use crate::types::{BoundingBox, Dimensions, Position};
use rand::rngs::SmallRng;
use rand::SeedableRng;
use std::io::{self, StdinLock, StdoutLock, Write};
use termion::input::Keys;
use termion::input::TermRead;
use termion::raw::RawTerminal;

type Stdout<'a> = RawTerminal<StdoutLock<'a>>;

type Rng = SmallRng;

/// The full state of a running Game
pub struct Game<'a> {
    settings: Settings,

    viewport: Viewport<Stdout<'a>>,

    /// An iterator on keypresses from the user
    keys: Keys<StdinLock<'a>>,

    /// The player character
    character: Character,

    /// The messages that have been said to the user, in forward time order
    messages: Vec<String>,

    /// A global random number generator for the game
    rng: Rng,
}

impl<'a> Game<'a> {
    pub fn new(
        settings: Settings,
        stdout: RawTerminal<StdoutLock<'a>>,
        stdin: StdinLock<'a>,
        w: u16,
        h: u16,
    ) -> Game<'a> {
        let rng = match settings.seed {
            Some(seed) => SmallRng::seed_from_u64(seed),
            None => SmallRng::from_entropy(),
        };
        Game {
            settings,
            rng,
            viewport: Viewport::new(
                BoundingBox::at_origin(Dimensions { w, h }),
                BoundingBox::at_origin(Dimensions { w: w - 2, h: h - 2 }),
                stdout,
            ),
            keys: stdin.keys(),
            character: Character::new(),
            messages: Vec::new(),
        }
    }

    /// Returns true if there's a collision in the game at the given Position
    fn collision_at(&self, pos: Position) -> bool {
        !pos.within(self.viewport.inner)
    }

    /// Draw all the game entities to the screen
    fn draw_entities(&mut self) -> io::Result<()> {
        self.viewport.draw(&self.character)
    }

    /// Get a message from the global map based on the rng in this game
    fn message(&mut self, name: &str) -> &'static str {
        message(name, &mut self.rng)
    }

    /// Say a message to the user
    fn say(&mut self, message_name: &str) -> io::Result<()> {
        let message = self.message(message_name);
        self.messages.push(message.to_string());
        self.viewport.write_message(message)
    }

    /// Run the game
    pub fn run(mut self) -> io::Result<()> {
        info!("Running game");
        self.viewport.init()?;
        self.draw_entities()?;
        self.say("global.welcome")?;
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
                None => (),
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

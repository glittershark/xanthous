use crate::display::{self, Viewport};
use crate::entities::{Character, Creature, Entity, EntityID, Identified};
use crate::messages::message;
use crate::settings::Settings;
use crate::types::command::Command;
use crate::types::entity_map::EntityMap;
use crate::types::{
    pos, BoundingBox, Collision, Dimensions, Position, Positioned,
    PositionedMut, Ticks,
};
use rand::rngs::SmallRng;
use rand::SeedableRng;
use std::io::{self, StdinLock, StdoutLock, Write};
use termion::input::Keys;
use termion::input::TermRead;
use termion::raw::RawTerminal;

type Stdout<'a> = RawTerminal<StdoutLock<'a>>;

type Rng = SmallRng;

type AnEntity<'a> = Box<dyn Entity>;

impl<'a> Positioned for AnEntity<'a> {
    fn position(&self) -> Position {
        (**self).position()
    }
}

impl<'a> PositionedMut for AnEntity<'a> {
    fn set_position(&mut self, pos: Position) {
        (**self).set_position(pos)
    }
}

/// The full state of a running Game
pub struct Game<'a> {
    settings: Settings,

    viewport: Viewport<Stdout<'a>>,

    /// An iterator on keypresses from the user
    keys: Keys<StdinLock<'a>>,

    /// The map of all the entities in the game
    entities: EntityMap<AnEntity<'a>>,

    /// The entity ID of the player character
    character_entity_id: EntityID,

    /// The messages that have been said to the user, in forward time order
    messages: Vec<String>,

    /// The index of the currently-displayed message. Used to track the index of
    /// the currently displayed message when handling PreviousMessage commands
    message_idx: usize,

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
        let mut entities: EntityMap<AnEntity<'a>> = EntityMap::new();

        // TODO make this dynamic
        {
            entities.insert(Box::new(Creature::new_from_raw(
                "gormlak",
                pos(10, 0),
            )));
        }

        Game {
            settings,
            rng,
            message_idx: 0,
            viewport: Viewport::new(
                BoundingBox::at_origin(Dimensions { w, h }),
                BoundingBox::at_origin(Dimensions { w: w - 2, h: h - 2 }),
                stdout,
            ),
            keys: stdin.keys(),
            character_entity_id: entities.insert(Box::new(Character::new())),
            messages: Vec::new(),
            entities,
        }
    }

    /// Returns a list of all creature entities at the given position
    fn creatures_at<'b>(&'b self, pos: Position) -> Vec<&'b Creature> {
        self.entities
            .at(pos)
            .iter()
            .filter_map(|e| e.downcast_ref())
            .collect()
    }

    /// Returns a collision, if any, at the given Position in the game
    fn collision_at(&self, pos: Position) -> Option<Collision> {
        if !pos.within(self.viewport.inner) {
            Some(Collision::Stop)
        } else {
            if self.creatures_at(pos).len() > 0 {
                Some(Collision::Combat)
            } else {
                None
            }
        }
    }

    fn character(&self) -> &Character {
        (*self.entities.get(self.character_entity_id).unwrap())
            .downcast_ref()
            .unwrap()
    }

    /// Draw all the game entities to the screen
    fn draw_entities(&mut self) -> io::Result<()> {
        for entity in self.entities.entities() {
            self.viewport.draw(entity)?;
        }
        Ok(())
    }

    /// Remove the given entity from the game, drawing over it if it's visible
    fn remove_entity(&mut self, entity_id: EntityID) -> io::Result<()> {
        if let Some(entity) = self.entities.remove(entity_id) {
            self.viewport.clear(entity.position())?;
        }
        Ok(())
    }

    /// Step the game forward the given number of ticks
    fn tick(&mut self, ticks: Ticks) {}

    /// Get a message from the global map based on the rng in this game
    fn message(&mut self, name: &str) -> &'static str {
        message(name, &mut self.rng)
    }

    /// Say a message to the user
    fn say(&mut self, message_name: &str) -> io::Result<()> {
        let message = self.message(message_name);
        self.messages.push(message.to_string());
        self.message_idx = self.messages.len() - 1;
        self.viewport.write_message(message)
    }

    fn previous_message(&mut self) -> io::Result<()> {
        if self.message_idx == 0 {
            return Ok(());
        }
        self.message_idx -= 1;
        let message = &self.messages[self.message_idx];
        self.viewport.write_message(message)
    }

    fn attack(&mut self, creature_id: EntityID) -> io::Result<()> {
        info!("Attacking creature {:?}", creature_id);
        self.say("combat.attack")?;
        let damage = self.character().damage();
        let creature = self
            .entities
            .get_mut(creature_id)
            .and_then(|e| e.downcast_mut::<Creature>())
            .expect(
                format!("Creature ID went away: {:?}", creature_id).as_str(),
            );
        creature.damage(damage);
        if creature.dead() {
            self.say("combat.killed")?;
            info!("Killed creature {:?}", creature_id);
            self.remove_entity(creature_id)?;
        }
        Ok(())
    }

    fn attack_at(&mut self, pos: Position) -> io::Result<()> {
        let creatures = self.creatures_at(pos);
        if creatures.len() == 1 {
            let creature = creatures.get(0).unwrap();
            self.attack(creature.id())
        } else {
            // TODO prompt with a menu of creatures to combat
            unimplemented!()
        }
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
            use Command::*;
            match Command::from_key(self.keys.next().unwrap().unwrap()) {
                Some(Quit) => {
                    info!("Quitting game due to user request");
                    break;
                }

                Some(Move(direction)) => {
                    use Collision::*;
                    let new_pos = self.character().position + direction;
                    match self.collision_at(new_pos) {
                        None => {
                            old_position = Some(self.character().position);
                            self.entities.update_position(
                                self.character_entity_id,
                                new_pos,
                            );
                        }
                        Some(Combat) => {
                            self.attack_at(new_pos)?;
                        }
                        Some(Stop) => (),
                    }
                }

                Some(PreviousMessage) => self.previous_message()?,

                None => (),
            }

            match old_position {
                Some(old_pos) => {
                    self.tick(self.character().speed().tiles_to_ticks(
                        (old_pos - self.character().position).as_tiles(),
                    ));
                    self.viewport.clear(old_pos)?;
                    self.viewport.draw(
                        // TODO this clone feels unnecessary.
                        &self.character().clone(),
                    )?;
                }
                None => (),
            }
            self.flush()?;
            debug!("{:?}", self.character());
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

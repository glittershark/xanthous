use crate::display::{self, Viewport};
use crate::entities::{
    Character, Creature, Entity, EntityID, Identified, Item,
};
use crate::messages::message;
use crate::settings::Settings;
use crate::types::command::Command;
use crate::types::entity_map::EntityMap;
use crate::types::{
    pos, BoundingBox, Collision, Dimensions, Position, Positioned,
    PositionedMut, Ticks,
};
use crate::util::promise::Cancelled;
use crate::util::promise::{promise, Complete, Promise, Promises};
use crate::util::template::TemplateParams;
use rand::rngs::SmallRng;
use rand::SeedableRng;
use std::io::{self, StdinLock, StdoutLock, Write};
use termion::input::Keys;
use termion::input::TermRead;
use termion::raw::RawTerminal;

type Stdout<'a> = RawTerminal<StdoutLock<'a>>;

type Rng = SmallRng;

type AnEntity = Box<dyn Entity>;

impl Positioned for AnEntity {
    fn position(&self) -> Position {
        (**self).position()
    }
}

impl PositionedMut for AnEntity {
    fn set_position(&mut self, pos: Position) {
        (**self).set_position(pos)
    }
}

enum PromptResolution {
    Uncancellable(Complete<String>),
    Cancellable(Complete<Result<String, Cancelled>>),
}

impl PromptResolution {
    fn is_cancellable(&self) -> bool {
        use PromptResolution::*;
        match self {
            Uncancellable(_) => false,
            Cancellable(_) => true,
        }
    }

    fn fulfill(&mut self, val: String) {
        use PromptResolution::*;
        match self {
            Cancellable(complete) => complete.ok(val),
            Uncancellable(complete) => complete.fulfill(val),
        }
    }

    fn cancel(&mut self) {
        use PromptResolution::*;
        match self {
            Cancellable(complete) => complete.cancel(),
            Uncancellable(complete) => {}
        }
    }
}

/// The kind of input the game is waiting to receive
enum InputState {
    /// The initial input state of the game - we're currently waiting for direct
    /// commands.
    Initial,

    /// A free text prompt has been shown to the user, and every character
    /// besides "escape" is interpreted as a response to that prompt
    Prompt {
        complete: PromptResolution,
        buffer: String,
    },
}

impl InputState {
    fn uncancellable_prompt(complete: Complete<String>) -> Self {
        InputState::Prompt {
            complete: PromptResolution::Uncancellable(complete),
            buffer: String::new(),
        }
    }

    fn cancellable_prompt(
        complete: Complete<Result<String, Cancelled>>,
    ) -> Self {
        InputState::Prompt {
            complete: PromptResolution::Cancellable(complete),
            buffer: String::new(),
        }
    }
}

impl Default for InputState {
    fn default() -> Self {
        InputState::Initial
    }
}

/// The full state of a running Game
pub struct Game<'a> {
    settings: Settings,

    viewport: Viewport<Stdout<'a>>,

    /// An iterator on keypresses from the user
    keys: Keys<StdinLock<'a>>,

    /// The kind of input the game is waiting to receive
    input_state: InputState,

    /// The map of all the entities in the game
    entities: EntityMap<AnEntity>,

    /// The entity ID of the player character
    character_entity_id: EntityID,

    /// The messages that have been said to the user, in forward time order
    messages: Vec<String>,

    /// The index of the currently-displayed message. Used to track the index of
    /// the currently displayed message when handling PreviousMessage commands
    message_idx: usize,

    /// A global random number generator for the game
    rng: Rng,

    /// A list of promises that are waiting on the game and a result
    promises: Promises<'a, Self>,
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
        let mut entities: EntityMap<AnEntity> = EntityMap::new();

        // TODO make this dynamic
        {
            entities.insert(Box::new(Creature::new_from_raw(
                "gormlak",
                pos(10, 0),
            )));

            entities
                .insert(Box::new(Item::new_from_raw("noodles", pos(0, 10))));
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
            input_state: Default::default(),
            character_entity_id: entities.insert(Box::new(Character::new())),
            messages: Vec::new(),
            entities,
            promises: Promises::new(),
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

    fn mut_character(&mut self) -> &mut Character {
        (*self.entities.get_mut(self.character_entity_id).unwrap())
            .downcast_mut()
            .unwrap()
    }

    /// Draw all the game entities to the screen
    fn draw_entities(&mut self) -> io::Result<()> {
        for entity in self.entities.entities() {
            self.viewport.draw(
                entity,
                &self.entities.neighbor_entities(entity.position()),
            )?;
        }
        Ok(())
    }

    /// Draw all the game entities to the screen
    fn draw_entities_at(&mut self, pos: Position) -> io::Result<()> {
        for entity in self.entities.at(pos) {
            self.viewport.draw(
                entity,
                &self.entities.neighbor_entities(entity.position()),
            )?;
        }
        Ok(())
    }

    /// Draw the game entity with the given ID, if any, to the screen
    fn draw_entity(&mut self, entity_id: EntityID) -> io::Result<bool> {
        if let Some(entity) = self.entities.get(entity_id) {
            self.viewport.draw(
                entity,
                &self.entities.neighbor_entities(entity.position()),
            )?;
            Ok(true)
        } else {
            Ok(false)
        }
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
    fn message<'params>(
        &mut self,
        name: &'static str,
        params: &TemplateParams<'params>,
    ) -> String {
        message(name, &mut self.rng, params)
    }

    /// Say a message to the user
    fn say<'params>(
        &mut self,
        message_name: &'static str,
        params: &TemplateParams<'params>,
    ) -> io::Result<()> {
        let message = self.message(message_name, params);
        self.messages.push(message.to_string());
        self.message_idx = self.messages.len() - 1;
        self.viewport.write_message(&message)?;
        Ok(())
    }

    /// Prompt the user for input, returning a Future for the result of the
    /// prompt
    fn prompt(
        &mut self,
        name: &'static str,
        params: &TemplateParams<'_>,
    ) -> io::Result<Promise<Self, String>> {
        let (complete, promise) = promise();
        self.input_state = InputState::uncancellable_prompt(complete);
        let message = self.message(name, params);
        self.viewport.write_prompt(&message)?;
        self.promises.push(Box::new(promise.clone()));
        Ok(promise)
    }

    fn prompt_cancellable(
        &mut self,
        name: &'static str,
        params: &TemplateParams<'_>,
    ) -> io::Result<Promise<Self, Result<String, Cancelled>>> {
        let (complete, promise) = promise();
        self.input_state = InputState::cancellable_prompt(complete);
        let message = self.message(name, params);
        self.viewport.write_prompt(&message)?;
        self.promises.push(Box::new(promise.clone()));
        Ok(promise)
    }

    fn previous_message(&mut self) -> io::Result<()> {
        if self.message_idx == 0 {
            return Ok(());
        }
        self.message_idx -= 1;
        let message = &self.messages[self.message_idx];
        self.viewport.write_message(message)?;
        Ok(())
    }

    fn creature(&self, creature_id: EntityID) -> Option<&Creature> {
        self.entities
            .get(creature_id)
            .and_then(|e| e.downcast_ref::<Creature>())
    }

    fn expect_creature(&self, creature_id: EntityID) -> &Creature {
        self.creature(creature_id).expect(
            format!("Creature ID went away: {:?}", creature_id).as_str(),
        )
    }

    fn mut_creature(&mut self, creature_id: EntityID) -> Option<&mut Creature> {
        self.entities
            .get_mut(creature_id)
            .and_then(|e| e.downcast_mut::<Creature>())
    }

    fn expect_mut_creature(&mut self, creature_id: EntityID) -> &mut Creature {
        self.mut_creature(creature_id).expect(
            format!("Creature ID went away: {:?}", creature_id).as_str(),
        )
    }

    fn attack(&mut self, creature_id: EntityID) -> io::Result<()> {
        info!("Attacking creature {:?}", creature_id);
        let damage = self.character().damage();
        let creature_name = self.expect_creature(creature_id).typ.name;
        let tps = template_params!({
            "creature" => {
                "name" => creature_name,
            },
        });
        self.say("combat.attack", &tps)?;

        let creature = self.expect_mut_creature(creature_id);
        creature.damage(damage);
        if creature.dead() {
            self.say("combat.killed", &tps)?;
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

    fn flush_promises(&mut self) {
        unsafe {
            let game = self as *mut Self;
            (*game).promises.give_all(&mut *game);
        }
    }

    /// Run the game
    pub fn run(mut self) -> io::Result<()> {
        info!("Running game");
        self.viewport.init()?;
        self.draw_entities()?;
        self.flush().unwrap();

        self.prompt("character.name_prompt", &template_params!())?
            .on_fulfill(|game, char_name| {
                game.say(
                    "global.welcome",
                    &template_params!({
                        "character" => {
                            "name" => char_name,
                        },
                    }),
                )
                .unwrap();
                game.flush().unwrap();
                game.mut_character().set_name(char_name.to_string());
            });

        loop {
            let mut old_position = None;
            let next_key = self.keys.next().unwrap().unwrap();
            match &mut self.input_state {
                InputState::Initial => {
                    use Command::*;
                    match Command::from_key(next_key) {
                        Some(Quit) => {
                            info!("Quitting game due to user request");
                            break;
                        }

                        Some(Move(direction)) => {
                            use Collision::*;
                            let new_pos = self.character().position + direction;
                            match self.collision_at(new_pos) {
                                None => {
                                    old_position =
                                        Some(self.character().position);
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
                            let character = self.character();
                            self.viewport.game_cursor_position =
                                character.position;
                            self.viewport.clear(old_pos)?;
                            self.draw_entities_at(old_pos)?;
                            self.draw_entity(self.character_entity_id)?;
                            self.tick(
                                self.character().speed().tiles_to_ticks(
                                    (old_pos - self.character().position)
                                        .as_tiles(),
                                ),
                            );
                        }
                        None => (),
                    }
                }

                InputState::Prompt { complete, buffer } => {
                    use termion::event::Key::*;
                    match next_key {
                        Char('\n') => {
                            info!("Prompt complete: \"{}\"", buffer);
                            self.viewport.clear_prompt()?;
                            complete.fulfill(buffer.clone());
                            self.input_state = InputState::Initial;
                        }
                        Char(chr) => {
                            buffer.push(chr);
                            self.viewport.push_prompt_chr(chr)?;
                        }
                        Esc => complete.cancel(),
                        Backspace => {
                            buffer.pop();
                            self.viewport.pop_prompt_chr()?;
                        }
                        _ => {}
                    }
                }
            }

            self.flush()?;
            self.flush_promises();
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

extern crate termion;
#[macro_use]
extern crate log;
extern crate config;
extern crate log4rs;
extern crate serde;
extern crate toml;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
#[macro_use]
extern crate clap;
#[macro_use]
extern crate prettytable;
#[macro_use]
extern crate lazy_static;
#[cfg(test)]
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate downcast_rs;
extern crate backtrace;
#[macro_use]
extern crate include_dir;
#[macro_use]
extern crate nom;
#[macro_use]
extern crate matches;
extern crate futures;

#[macro_use]
mod util;
#[macro_use]
mod types;
#[macro_use]
mod entities;
mod description;
mod display;
mod game;
mod level_gen;
mod messages;
mod settings;

use crate::types::Dimensions;
use clap::App;
use game::Game;
use prettytable::format::consts::FORMAT_BOX_CHARS;
use rand::rngs::SmallRng;
use rand::SeedableRng;
use settings::Settings;

use backtrace::Backtrace;
use std::io::{self, StdinLock, StdoutLock};
use std::panic;

use termion::raw::IntoRawMode;
use termion::raw::RawTerminal;

fn init(
    settings: Settings,
    stdout: RawTerminal<StdoutLock<'_>>,
    stdin: StdinLock<'_>,
    w: u16,
    h: u16,
) -> io::Result<()> {
    panic::set_hook(if settings.logging.print_backtrace {
        Box::new(|info| (error!("{}\n{:#?}", info, Backtrace::new())))
    } else {
        Box::new(|info| (error!("{}\n{:#?}", info, Backtrace::new())))
    });

    let game = Game::new(settings, stdout, stdin, w, h);
    game.run()
}

fn generate_level<'a, W: io::Write>(
    stdout: &mut W,
    params: &clap::ArgMatches<'a>,
) -> io::Result<()> {
    let mut rand = SmallRng::from_entropy();

    let mut dimensions: Dimensions = Default::default();
    if let Some(h_s) = params.value_of("height") {
        dimensions.h = h_s.parse().unwrap();
    }
    if let Some(w_s) = params.value_of("width") {
        dimensions.w = w_s.parse().unwrap();
    }

    let level = match params.value_of("generator") {
        None => panic!("Must supply a generator with --generator"),
        Some("cave_automata") => level_gen::cave_automata::generate(
            dimensions,
            &level_gen::cave_automata::Params::from_matches(params),
            &mut rand,
        ),
        Some(gen) => panic!("Unrecognized generator: {}", gen),
    };
    level_gen::draw_level(level, stdout)
}

fn main() -> io::Result<()> {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();
    let settings = Settings::load().unwrap();
    settings.logging.init_log();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    let stdin = io::stdin();
    let stdin = stdin.lock();

    let termsize = termion::terminal_size().ok();
    let (termwidth, termheight) = termsize.unwrap_or((70, 40));

    match matches.subcommand() {
        ("info", _) => {
            let mut table = table!(
                [br->"termwidth", termwidth],
                [br->"termheight", termheight],
                [br->"logfile", settings.logging.file],
                [br->"loglevel", settings.logging.level]
            );
            table.set_format(*FORMAT_BOX_CHARS);
            table.printstd();
            Ok(())
        }
        ("generate-level", params) => {
            generate_level(&mut stdout, params.unwrap())
        }
        _ => {
            let stdout = stdout.into_raw_mode().unwrap();
            init(settings, stdout, stdin, termwidth, termheight)
        }
    }
}

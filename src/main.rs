extern crate termion;
#[macro_use]
extern crate log;
extern crate config;
extern crate log4rs;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate clap;
#[macro_use]
extern crate prettytable;

mod display;
mod game;
#[macro_use]
mod types;
mod entities;
mod settings;

use clap::App;
use game::Game;
use prettytable::format::consts::FORMAT_BOX_CHARS;
use settings::Settings;

use std::io::{self, StdinLock, StdoutLock};

use termion::raw::IntoRawMode;
use termion::raw::RawTerminal;

fn init(
    settings: Settings,
    stdout: RawTerminal<StdoutLock<'_>>,
    stdin: StdinLock<'_>,
    w: u16,
    h: u16,
) {
    let game = Game::new(settings, stdout, stdin, w, h);
    game.run()
}

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();
    let settings = Settings::load().unwrap();
    settings.logging.init_log();
    let stdout = io::stdout();
    let stdout = stdout.lock();

    let stdin = io::stdin();
    let stdin = stdin.lock();

    let termsize = termion::terminal_size().ok();
    // let termwidth = termsize.map(|(w, _)| w - 2).unwrap_or(70);
    // let termheight = termsize.map(|(_, h)| h - 2).unwrap_or(40);
    let (termwidth, termheight) = termsize.unwrap_or((70, 40));

    match matches.subcommand() {
        ("debug", _) => {
            let mut table = table!(
                [br->"termwidth", termwidth],
                [br->"termheight", termheight],
                [br->"logfile", settings.logging.file],
                [br->"loglevel", settings.logging.level]
            );
            table.set_format(*FORMAT_BOX_CHARS);
            table.printstd();
        }
        _ => {
            let stdout = stdout.into_raw_mode().unwrap();
            init(settings, stdout, stdin, termwidth, termheight);
        }
    }
}

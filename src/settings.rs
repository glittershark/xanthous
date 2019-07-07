use config::{Config, ConfigError};
use log::LevelFilter;
use log4rs::append::file::FileAppender;
use log4rs::config::{Appender, Root};
use log4rs::encode::pattern::PatternEncoder;

#[derive(Debug, Deserialize)]
pub struct Logging {
    #[serde(default = "Logging::default_level")]
    pub level: LevelFilter,

    #[serde(default = "Logging::default_file")]
    pub file: String,
}

impl Default for Logging {
    fn default() -> Self {
        Logging {
            level: LevelFilter::Off,
            file: "debug.log".to_string(),
        }
    }
}

impl Logging {
    pub fn init_log(&self) {
        let logfile = FileAppender::builder()
            .encoder(Box::new(PatternEncoder::new("{d} {l} - {m}\n")))
            .build(self.file.clone())
            .unwrap();

        let config = log4rs::config::Config::builder()
            .appender(Appender::builder().build("logfile", Box::new(logfile)))
            .build(Root::builder().appender("logfile").build(self.level))
            .unwrap();

        log4rs::init_config(config).unwrap();
    }

    fn default_level() -> LevelFilter {
        Logging::default().level
    }

    fn default_file() -> String {
        Logging::default().file
    }
}

#[derive(Debug, Deserialize)]
pub struct Settings {
    pub seed: Option<u64>,
    pub logging: Logging,
}

impl Settings {
    pub fn load() -> Result<Self, ConfigError> {
        let mut s = Config::new();
        s.merge(config::File::with_name("Config").required(false))?;
        s.merge(config::Environment::with_prefix("XAN"))?;
        s.try_into()
    }
}

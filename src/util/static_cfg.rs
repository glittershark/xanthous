use include_dir::Dir;
use serde::de;

macro_rules! __static_cfg_include {
    (toml_file, $filename:expr) => {
        include_str!($filename)
    };
    (toml_dir, $filename:expr) => {
        include_dir!($filename)
    };
    (json_file, $filename:expr) => {
        include_str!($filename)
    };
    (json_dir, $filename:expr) => {
        include_dir!($filename)
    };
}

macro_rules! __static_cfg_type {
    (toml_file) => (&'static str);
    (json_file) => (&'static str);
    (toml_dir) => (include_dir::Dir<'static>);
    (json_dir) => (include_dir::Dir<'static>);
}

macro_rules! __static_cfg_parse {
    (toml_file, $e:expr) => {
        toml::from_str($e).unwrap()
    };

    (json_file, $e:expr) => {
        serde_json::from_str($e).unwrap()
    };

    (toml_dir, $e:expr) => {
        crate::util::static_cfg::parse_toml_dir($e)
    };

    (json_dir, $e:expr) => {
        crate::util::static_cfg::parse_json_dir($e)
    };
}

macro_rules! __static_cfg_inner {
    ($(#[$attr:meta])* ($($vis:tt)*) static ref $N:ident : $T:ty = $kind:ident($filename:expr); $($t:tt)*) => {
        // static RAW: &'static str = __static_cfg_include!($kind, $filename);
        static RAW: __static_cfg_type!($kind) = __static_cfg_include!($kind, $filename);
        lazy_static! {
            $(#[$attr])* static ref $N: $T = __static_cfg_parse!($kind, RAW);
        }

        static_cfg!($($t)*);
    }
}

#[macro_export]
macro_rules! static_cfg {
    ($(#[$attr:meta])* static ref $N:ident : $T:ty = $kind:ident($filename:expr); $($t:tt)*) => {
        __static_cfg_inner!($(#[$attr])* () static ref $N : $T = $kind($filename); $($t)*);
    };

    ($(#[$attr:meta])* pub static ref $N:ident : $T:ty = $kind:ident($filename:expr); $($t:tt)*) => {
        __static_cfg_inner!($(#[$attr])* (pub) static ref $N : $T = $kind($filename); $($t)*);
    };

    ($(#[$attr:meta])* pub ($($vis:tt)+) static ref $N:ident : $T:ty = $kind:ident($filename:expr); $($t:tt)*) => {
        __static_cfg_inner!($(#[$attr])* (pub ($($vis)+)) static ref $N : $T = $kind($filename); $($t)*);
    };

    () => ()
}

pub fn parse_toml_dir<'a, T>(d: Dir<'a>) -> Vec<T>
where
    T: de::Deserialize<'a>,
{
    d.files()
        .iter()
        .map(|f| toml::from_str(f.contents_utf8().unwrap()).unwrap())
        .collect()
}

pub fn parse_json_dir<'a, T>(d: Dir<'a>) -> Vec<T>
where
    T: de::Deserialize<'a>,
{
    d.files()
        .iter()
        .map(|f| serde_json::from_str(f.contents_utf8().unwrap()).unwrap())
        .collect()
}

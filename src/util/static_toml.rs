macro_rules! __static_cfg_parse {
    (toml_file, $e:expr) => {
        toml::from_str($e)
    };

    (json_file, $e:expr) => {
        json::from_str($e)
    };
}

macro_rules! __static_cfg_inner {
    ($(#[$attr:meta])* ($($vis:tt)*) static ref $N:ident : $T:ty = $kind:ident($filename:expr); $($t:tt)*) => {
        static RAW: &'static str = include_str!($filename);
        lazy_static! {
            $(#[$attr])* static ref $N: $T = __static_cfg_parse!($kind, RAW).unwrap();
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

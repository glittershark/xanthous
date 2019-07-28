#[macro_export]
macro_rules! new_entity {
    ($name: ident) => {
        new_entity!($name, {})
    };

    ($name: ident { position: $position:expr $(, $fields:tt)* }) => {
        $name {
            id: None,
            position: $position,
            $($fields)*
        }
    };

    ($name: ident { $position:expr $(, $fields:tt)* }) => {
        $name {
            id: None,
            position: $position,
            $($fields)*
        }
    };
}

#[macro_export]
macro_rules! boring_entity {
    ($name:ident) => {
        entity! {
            pub struct $name {}
        }

        impl $name {
            #[allow(dead_code)]
            pub fn new(position: $crate::types::Position) -> Self {
                $name { id: None, position }
            }
        }
    };

    ($name:ident, char: $char: expr) => {
        boring_entity!($name);

        impl $crate::display::Draw for $name {
            fn do_draw(&self, out: &mut Write) -> io::Result<()> {
                write!(out, "{}", $char)
            }
        }
    };
}

#[macro_export]
macro_rules! entity {
    ($name: ident) => {
        positioned!($name);
        positioned_mut!($name);
        identified!($name, $crate::entities::EntityID);
        impl $crate::entities::entity::Entity for $name {}
    };

    (pub struct $name:ident { $($struct_contents:tt)* } $($rest:tt)*) => {
        #[derive(Debug, PartialEq, Eq, Clone)]
        pub struct $name {
            pub id: Option<$crate::entities::EntityID>,
            pub position: $crate::types::Position,
            $($struct_contents)*
        }

        entity!($name);
        entity!($($rest)*);
    };

    () => {};
}

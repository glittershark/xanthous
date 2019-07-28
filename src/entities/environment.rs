use crate::display;
use crate::display::draw_box::{BoxStyle, Stylable};
use crate::entities::Entity;
use crate::types::{Neighbors, Position};
use std::io::{self, Write};

entity! {
    pub struct Wall {
        pub style: BoxStyle
    }
}

impl Wall {
    pub fn new(position: Position, style: BoxStyle) -> Self {
        new_entity!(Wall { position, style })
    }
}

impl display::DrawWithNeighbors for Wall {
    fn do_draw_with_neighbors<'a, 'b>(
        &'a self,
        out: &'b mut Write,
        neighbors: &'a Neighbors<Vec<&'a Box<dyn Entity>>>,
    ) -> io::Result<()> {
        let neighbor_styles: Neighbors<Option<BoxStyle>> =
            neighbors.map(|es| {
                es.iter()
                    .filter_map(|e| e.downcast_ref::<Wall>())
                    .map(|wall| wall.style)
                    .next()
            });
        write!(out, "{}", neighbor_styles.style(self.style))
    }
}

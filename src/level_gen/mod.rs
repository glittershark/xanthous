use crate::display::draw_box::BoxStyle;
use crate::display::utils::clone_times;
use crate::display::DrawWithNeighbors;
use crate::entities::entity::Entity;
use crate::entities::environment::Wall;
use crate::types::entity_map::EntityMap;
use crate::types::pos;
use itertools::Itertools;
use std::io;

pub mod cave_automata;
pub mod util;

pub fn level_to_entities(level: Vec<Vec<bool>>) -> EntityMap<Box<dyn Entity>> {
    let mut res: EntityMap<Box<dyn Entity>> = EntityMap::new();

    let xmax = level.len() as i16;
    let ymax = if xmax == 0 {
        0i16
    } else {
        level[0].len() as i16
    };

    let get = |mut x: i16, mut y: i16| {
        if x < 0 {
            x = 0;
        }
        if y < 0 {
            y = 0;
        }
        if x >= xmax - 1 {
            x = xmax - 1;
        }
        if y >= ymax - 1 {
            y = ymax - 1;
        }
        level[x as usize][y as usize]
    };

    for x in 0..xmax {
        for y in 0..ymax {
            if get(x, y) {
                // don't output walls that are surrounded on all 8 sides by
                // walls
                if (x == 0 || get(x - 1, y))
                    && (y == 0 || get(x, y - 1))
                    && (x == xmax - 1 || get(x + 1, y))
                    && (y == ymax - 1 || get(x, y + 1))
                    && ((x == 0 && y == 0) || get(x - 1, y - 1))
                    && ((x == 0 && y == ymax - 1) || get(x - 1, y + 1))
                    && ((x == xmax - 1 && y == 0) || get(x + 1, y - 1))
                    && ((x == xmax - 1 && y == ymax - 1) || get(x + 1, y + 1))
                {
                    continue;
                }
                res.insert(Box::new(Wall::new(
                    pos(y as i16, x as i16),
                    BoxStyle::Thin,
                )));
            }
        }
    }

    res
}

pub fn draw_level<W: io::Write>(
    level: Vec<Vec<bool>>,
    out: &mut W,
) -> io::Result<()> {
    if level.is_empty() {
        return Ok(());
    }

    let mut lines = clone_times::<Vec<char>, Vec<Vec<char>>>(
        clone_times(' ', level[0].len() as u16),
        level.len() as u16,
    );

    let em = level_to_entities(level);

    for entity in em.entities() {
        let mut buf = Vec::new();
        entity.do_draw_with_neighbors(
            &mut buf,
            &em.neighbor_entities(entity.position()),
        )?;
        let buf_s = std::str::from_utf8(&buf).unwrap();
        if let Some(chr) = buf_s.chars().next() {
            lines[entity.position().y as usize][entity.position().x as usize] =
                chr;
        }
    }

    let res = lines
        .iter()
        .map(|line| line.iter().collect::<String>())
        .join("\n");

    write!(out, "{}", res)
}

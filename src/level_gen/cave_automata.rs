use crate::level_gen::util::fill_outer_edges;
use crate::level_gen::util::rand_initialize;
use crate::types::Dimensions;
use rand::Rng;

pub struct Params {
    chance_to_start_alive: f64,
    birth_limit: i32,
    death_limit: i32,
    steps: usize,
}

macro_rules! parse_optional {
    ($out: ident . $attr: ident, $matches: expr, $arg: expr) => {
        if let Some(val_s) = $matches.value_of($arg) {
            $out.$attr = val_s.parse().unwrap();
        }
    };
}

macro_rules! parse_optional_matches {
    ($matches: expr) => {};
    ($matches: expr , { $ret: ident . $attr: ident = $arg: expr }) => {
        parse_optional!($ret.$attr, $matches, $arg);
    };
    ($matches: expr, { $($ret: ident . $attr: ident = $arg: expr ,)* }) => {
        $(parse_optional!($ret.$attr, $matches, $arg);)*
    };
}

impl Params {
    pub fn from_matches<'a>(matches: &clap::ArgMatches<'a>) -> Self {
        let mut ret: Self = Default::default();
        parse_optional_matches!(matches, {
            ret.chance_to_start_alive = "start-alive-chance",
            ret.birth_limit = "birth-limit",
            ret.death_limit = "death-limit",
            ret.steps = "steps",
        });
        ret
    }
}

impl Default for Params {
    fn default() -> Self {
        Params {
            chance_to_start_alive: 0.45,
            birth_limit: 4,
            death_limit: 3,
            steps: 2,
        }
    }
}

pub fn generate<R: Rng + ?Sized>(
    dimensions: Dimensions,
    params: &Params,
    rand: &mut R,
) -> Vec<Vec<bool>> {
    let mut cells =
        rand_initialize(dimensions, rand, params.chance_to_start_alive);
    for _ in 0..params.steps {
        step_automata(&mut cells, dimensions, params);
    }

    fill_outer_edges(&mut cells);

    cells
}

fn step_automata(
    cells: &mut Vec<Vec<bool>>,
    dimensions: Dimensions,
    params: &Params,
) {
    let orig_cells = (*cells).clone();
    for x in 0..(dimensions.h as usize) {
        for y in 0..(dimensions.w as usize) {
            let nbs = num_alive_neighbors(&orig_cells, x as i32, y as i32);
            if orig_cells[x][y] {
                if nbs < params.death_limit {
                    cells[x][y] = false;
                } else {
                    cells[x][y] = true;
                }
            } else if nbs > params.birth_limit {
                cells[x][y] = true;
            } else {
                cells[x][y] = false;
            }
        }
    }
}

const COUNT_EDGES_AS_NEIGHBORS: bool = true;

fn num_alive_neighbors(cells: &[Vec<bool>], x: i32, y: i32) -> i32 {
    let mut count = 0;
    for i in -1..2 {
        for j in -1..2 {
            if i == 0 && j == 0 {
                continue;
            }

            let neighbor_x = x + i;
            let neighbor_y = y + j;

            if (COUNT_EDGES_AS_NEIGHBORS
                && (neighbor_x < 0
                    || neighbor_y < 0
                    || neighbor_x >= (cells.len() as i32)
                    || neighbor_y >= (cells[0].len()) as i32))
                || cells[neighbor_x as usize][neighbor_y as usize]
            {
                count += 1;
            }
        }
    }
    count
}

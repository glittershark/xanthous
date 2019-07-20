use crate::level_gen::util::rand_initialize;
use crate::types::Dimensions;
use rand::Rng;

pub struct Params {
    chance_to_start_alive: f64,
    dimensions: Dimensions,
    birth_limit: i32,
    death_limit: i32,
    steps: usize,
}

impl Default for Params {
    fn default() -> Self {
        Params {
            chance_to_start_alive: 0.45,
            dimensions: Dimensions { w: 80, h: 20 },
            birth_limit: 4,
            death_limit: 3,
            steps: 2,
        }
    }
}

pub fn generate<R: Rng + ?Sized>(
    params: &Params,
    rand: &mut R,
) -> Vec<Vec<bool>> {
    let mut cells =
        rand_initialize(&params.dimensions, rand, params.chance_to_start_alive);
    for _ in 0..params.steps {
        step_automata(&mut cells, params);
    }
    cells
}

fn step_automata(cells: &mut Vec<Vec<bool>>, params: &Params) {
    let orig_cells = (*cells).clone();
    for x in 0..(params.dimensions.h as usize) {
        for y in 0..(params.dimensions.w as usize) {
            let nbs = num_alive_neighbors(&orig_cells, x as i32, y as i32);
            if orig_cells[x][y] {
                if nbs < params.death_limit {
                    cells[x][y] = false;
                } else {
                    cells[x][y] = true;
                }
            } else {
                if nbs > params.birth_limit {
                    cells[x][y] = true;
                } else {
                    cells[x][y] = false;
                }
            }
        }
    }
}

const COUNT_EDGES_AS_NEIGHBORS: bool = true;

fn num_alive_neighbors(cells: &Vec<Vec<bool>>, x: i32, y: i32) -> i32 {
    let mut count = 0;
    for i in -1..2 {
        for j in -1..2 {
            if i == 0 && j == 0 {
                continue;
            }

            let neighbor_x = x + i;
            let neighbor_y = y + j;

            if COUNT_EDGES_AS_NEIGHBORS
                && (neighbor_x < 0
                    || neighbor_y < 0
                    || neighbor_x >= (cells.len() as i32)
                    || neighbor_y >= (cells[0].len()) as i32)
            {
                count += 1;
            } else if cells[neighbor_x as usize][neighbor_y as usize] {
                count += 1;
            }
        }
    }
    count
}

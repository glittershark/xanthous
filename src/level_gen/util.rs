use crate::types::Dimensions;
use rand::{distributions, Rng};

pub fn falses(dims: &Dimensions) -> Vec<Vec<bool>> {
    let mut ret = Vec::with_capacity(dims.h as usize);
    for _ in 0..dims.h {
        let mut row = Vec::with_capacity(dims.w as usize);
        for _ in 0..dims.w {
            row.push(false);
        }
        ret.push(row);
    }
    ret
}

/// Randomly initialize a 2-dimensional boolean vector of the given
/// `Dimensions`, using the given random number generator and alive chance
pub fn rand_initialize<R: Rng + ?Sized>(
    dims: &Dimensions,
    rng: &mut R,
    alive_chance: f64,
) -> Vec<Vec<bool>> {
    let distrib = distributions::Bernoulli::new(alive_chance).unwrap();
    let mut ret = Vec::with_capacity(dims.h as usize);
    for _ in 0..dims.h {
        let mut row = Vec::with_capacity(dims.w as usize);
        for _ in 0..dims.w {
            row.push(rng.sample(distrib));
        }
        ret.push(row);
    }
    ret
}

/// Fill the outer edges of a generated level with walls
pub fn fill_outer_edges(level: &mut Vec<Vec<bool>>) {
    let xmax = level.len();
    if xmax == 0 {
        return;
    }
    let ymax = level[0].len();

    for x in 0..xmax {
        level[x][0] = true;
        level[x][ymax - 1] = true;
    }

    for y in 0..level[0].len() {
        level[0][y] = true;
        level[xmax - 1][y] = true;
    }
}

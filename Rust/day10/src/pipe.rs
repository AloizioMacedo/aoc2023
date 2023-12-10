use ndarray::Array2;

use anyhow::{anyhow, Result};
use petgraph::graphmap::UnGraphMap;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Pipe {
    Vertical,
    Horizontal,
    NE,
    NW,
    SW,
    SE,
    Origin,

    #[default]
    Empty,
}

impl TryFrom<char> for Pipe {
    type Error = anyhow::Error;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '|' => Ok(Self::Vertical),
            '-' => Ok(Self::Horizontal),
            'L' => Ok(Self::NE),
            'J' => Ok(Self::NW),
            '7' => Ok(Self::SW),
            'F' => Ok(Self::SE),
            'S' => Ok(Self::Origin),
            '.' => Ok(Self::Empty),
            _ => Err(anyhow!("Invalid pipe")),
        }
    }
}

impl Pipe {
    pub(crate) fn get_possible_right(&self) -> Vec<Pipe> {
        match self {
            Pipe::Horizontal => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NW, Pipe::SW],
            Pipe::NE => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NW, Pipe::SW],
            Pipe::SE => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NW, Pipe::SW],
            Pipe::Origin => vec![Pipe::Horizontal, Pipe::NW, Pipe::SW],
            _ => vec![],
        }
    }

    pub(crate) fn get_possible_left(&self) -> Vec<Pipe> {
        match self {
            Pipe::Horizontal => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NE, Pipe::SE],
            Pipe::NW => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NE, Pipe::SE],
            Pipe::SW => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NE, Pipe::SE],
            Pipe::Origin => vec![Pipe::Horizontal, Pipe::NE, Pipe::SE],
            _ => vec![],
        }
    }

    pub(crate) fn get_possible_up(&self) -> Vec<Pipe> {
        match self {
            Pipe::Vertical => vec![Pipe::Origin, Pipe::Vertical, Pipe::SE, Pipe::SW],
            Pipe::NE => vec![Pipe::Origin, Pipe::Vertical, Pipe::SE, Pipe::SW],
            Pipe::NW => vec![Pipe::Origin, Pipe::Vertical, Pipe::SE, Pipe::SW],
            Pipe::Origin => vec![Pipe::Vertical, Pipe::SW, Pipe::SE],
            _ => vec![],
        }
    }

    pub(crate) fn get_possible_down(&self) -> Vec<Pipe> {
        match self {
            Pipe::Vertical => vec![Pipe::Origin, Pipe::Vertical, Pipe::NE, Pipe::NW],
            Pipe::SE => vec![Pipe::Origin, Pipe::Vertical, Pipe::NE, Pipe::NW],
            Pipe::SW => vec![Pipe::Origin, Pipe::Vertical, Pipe::NE, Pipe::NW],
            Pipe::Origin => vec![Pipe::Vertical, Pipe::NE, Pipe::NW],
            _ => vec![],
        }
    }

    pub(crate) fn get_scaled_up_version(&self) -> Array2<Pipe> {
        match self {
            Pipe::Vertical => Array2::from_shape_vec(
                (3, 3),
                vec![
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                ],
            )
            .expect("Should have correct shape"),
            Pipe::Horizontal => Array2::from_shape_vec(
                (3, 3),
                vec![
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Horizontal,
                    Pipe::Horizontal,
                    Pipe::Horizontal,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                ],
            )
            .expect("Should have correct shape"),
            Pipe::NE => Array2::from_shape_vec(
                (3, 3),
                vec![
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::NE,
                    Pipe::Horizontal,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                ],
            )
            .expect("Should have correct shape"),
            Pipe::NW => Array2::from_shape_vec(
                (3, 3),
                vec![
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                    Pipe::Horizontal,
                    Pipe::NW,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                ],
            )
            .expect("Should have correct shape"),
            Pipe::SW => Array2::from_shape_vec(
                (3, 3),
                vec![
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Horizontal,
                    Pipe::SW,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                ],
            )
            .expect("Should have correct shape"),
            Pipe::SE => Array2::from_shape_vec(
                (3, 3),
                vec![
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::SE,
                    Pipe::Horizontal,
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                ],
            )
            .expect("Should have correct shape"),
            Pipe::Empty => Array2::from_shape_vec(
                (3, 3),
                vec![
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                    Pipe::Empty,
                ],
            )
            .expect("Should have correct shape"),
            Pipe::Origin => Array2::from_shape_vec(
                (3, 3),
                vec![
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                    Pipe::Horizontal,
                    Pipe::Origin,
                    Pipe::Horizontal,
                    Pipe::Empty,
                    Pipe::Vertical,
                    Pipe::Empty,
                ],
            )
            .expect("Should have correct shape"),
        }
    }
}

pub(crate) fn find_loop(
    graph: &UnGraphMap<(usize, usize), ()>,
    beginning: (usize, usize),
) -> Result<Vec<(usize, usize)>> {
    let mut queue = vec![vec![beginning]];

    while let Some(next) = queue.pop() {
        let last = *next.last().expect("Paths should not be empty");

        if last == beginning && next.len() > 1 {
            return Ok(next);
        }

        let neighbors: Vec<(usize, usize)> = graph
            .neighbors(last)
            .filter(|n| {
                if next.len() < 2 {
                    return true;
                }
                if let Some(x) = next.get(next.len() - 2) {
                    n != x
                } else {
                    true
                }
            })
            .collect();

        for neighbor in neighbors {
            let mut path = next.clone();
            path.push(neighbor);

            queue.push(path);
        }
    }

    Err(anyhow!("ERROR: Loop not found"))
}

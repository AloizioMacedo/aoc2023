use std::{
    collections::{HashMap, HashSet},
    default,
};

use anyhow::{anyhow, Result};
use ndarray::Array2;
use petgraph::{data::Build, graphmap::UnGraphMap};

const INPUT: &str = include_str!("../input.txt");
const TEST: &str = include_str!("../test_input_p1.txt");

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum Pipe {
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
    fn get_possible_right(&self) -> Vec<Pipe> {
        match self {
            Pipe::Horizontal => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NW, Pipe::SW],
            Pipe::NE => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NW, Pipe::SW],
            Pipe::SE => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NW, Pipe::SW],
            Pipe::Origin => vec![
                Pipe::Vertical,
                Pipe::Horizontal,
                Pipe::NE,
                Pipe::NW,
                Pipe::SW,
                Pipe::SE,
            ],
            _ => vec![],
        }
    }

    fn get_possible_left(&self) -> Vec<Pipe> {
        match self {
            Pipe::Horizontal => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NE, Pipe::SE],
            Pipe::NW => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NE, Pipe::SE],
            Pipe::SW => vec![Pipe::Origin, Pipe::Horizontal, Pipe::NE, Pipe::SE],
            Pipe::Origin => vec![
                Pipe::Vertical,
                Pipe::Horizontal,
                Pipe::NE,
                Pipe::NW,
                Pipe::SW,
                Pipe::SE,
            ],
            _ => vec![],
        }
    }

    fn get_possible_up(&self) -> Vec<Pipe> {
        match self {
            Pipe::Vertical => vec![Pipe::Origin, Pipe::Vertical, Pipe::SE, Pipe::SW],
            Pipe::NE => vec![Pipe::Origin, Pipe::Vertical, Pipe::SE, Pipe::SW],
            Pipe::NW => vec![Pipe::Origin, Pipe::Vertical, Pipe::SE, Pipe::SW],
            Pipe::Origin => vec![
                Pipe::Vertical,
                Pipe::Horizontal,
                Pipe::NE,
                Pipe::NW,
                Pipe::SW,
                Pipe::SE,
            ],
            _ => vec![],
        }
    }

    fn get_possible_down(&self) -> Vec<Pipe> {
        match self {
            Pipe::Vertical => vec![Pipe::Origin, Pipe::Vertical, Pipe::NE, Pipe::NW],
            Pipe::SE => vec![Pipe::Origin, Pipe::Vertical, Pipe::NE, Pipe::NW],
            Pipe::SW => vec![Pipe::Origin, Pipe::Vertical, Pipe::NE, Pipe::NW],
            Pipe::Origin => vec![
                Pipe::Vertical,
                Pipe::Horizontal,
                Pipe::NE,
                Pipe::NW,
                Pipe::SW,
                Pipe::SE,
            ],
            _ => vec![],
        }
    }
}

#[derive(Debug)]
struct Grid {
    origin: (usize, usize),
    matrix: Array2<Pipe>,
}

impl Grid {
    fn get_graph(&self) -> UnGraphMap<(usize, usize), ()> {
        let mut graph = UnGraphMap::new();

        for ((i, j), p) in self.matrix.indexed_iter() {
            if i > 0 && p.get_possible_up().contains(&self.matrix[(i - 1, j)]) {
                graph.add_edge((i, j), (i - 1, j), ());
            }
            if i < self.matrix.nrows() - 1
                && p.get_possible_down().contains(&self.matrix[(i + 1, j)])
            {
                graph.add_edge((i, j), (i + 1, j), ());
            }

            if j > 0 && p.get_possible_left().contains(&self.matrix[(i, j - 1)]) {
                graph.add_edge((i, j), (i, j - 1), ());
            }
            if j < self.matrix.ncols() - 1
                && p.get_possible_left().contains(&self.matrix[(i, j + 1)])
            {
                graph.add_edge((i, j), (i, j + 1), ());
            }
        }

        graph
    }
}

fn parse_contents(contents: &str) -> Result<Grid> {
    let lines: Vec<&str> = contents.lines().collect();

    let n_rows = lines.len();
    let n_cols = lines[0].len();

    let mut matrix: Array2<Pipe> = Array2::default((n_rows, n_cols));

    let mut origin = (0, 0);
    for (i, line) in lines.iter().enumerate() {
        for (j, c) in line.chars().enumerate() {
            matrix[(i, j)] = Pipe::try_from(c)?;

            if matches!(matrix[(i, j)], Pipe::Origin) {
                origin = (i, j);
            }
        }
    }

    Ok(Grid { origin, matrix })
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let grid = parse_contents(contents)?;

    todo!()
}

fn find_loop(graph: &UnGraphMap<(usize, usize), ()>, beginning: (usize, usize)) -> Result<usize> {
    let mut queue = vec![beginning];
    let mut visited: HashSet<(usize, usize)> = HashSet::new();

    let mut distances: HashMap<(usize, usize), usize> = HashMap::new();
    distances.insert(beginning, 0);

    while let Some(next) = queue.pop() {
        if !visited.insert(next) {
            return Ok(distances[&next]);
        }

        let neighbors: Vec<(usize, usize)> = graph
            .neighbors(next)
            .filter(|n| !visited.contains(n))
            .collect();

        for neighbor in neighbors {
            queue.push(neighbor);
            distances.insert(neighbor, distances[&next] + 1);
        }
    }

    Err(anyhow!("ERROR: Loop not found"))
}

fn main() -> Result<()> {
    let grid = parse_contents(TEST)?;

    let graph = grid.get_graph();
    println!("{:?}", graph);

    println!("{}", find_loop(&graph, grid.origin)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input_p1.txt");

    #[test]
    fn it_works() {
        assert_eq!(solve_part_one(TEST).unwrap(), 8)
    }
}

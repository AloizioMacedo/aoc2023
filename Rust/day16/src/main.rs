use std::{
    collections::{HashSet, VecDeque},
    fmt::Display,
};

use anyhow::{anyhow, Result};
use ndarray::Array2;

const TEST_INPUT: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Direction {
    ToRight,
    ToDown,
    ToLeft,
    ToUp,
}

#[derive(Debug, Clone, Copy)]
enum Tile {
    Empty,
    Vertical,
    Horizontal,
    DiagonalRightTop,
    DiagonalLeftTop,
}

#[derive(Debug, Clone, Copy)]
enum Energized {
    Tile(Tile),
    Energized,
}

impl Default for Energized {
    fn default() -> Self {
        Self::Tile(Tile::Empty)
    }
}

impl Display for Energized {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tile(_) => write!(f, "."),
            Self::Energized => write!(f, "#"),
        }
    }
}

impl Tile {
    fn get_next(&self, dir: &Direction) -> Vec<Direction> {
        match self {
            Self::Empty => vec![*dir],
            Self::Vertical => match dir {
                Direction::ToRight => vec![Direction::ToUp, Direction::ToDown],
                Direction::ToLeft => vec![Direction::ToUp, Direction::ToDown],
                Direction::ToUp => vec![Direction::ToUp],
                Direction::ToDown => vec![Direction::ToDown],
            },
            Self::Horizontal => match dir {
                Direction::ToUp => vec![Direction::ToLeft, Direction::ToRight],
                Direction::ToDown => vec![Direction::ToLeft, Direction::ToRight],
                Direction::ToLeft => vec![Direction::ToLeft],
                Direction::ToRight => vec![Direction::ToRight],
            },
            Self::DiagonalRightTop => match dir {
                Direction::ToUp => vec![Direction::ToRight],
                Direction::ToDown => vec![Direction::ToLeft],
                Direction::ToLeft => vec![Direction::ToDown],
                Direction::ToRight => vec![Direction::ToUp],
            },
            Self::DiagonalLeftTop => match dir {
                Direction::ToUp => vec![Direction::ToLeft],
                Direction::ToDown => vec![Direction::ToRight],
                Direction::ToLeft => vec![Direction::ToUp],
                Direction::ToRight => vec![Direction::ToDown],
            },
        }
    }
}

impl TryFrom<char> for Tile {
    type Error = anyhow::Error;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '.' => Ok(Self::Empty),
            '|' => Ok(Self::Vertical),
            '-' => Ok(Self::Horizontal),
            '/' => Ok(Self::DiagonalRightTop),
            '\\' => Ok(Self::DiagonalLeftTop),
            _ => Err(anyhow!("Invalid char")),
        }
    }
}

fn parse_contents(contents: &str) -> Array2<Tile> {
    let mut lines = contents.lines().peekable();
    let first_line = *lines.peek().expect("Should have at least one line");

    let nrows = lines.count();
    let ncols = first_line.chars().count();

    let contents: Vec<Tile> = contents
        .lines()
        .flat_map(|line| line.chars().flat_map(|c| Tile::try_from(c)))
        .collect();

    Array2::from_shape_vec((nrows, ncols), contents).expect("Should have correct shape")
}

fn release_beam(
    matrix: &Array2<Tile>,
    beginning: ((usize, usize), Direction),
) -> Array2<Energized> {
    let nrows = matrix.nrows();
    let ncols = matrix.ncols();

    let mut energized_matrix = Array2::default((nrows, ncols));

    let mut queue = VecDeque::new();
    let mut visited: HashSet<((usize, usize), Direction)> = HashSet::new();

    queue.push_front(beginning);

    while let Some((current, direction)) = queue.pop_back() {
        if !visited.insert((current, direction)) {
            continue;
        }

        energized_matrix[current] = Energized::Energized;

        let next_directions = matrix[current].get_next(&direction);

        for direction in next_directions {
            match direction {
                Direction::ToUp => {
                    if current.0 > 0 {
                        queue.push_front(((current.0 - 1, current.1), direction))
                    }
                }
                Direction::ToRight => {
                    if current.1 < ncols - 1 {
                        queue.push_front(((current.0, current.1 + 1), direction))
                    }
                }
                Direction::ToDown => {
                    if current.0 < nrows - 1 {
                        queue.push_front(((current.0 + 1, current.1), direction))
                    }
                }
                Direction::ToLeft => {
                    if current.1 > 0 {
                        queue.push_front(((current.0, current.1 - 1), direction))
                    }
                }
            }
        }
    }

    energized_matrix
}

fn solve_part_one(contents: &str) -> usize {
    let matrix = parse_contents(contents);

    let energized = release_beam(&matrix, ((0, 0), Direction::ToRight));

    energized
        .iter()
        .filter(|t| matches!(t, Energized::Energized))
        .count()
}

fn solve_part_two(contents: &str) -> usize {
    let matrix = parse_contents(contents);

    let mut max = usize::MIN;

    for i in 0..matrix.nrows() {
        let energized = release_beam(&matrix, ((i, 0), Direction::ToRight));

        let n_energized = energized
            .iter()
            .filter(|t| matches!(t, Energized::Energized))
            .count();

        if max < n_energized {
            max = n_energized;
        }

        let energized = release_beam(&matrix, ((i, matrix.ncols() - 1), Direction::ToLeft));

        let n_energized = energized
            .iter()
            .filter(|t| matches!(t, Energized::Energized))
            .count();

        if max < n_energized {
            max = n_energized;
        }
    }

    for j in 0..matrix.ncols() {
        let energized = release_beam(&matrix, ((0, j), Direction::ToDown));

        let n_energized = energized
            .iter()
            .filter(|t| matches!(t, Energized::Energized))
            .count();

        if max < n_energized {
            max = n_energized;
        }

        let energized = release_beam(&matrix, ((matrix.nrows() - 1, j), Direction::ToUp));

        let n_energized = energized
            .iter()
            .filter(|t| matches!(t, Energized::Energized))
            .count();

        if max < n_energized {
            max = n_energized;
        }
    }

    max
}

fn main() {
    println!("{}", solve_part_one(INPUT));
    println!("{}", solve_part_two(INPUT));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT), 46)
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT), 51)
    }
}

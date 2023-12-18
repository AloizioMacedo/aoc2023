use std::{
    collections::{HashSet, VecDeque},
    str::FromStr,
};

use anyhow::{anyhow, Result};
use itertools::Itertools;
use ndarray::Array2;

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone, Copy)]
enum Direction {
    U,
    R,
    D,
    L,
}

impl FromStr for Direction {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "U" => Ok(Self::U),
            "R" => Ok(Self::R),
            "D" => Ok(Self::D),
            "L" => Ok(Self::L),
            _ => Err(anyhow!("String {s} is not a valid direction")),
        }
    }
}

impl TryFrom<char> for Direction {
    type Error = anyhow::Error;

    fn try_from(value: char) -> Result<Self> {
        match value {
            '0' => Ok(Self::R),
            '1' => Ok(Self::D),
            '2' => Ok(Self::L),
            '3' => Ok(Self::U),
            _ => Err(anyhow!("Char {value} cannot be a direction")),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Instruction<'a> {
    direction: Direction,
    count: usize,
    color: &'a str,
}

fn parse_line(line: &str) -> Result<Instruction> {
    let (direction, count, color) = line
        .splitn(3, ' ')
        .collect_tuple()
        .ok_or(anyhow!("Syntax error"))?;

    let direction = direction.parse()?;
    let count = count.parse()?;
    let color = color.trim_matches(|c| c == '(' || c == ')');

    Ok(Instruction {
        direction,
        count,
        color,
    })
}

fn parse_line_p2(line: &str) -> Result<Instruction> {
    let (_, _, instruction) = line
        .splitn(3, ' ')
        .collect_tuple()
        .ok_or(anyhow!("Syntax error"))?;

    let instruction = instruction.trim_matches(|c| c == '(' || c == ')' || c == '#');
    let count = usize::from_str_radix(&instruction[0..5], 16)?;
    let direction = instruction
        .chars()
        .last()
        .ok_or(anyhow!("Empty direction"))?;
    let direction = Direction::try_from(direction)?;

    Ok(Instruction {
        direction,
        count,
        color: "",
    })
}

fn parse_contents(contents: &str) -> Result<Vec<Instruction>> {
    contents.lines().map(parse_line).collect()
}

fn parse_contents_p2(contents: &str) -> Result<Vec<Instruction>> {
    contents.lines().map(parse_line_p2).collect()
}

fn build_grid(instructions: &[Instruction]) -> Grid {
    let mut origin = (0, 0);

    let nrows = instructions.iter().fold(0, |acc, x| match x {
        Instruction {
            direction: Direction::U,
            count,
            ..
        } => {
            origin = (origin.0 + count, origin.1);
            acc + count
        }
        Instruction {
            direction: Direction::D,
            count,
            ..
        } => acc + count,
        _ => acc,
    });

    let ncols = instructions.iter().fold(0, |acc, x| match x {
        Instruction {
            direction: Direction::R,
            count,
            ..
        } => acc + count,
        Instruction {
            direction: Direction::L,
            count,
            ..
        } => {
            origin = (origin.0, origin.1 + count);
            acc + count
        }
        _ => acc,
    });

    let mut matrix = Array2::from_elem((nrows + 1, ncols + 1), '.');

    let mut current = origin;

    for Instruction {
        direction, count, ..
    } in instructions
    {
        for _ in 0..*count {
            match direction {
                Direction::U => current = (current.0 - 1, current.1),
                Direction::R => current = (current.0, current.1 + 1),
                Direction::D => current = (current.0 + 1, current.1),
                Direction::L => current = (current.0, current.1 - 1),
            };

            matrix[current] = '#';
        }
    }

    Grid { origin, matrix }
}

fn flood_fill(matrix: &Array2<char>) -> Array2<char> {
    let mut queue: VecDeque<(usize, usize)> = VecDeque::new();
    queue.push_back((0, 0));

    let mut visited = HashSet::new();

    let mut new_matrix = matrix.clone();
    while let Some((i, j)) = queue.pop_front() {
        if !visited.insert((i, j)) {
            continue;
        };
        new_matrix[(i, j)] = 'O';

        if i > 0 && !visited.contains(&(i - 1, j)) && matrix[(i - 1, j)] != '#' {
            queue.push_back((i - 1, j));
        }
        if i < matrix.nrows() - 1 && !visited.contains(&(i + 1, j)) && matrix[(i + 1, j)] != '#' {
            queue.push_back((i + 1, j));
        }
        if j > 0 && !visited.contains(&(i, j - 1)) && matrix[(i, j - 1)] != '#' {
            queue.push_back((i, j - 1));
        }
        if j < matrix.ncols() - 1 && !visited.contains(&(i, j + 1)) && matrix[(i, j + 1)] != '#' {
            queue.push_back((i, j + 1));
        }
    }

    new_matrix
}

#[derive(Debug)]
struct Grid {
    origin: (usize, usize),
    matrix: Array2<char>,
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let instructions = parse_contents(contents)?;
    let grid = build_grid(&instructions);
    let filled = flood_fill(&grid.matrix);
    println!("{:?}", filled);

    Ok(filled.iter().filter(|&&c| c == '#' || c == '.').count())
}

fn solve_part_two(contents: &str) {}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 62);
    }
}

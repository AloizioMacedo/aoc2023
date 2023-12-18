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

fn get_vertices(instructions: &[Instruction]) -> (Vec<(i64, i64)>, i64) {
    let mut current = (0, 0);

    let mut vertices = vec![(0, 0)];

    let mut total_boundary = 0;
    for Instruction {
        count, direction, ..
    } in instructions
    {
        match direction {
            Direction::U => current = (current.0 - *count as i64, current.1),
            Direction::R => current = (current.0, current.1 + *count as i64),
            Direction::D => current = (current.0 + *count as i64, current.1),
            Direction::L => current = (current.0, current.1 - *count as i64),
        };

        total_boundary += *count;
        vertices.push(current);
    }

    (vertices, total_boundary as i64)
}

fn shoelace(vertices: &[(i64, i64)]) -> i64 {
    if vertices.is_empty() {
        return 0;
    }

    let mut vertices = vertices.to_vec();
    vertices.push(
        *vertices
            .first()
            .expect("Vertices should not be empty, as it would be early returned"),
    );

    let mut total = 0;
    for window in vertices.windows(2) {
        let (x1, y1) = window[0];
        let (x2, y2) = window[1];

        total += (x1 * y2) - (y1 * x2);
    }

    total
}

fn get_count_of_points_inside(twice_area: i64, boundary: i64) -> i64 {
    (twice_area - boundary + 2) / 2
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let instructions = parse_contents(contents)?;
    let grid = build_grid(&instructions);
    let filled = flood_fill(&grid.matrix);

    Ok(filled.iter().filter(|&&c| c == '#' || c == '.').count())
}

fn solve_part_two(contents: &str) -> Result<usize> {
    let instructions = parse_contents_p2(contents)?;
    let (vertices, boundary) = get_vertices(&instructions);

    let twice_area = shoelace(&vertices).abs();

    Ok(get_count_of_points_inside(twice_area, boundary) as usize + boundary as usize)
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 62);
    }

    #[test]
    fn test_shoelace() {
        assert_eq!(shoelace(&[(1, 6), (3, 1), (7, 2), (4, 4), (8, 5)]), 33);
    }

    #[test]
    fn test_pick() {
        assert_eq!(get_count_of_points_inside(12, 10), 2);
    }

    #[test]
    fn test_boundary() {
        assert_eq!(
            get_vertices(&[
                Instruction {
                    direction: Direction::R,
                    count: 3,
                    color: "",
                },
                Instruction {
                    direction: Direction::D,
                    count: 2,
                    color: "",
                },
                Instruction {
                    direction: Direction::L,
                    count: 3,
                    color: "",
                },
                Instruction {
                    direction: Direction::U,
                    count: 2,
                    color: "",
                },
            ],)
            .1,
            10
        );
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST).unwrap(), 952408144115);
    }
}

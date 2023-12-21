use std::{collections::VecDeque, thread::current};

use anyhow::{anyhow, Result};
use ndarray::Array2;

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

fn parse_line(line: &str) {}

fn parse_contents(contents: &str) -> Result<Array2<char>> {
    let lines = contents.lines().collect::<Vec<_>>();

    let nrows = lines.len();
    let ncols = lines[0].len();

    let flat_matrix = lines.iter().flat_map(|x| x.chars()).collect::<Vec<_>>();

    Ok(Array2::from_shape_vec((nrows, ncols), flat_matrix)?)
}

fn bfs(matrix: &Array2<char>, steps: usize) -> Result<usize> {
    let mut matrix = matrix.clone();

    let nrows = matrix.nrows();
    let ncols = matrix.ncols();

    let beginning = matrix
        .indexed_iter()
        .find(|(_, &v)| v == 'S')
        .ok_or(anyhow!("No 'S'"))?
        .0;

    let mut current_queue = VecDeque::new();
    let mut next_queue = VecDeque::new();

    current_queue.push_front(beginning);

    let mut counter = 0;
    while let Some((i, j)) = current_queue.pop_back() {
        if counter >= steps {
            break;
        }

        matrix[(i, j)] = '.';

        if i > 0 && matrix[(i - 1, j)] == '.' {
            matrix[(i - 1, j)] = 'O';
            next_queue.push_front((i - 1, j));
        }
        if i < nrows - 1 && matrix[(i + 1, j)] == '.' {
            matrix[(i + 1, j)] = 'O';
            next_queue.push_front((i + 1, j));
        }
        if j > 0 && matrix[(i, j - 1)] == '.' {
            matrix[(i, j - 1)] = 'O';
            next_queue.push_front((i, j - 1));
        }
        if j < ncols - 1 && matrix[(i, j + 1)] == '.' {
            matrix[(i, j + 1)] = 'O';
            next_queue.push_front((i, j + 1));
        }

        if current_queue.is_empty() {
            current_queue.extend(&next_queue);
            next_queue.clear();

            counter += 1;
        }
    }

    Ok(matrix.iter().filter(|&&x| x == 'O').count())
}

fn solve_part_one(contents: &str, n: usize) -> Result<usize> {
    let matrix = parse_contents(contents)?;

    bfs(&matrix, n)
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT, 64)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST, 6).unwrap(), 16);
    }
}

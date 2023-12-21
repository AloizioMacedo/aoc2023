use std::collections::{HashSet, VecDeque};

use anyhow::{anyhow, Result};
use ndarray::Array2;

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone)]
struct Matrix {
    base: Array2<char>,
    steps: HashSet<(i32, i32)>,
}

impl Matrix {
    fn get(&self, tup: (i32, i32)) -> char {
        if self.steps.contains(&tup) {
            return 'O';
        }

        let nrows = self.base.nrows();
        let ncols = self.base.ncols();

        self.base[(
            tup.0.rem_euclid(nrows as i32) as usize,
            tup.1.rem_euclid(ncols as i32) as usize,
        )]
    }

    fn set(&mut self, tup: (i32, i32), c: char) {
        if let 'O' = c {
            self.steps.insert(tup);
        } else {
            self.steps.remove(&tup);
        }
    }
}

fn parse_contents(contents: &str) -> Result<Array2<char>> {
    let lines = contents.lines().collect::<Vec<_>>();

    let nrows = lines.len();
    let ncols = lines[0].len();

    let flat_matrix = lines.iter().flat_map(|x| x.chars()).collect::<Vec<_>>();

    Ok(Array2::from_shape_vec((nrows, ncols), flat_matrix)?)
}

fn parse_contents_p2(contents: &str) -> Result<Matrix> {
    let lines = contents.lines().collect::<Vec<_>>();

    let nrows = lines.len();
    let ncols = lines[0].len();

    let flat_matrix = lines.iter().flat_map(|x| x.chars()).collect::<Vec<_>>();

    let base = Array2::from_shape_vec((nrows, ncols), flat_matrix)?;
    let steps = HashSet::new();

    Ok(Matrix { base, steps })
}

fn bfs_matrix(matrix: &Matrix, steps: usize) -> Result<usize> {
    let mut matrix = matrix.clone();

    let beginning = matrix
        .base
        .indexed_iter()
        .find(|(_, &v)| v == 'S')
        .ok_or(anyhow!("No 'S'"))?
        .0;

    matrix.base[beginning] = 'O';

    matrix.set((beginning.0 as i32, beginning.1 as i32), 'O');

    let mut current_queue = VecDeque::new();
    let mut next_queue = VecDeque::new();

    current_queue.push_front((beginning.0 as i32, beginning.1 as i32));

    let mut counter = 0;
    while let Some((i, j)) = current_queue.pop_back() {
        if counter >= steps {
            break;
        }

        //eprintln!(
        //"Current: {:?}, Steps before proceeding: {:?}",
        //(i, j),
        //matrix.steps
        //);

        matrix.set((i, j), '.');
        // eprintln!(
        //"North: {:?}, South: {:?}, West: {:?}, East: {:?}",
        //matrix.get((i - 1, j)),
        //matrix.get((i + 1, j)),
        //matrix.get((i, j - 1)),
        //matrix.get((i, j + 1)),
        //);

        if matches!(matrix.get((i - 1, j)), '.' | 'O') {
            matrix.set((i - 1, j), 'O');
            next_queue.push_front((i - 1, j));
        }
        if matches!(matrix.get((i + 1, j)), '.' | 'O') {
            matrix.set((i + 1, j), 'O');
            next_queue.push_front((i + 1, j));
        }
        if matches!(matrix.get((i, j - 1)), '.' | 'O') {
            matrix.set((i, j - 1), 'O');
            next_queue.push_front((i, j - 1));
        }
        if matches!(matrix.get((i, j + 1)), '.' | 'O') {
            matrix.set((i, j + 1), 'O');
            next_queue.push_front((i, j + 1));
        }

        // eprintln!("Steps after proceeding: {:?}", matrix.steps);

        if current_queue.is_empty() {
            current_queue.extend(&next_queue);
            next_queue.clear();

            counter += 1;
            println!("{:?}", current_queue.len());
            println!("{counter}: {}", matrix.steps.len())
        }
    }

    Ok(matrix.steps.len())
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

fn solve_part_two(contents: &str, n: usize) -> Result<usize> {
    let matrix = parse_contents_p2(contents)?;
    let nrows = matrix.base.nrows();

    bfs_matrix(&matrix, n)
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT, 64)?);
    // println!("{}", solve_part_two(TEST, 5)?);
    println!("{}", solve_part_two(TEST, 26_501_365)?);
    // println!("{}", solve_part_one(INPUT, 26_501_365)?);

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

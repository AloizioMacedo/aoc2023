use std::collections::{HashSet, VecDeque};

use anyhow::{anyhow, Result};
use ndarray::Array2;

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

fn parse_contents(contents: &str) -> Result<Array2<char>> {
    let contents = contents
        .lines()
        .map(|l| l.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let nrows = contents.len();
    let ncols = contents[0].len();

    let contents = contents.iter().flatten().copied().collect();

    Ok(Array2::from_shape_vec((nrows, ncols), contents)?)
}

fn bfs(matrix: &Array2<char>, origin: (usize, usize), destination: (usize, usize)) -> usize {
    let mut queue = VecDeque::new();

    let nrows = matrix.nrows();
    let ncols = matrix.ncols();

    let mut visited = HashSet::new();
    visited.insert(origin);
    queue.push_front((origin, visited));

    let mut greatest_path_length = 0;

    while let Some(((i, j), p)) = queue.pop_back() {
        if i > 0
            && !p.contains(&(i - 1, j))
            && !(matches!(matrix[(i, j)], '>' | 'v' | '<') || matrix[(i - 1, j)] == '#')
        {
            if (i - 1, j) == destination {
                greatest_path_length = greatest_path_length.max(p.len());
            } else {
                let mut p = p.clone();
                p.insert((i - 1, j));
                queue.push_front(((i - 1, j), p));
            }
        }
        if i < nrows - 1
            && !p.contains(&(i + 1, j))
            && !(matches!(matrix[(i, j)], '>' | '^' | '<') || matrix[(i + 1, j)] == '#')
        {
            if (i + 1, j) == destination {
                greatest_path_length = greatest_path_length.max(p.len());
            } else {
                let mut p = p.clone();
                p.insert((i + 1, j));
                queue.push_front(((i + 1, j), p));
            }
        }
        if j > 0
            && !p.contains(&(i, j - 1))
            && !(matches!(matrix[(i, j)], '^' | '>' | 'v') || matrix[(i, j - 1)] == '#')
        {
            if (i, j - 1) == destination {
                greatest_path_length = greatest_path_length.max(p.len());
            } else {
                let mut p = p.clone();
                p.insert((i, j - 1));
                queue.push_front(((i, j - 1), p));
            }
        }
        if j < ncols - 1
            && !p.contains(&(i, j + 1))
            && !(matches!(matrix[(i, j)], '^' | '<' | 'v') || matrix[(i, j + 1)] == '#')
        {
            if (i, j + 1) == destination {
                greatest_path_length = greatest_path_length.max(p.len());
            } else {
                let mut p = p.clone();
                p.insert((i, j + 1));
                queue.push_front(((i, j + 1), p));
            }
        }
    }

    greatest_path_length
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let matrix = parse_contents(contents)?;
    let nrows = matrix.nrows();
    let ncols = matrix.ncols();

    Ok(bfs(&matrix, (0, 1), (nrows - 1, ncols - 2)))
}

fn main() -> Result<()> {
    println!("{:?}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 94);
    }
}

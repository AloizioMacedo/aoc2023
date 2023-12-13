use anyhow::{anyhow, Result};
use ndarray::Array2;

const TEST_INPUT: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

fn parse_contents(contents: &str) -> Result<Vec<Array2<char>>> {
    let blocks = contents.split("\n\n");

    let mut matrices = Vec::new();
    for block in blocks {
        let nrows = block.lines().count();
        let ncols = block
            .split_once('\n')
            .ok_or(anyhow!("Syntax error"))?
            .0
            .len();
        let block = block.lines().flat_map(|l| l.chars()).collect();

        let matrix = Array2::from_shape_vec((nrows, ncols), block)?;
        matrices.push(matrix);
    }

    Ok(matrices)
}

fn is_reflection_across_col(matrix: &Array2<char>, j: usize) -> bool {
    let max_delta = j.min(matrix.ncols() - 1 - (j + 1));

    for k in 0..=max_delta {
        for i in 0..matrix.nrows() {
            if matrix[(i, j - k)] != matrix[(i, j + 1 + k)] {
                return false;
            }
        }
    }

    true
}

fn is_reflection_across_row(matrix: &Array2<char>, i: usize) -> bool {
    let max_delta = i.min(matrix.nrows() - 1 - (i + 1));

    for k in 0..=max_delta {
        for j in 0..matrix.ncols() {
            if matrix[(i - k, j)] != matrix[(i + 1 + k, j)] {
                return false;
            }
        }
    }

    true
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let matrices = parse_contents(contents)?;

    let mut total = 0;

    for matrix in matrices {
        for j in 0..(matrix.ncols() - 1) {
            if is_reflection_across_col(&matrix, j) {
                total += j + 1;
            }
        }

        for i in 0..(matrix.nrows() - 1) {
            if is_reflection_across_row(&matrix, i) {
                total += 100 * (i + 1);
            }
        }
    }

    Ok(total)
}

fn solve_part_two() {}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT: &str = include_str!("../test_input.txt");

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 405);
    }
}

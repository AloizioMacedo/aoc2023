use anyhow::{anyhow, Result};
use ndarray::Array2;

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

enum Reflection {
    Row(usize),
    Column(usize),
}

fn get_rows_and_columns_from_part_one(contents: &str) -> Result<Vec<Reflection>> {
    let matrices = parse_contents(contents)?;

    let mut reflections = Vec::new();
    for matrix in matrices {
        for j in 0..(matrix.ncols() - 1) {
            if is_reflection_across_col(&matrix, j) {
                reflections.push(Reflection::Column(j));
            }
        }

        for i in 0..(matrix.nrows() - 1) {
            if is_reflection_across_row(&matrix, i) {
                reflections.push(Reflection::Row(i));
            }
        }
    }

    Ok(reflections)
}

fn flip(matrix: &Array2<char>, i: usize, j: usize) -> Array2<char> {
    let mut matrix = matrix.clone();

    match matrix[(i, j)] {
        '#' => matrix[(i, j)] = '.',
        '.' => matrix[(i, j)] = '#',
        _ => panic!("Flipping a matrix that is ill-formed to the problem"),
    }

    matrix
}

fn solve_part_two(contents: &str) -> Result<usize> {
    let matrices = parse_contents(contents)?;
    let rows_and_cols = get_rows_and_columns_from_part_one(contents)?;

    let mut total = 0;

    'outer: for (matrix, row_or_col) in matrices.iter().zip(rows_and_cols) {
        for i1 in 0..matrix.nrows() {
            for j1 in 0..matrix.ncols() {
                let matrix = flip(matrix, i1, j1);

                for j in 0..(matrix.ncols() - 1) {
                    if let Reflection::Column(c) = row_or_col {
                        if c == j {
                            continue;
                        }
                    }

                    if is_reflection_across_col(&matrix, j) {
                        total += j + 1;
                        continue 'outer;
                    }
                }

                for i in 0..(matrix.nrows() - 1) {
                    if let Reflection::Row(r) = row_or_col {
                        if r == i {
                            continue;
                        }
                    }

                    if is_reflection_across_row(&matrix, i) {
                        total += 100 * (i + 1);
                        continue 'outer;
                    }
                }
            }
        }
    }

    Ok(total)
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT)?);

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

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT).unwrap(), 400);
    }
}

use anyhow::{anyhow, Result};
use ndarray::{Array2, Axis};

const INPUT: &str = include_str!("../input.txt");
const TEST_INPUT: &str = include_str!("../test_input.txt");

fn parse_contents(contents: &str) -> Result<Array2<char>> {
    let contents: Vec<Vec<char>> = contents.lines().map(|x| x.chars().collect()).collect();

    let nrows = contents.len();
    let ncols = contents[0].len();

    let contents: Vec<char> = contents.into_iter().flatten().collect();

    Ok(Array2::from_shape_vec((nrows, ncols), contents)?)
}

fn tilt_north(matrix: &mut Array2<char>) {
    for mut column in matrix.axis_iter_mut(Axis(1)) {
        for height in 0..column.len() {
            if column[height] != 'O' || height == 0 {
                continue;
            }

            let mut going_down_idx = (height - 1) as i32;

            while let Some('.') = column.get(going_down_idx as usize) {
                going_down_idx -= 1
            }

            column[height] = '.';
            column[(going_down_idx + 1) as usize] = 'O';
        }
    }
}

fn calculate_load(matrix: &Array2<char>) -> usize {
    let ncols = matrix.ncols();
    matrix
        .indexed_iter()
        .flat_map(|((i, j), &v)| if v == 'O' { Some(ncols - i) } else { None })
        .sum()
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let mut matrix = parse_contents(contents)?;
    tilt_north(&mut matrix);

    Ok(calculate_load(&matrix))
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 136);
    }
}

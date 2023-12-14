use std::collections::HashMap;

use anyhow::Result;
use ndarray::{Array2, Axis};

const INPUT: &str = include_str!("../input.txt");

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

            let mut going_up_idx = (height - 1) as i32;

            while let Some('.') = column.get(going_up_idx as usize) {
                going_up_idx -= 1
            }

            column[height] = '.';
            column[(going_up_idx + 1) as usize] = 'O';
        }
    }
}

fn tilt_south(matrix: &mut Array2<char>) {
    for mut column in matrix.axis_iter_mut(Axis(1)) {
        for height in (0..column.len()).rev() {
            if column[height] != 'O' || height == column.len() - 1 {
                continue;
            }

            let mut going_down_idx = (height + 1) as i32;

            while let Some('.') = column.get(going_down_idx as usize) {
                going_down_idx += 1
            }

            column[height] = '.';
            column[(going_down_idx - 1) as usize] = 'O';
        }
    }
}

fn tilt_east(matrix: &mut Array2<char>) {
    for mut row in matrix.axis_iter_mut(Axis(0)) {
        for col in (0..row.len()).rev() {
            if row[col] != 'O' || col == row.len() - 1 {
                continue;
            }

            let mut going_east_idx = (col + 1) as i32;

            while let Some('.') = row.get(going_east_idx as usize) {
                going_east_idx += 1
            }

            row[col] = '.';
            row[(going_east_idx - 1) as usize] = 'O';
        }
    }
}

fn tilt_west(matrix: &mut Array2<char>) {
    for mut row in matrix.axis_iter_mut(Axis(0)) {
        for col in 0..row.len() {
            if row[col] != 'O' || col == 0 {
                continue;
            }

            let mut going_up_idx = (col - 1) as i32;

            while let Some('.') = row.get(going_up_idx as usize) {
                going_up_idx -= 1
            }

            row[col] = '.';
            row[(going_up_idx + 1) as usize] = 'O';
        }
    }
}

fn calculate_load(matrix: &Array2<char>) -> usize {
    let ncols = matrix.ncols();
    matrix
        .indexed_iter()
        .flat_map(|((i, _), &v)| if v == 'O' { Some(ncols - i) } else { None })
        .sum()
}

fn cycle(matrix: &mut Array2<char>) {
    tilt_north(matrix);
    tilt_west(matrix);
    tilt_south(matrix);
    tilt_east(matrix);
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let mut matrix = parse_contents(contents)?;
    tilt_north(&mut matrix);

    Ok(calculate_load(&matrix))
}

fn solve_part_two(contents: &str) -> Result<usize> {
    let mut matrix = parse_contents(contents)?;

    let mut cache: HashMap<Array2<char>, usize> = HashMap::new();
    cache.insert(matrix.clone(), 0);

    for i in 1..1_000_000_000 {
        cycle(&mut matrix);

        if let Some(last_point) = cache.insert(matrix.clone(), i) {
            let cycle_length = i - last_point;

            let i = ((1_000_000_000 - i) / cycle_length) * cycle_length + i;
            for _ in i..1_000_000_000 {
                cycle(&mut matrix);
            }

            break;
        }
    }

    Ok(calculate_load(&matrix))
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
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 136);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT).unwrap(), 64);
    }
}

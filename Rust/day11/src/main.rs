use anyhow::{anyhow, Result};
use ndarray::Array2;

const TEST_INPUT: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

fn parse_contents(contents: &str) -> Result<Array2<char>> {
    let lines: Vec<&str> = contents.lines().collect();

    let nrows = lines.len();
    let ncols = lines[0].len();

    let chars: Vec<char> = contents.lines().flat_map(|l| l.chars()).collect();

    Ok(Array2::from_shape_vec((nrows, ncols), chars)?)
}

fn expand(universe: Array2<char>) -> Array2<char> {
    let mut rows_to_expand: Vec<usize> = Vec::new();

    for i in 0..universe.nrows() {
        if (0..universe.ncols())
            .map(|j| universe[(i, j)])
            .all(|x| x == '.')
        {
            rows_to_expand.push(i)
        }
    }

    let mut cols_to_expand: Vec<usize> = Vec::new();

    for j in 0..universe.ncols() {
        if (0..universe.nrows())
            .map(|i| universe[(i, j)])
            .all(|x| x == '.')
        {
            cols_to_expand.push(j)
        }
    }

    let new_nrows = universe.nrows() + rows_to_expand.len();
    let new_ncols = universe.ncols() + cols_to_expand.len();

    let mut new_array = Array2::from_elem((new_nrows, new_ncols), '.');

    let mut i_step_counter = 0;
    for i in 0..universe.nrows() {
        let mut j_step_counter = 0;

        if rows_to_expand.contains(&i) {
            i_step_counter += 1
        }

        for j in 0..universe.ncols() {
            if cols_to_expand.contains(&j) {
                j_step_counter += 1
            }

            new_array[(i + i_step_counter, j + j_step_counter)] = universe[(i, j)]
        }
    }

    new_array
}

fn expand_big(universe: Array2<char>, factor_of_expansion: usize) -> Array2<char> {
    let mut rows_to_expand: Vec<usize> = Vec::new();

    for i in 0..universe.nrows() {
        if (0..universe.ncols())
            .map(|j| universe[(i, j)])
            .all(|x| x == '.')
        {
            rows_to_expand.push(i)
        }
    }

    let mut cols_to_expand: Vec<usize> = Vec::new();

    for j in 0..universe.ncols() {
        if (0..universe.nrows())
            .map(|i| universe[(i, j)])
            .all(|x| x == '.')
        {
            cols_to_expand.push(j)
        }
    }

    let new_nrows = universe.nrows() + (factor_of_expansion - 1) * rows_to_expand.len();
    let new_ncols = universe.ncols() + (factor_of_expansion - 1) * cols_to_expand.len();

    let mut new_array = Array2::from_elem((new_nrows, new_ncols), '.');

    let mut i_step_counter = 0;
    for i in 0..universe.nrows() {
        let mut j_step_counter = 0;

        if rows_to_expand.contains(&i) {
            i_step_counter += factor_of_expansion - 1
        }

        for j in 0..universe.ncols() {
            if cols_to_expand.contains(&j) {
                j_step_counter += factor_of_expansion - 1
            }

            new_array[(i + i_step_counter, j + j_step_counter)] = universe[(i, j)]
        }
    }

    new_array
}
fn solve_part_one(contents: &str) -> Result<i32> {
    let universe = parse_contents(contents)?;
    let universe = expand(universe);

    let positions: Vec<(usize, usize)> = universe
        .indexed_iter()
        .filter(|((_, _), &v)| v == '#')
        .map(|(tup, _)| tup)
        .collect();

    let mut distances = Vec::new();

    for i in 0..positions.len() {
        for j in (i + 1)..positions.len() {
            let first_pos = positions[i];
            let second_pos = positions[j];

            let distance = (second_pos.1 as i32 - first_pos.1 as i32).abs()
                + (second_pos.0 as i32 - first_pos.0 as i32).abs();

            distances.push(distance);
        }
    }

    Ok(distances.iter().sum())
}

fn solve_part_two(contents: &str, factor_of_expansion: usize) -> Result<i64> {
    let universe = parse_contents(contents)?;
    let universe = expand_big(universe, factor_of_expansion);

    let positions: Vec<(usize, usize)> = universe
        .indexed_iter()
        .filter(|((_, _), &v)| v == '#')
        .map(|(tup, _)| tup)
        .collect();

    let mut distances = Vec::new();

    for i in 0..positions.len() {
        for j in (i + 1)..positions.len() {
            let first_pos = positions[i];
            let second_pos = positions[j];

            let distance = (second_pos.1 as i64 - first_pos.1 as i64).abs()
                + (second_pos.0 as i64 - first_pos.0 as i64).abs();

            distances.push(distance);
        }
    }

    Ok(distances.iter().sum())
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT, 1_000_000)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input.txt");

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 374);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST, 10).unwrap(), 1030);
        assert_eq!(solve_part_two(TEST, 100).unwrap(), 8410);
    }
}

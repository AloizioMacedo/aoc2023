use anyhow::Result;
use ndarray::Array2;

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

    let mut new_array = Array2::from_elem((universe.nrows(), universe.ncols()), 'E');

    for i in 0..universe.nrows() {
        if rows_to_expand.contains(&i) {
            continue;
        }

        for j in 0..universe.ncols() {
            if cols_to_expand.contains(&j) {
                continue;
            }

            new_array[(i, j)] = universe[(i, j)]
        }
    }

    new_array
}

fn solve_part_one(contents: &str) -> Result<usize> {
    solve_part_two(contents, 2)
}

fn solve_part_two(contents: &str, factor_of_expansion: usize) -> Result<usize> {
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

            let (first_hor, second_hor) = if first_pos.0 < second_pos.0 {
                (first_pos.0, second_pos.0)
            } else {
                (second_pos.0, first_pos.0)
            };

            let (first_ver, second_ver) = if first_pos.1 < second_pos.1 {
                (first_pos.1, second_pos.1)
            } else {
                (second_pos.1, first_pos.1)
            };

            let mut total = 0;

            for k in first_ver..second_ver {
                if universe[(first_hor, k)] == 'E' {
                    total += factor_of_expansion
                } else {
                    total += 1
                }
            }
            for k in first_hor..second_hor {
                if universe[(k, second_ver)] == 'E' {
                    total += factor_of_expansion
                } else {
                    total += 1
                }
            }

            distances.push(total);
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

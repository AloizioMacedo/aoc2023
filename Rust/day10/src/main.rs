use anyhow::Result;
use ndarray::Array2;

mod grid;
mod pipe;

const INPUT: &str = include_str!("../input.txt");

fn parse_contents(contents: &str) -> Result<grid::Grid> {
    let lines: Vec<&str> = contents.lines().collect();

    let n_rows = lines.len();
    let n_cols = lines[0].len();

    let mut matrix: Array2<pipe::Pipe> = Array2::default((n_rows, n_cols));

    let mut origin = (0, 0);
    for (i, line) in lines.iter().enumerate() {
        for (j, c) in line.chars().enumerate() {
            matrix[(i, j)] = pipe::Pipe::try_from(c)?;

            if matches!(matrix[(i, j)], pipe::Pipe::Origin) {
                origin = (i, j);
            }
        }
    }

    Ok(grid::Grid { origin, matrix })
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let grid = parse_contents(contents)?;
    let lp = pipe::find_loop(&grid.get_graph(), grid.origin)?;

    Ok((lp.len() - 1) / 2) // Subtract one to remove the origin.
}

fn solve_part_two(contents: &str) -> Result<usize> {
    let grid = parse_contents(contents)?;
    let lp = pipe::find_loop(&grid.get_graph(), grid.origin)?;

    let scaled_up = grid::scale_up(&grid.matrix);
    let transformed_matrix = grid::transform_matrix(&scaled_up, &lp);

    let filled = grid::flood_fill(&transformed_matrix);
    let scaled_down = grid::scale_down(&filled);

    Ok(scaled_down
        .iter()
        .filter(|e| matches!(e, grid::Filled::Empty))
        .count())
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input_p1.txt");

    #[test]
    fn it_works() {
        assert_eq!(solve_part_one(TEST).unwrap(), 8)
    }
}

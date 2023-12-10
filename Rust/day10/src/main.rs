use anyhow::{anyhow, Result};
use petgraph::{
    algo::{self},
    graph,
    graphmap::UnGraphMap,
};

const INPUT: &str = include_str!("../input.txt");
const TEST: &str = include_str!("../test_input.txt");

#[derive(Debug)]
enum Pipe {
    Vertical,
    Horizontal,
    NE,
    NW,
    SW,
    SE,
    Origin,
}

impl TryFrom<char> for Pipe {
    type Error = anyhow::Error;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '|' => Ok(Self::Vertical),
            '-' => Ok(Self::Horizontal),
            'L' => Ok(Self::NE),
            'J' => Ok(Self::NW),
            '7' => Ok(Self::SW),
            'F' => Ok(Self::SE),
            'S' => Ok(Self::Origin),
            _ => Err(anyhow!("Invalid pipe")),
        }
    }
}

#[derive(Debug)]
struct Grid {
    origin: (usize, usize),
    graph: UnGraphMap<(usize, usize), ()>,
}

fn parse_contents(contents: &str) -> Result<Grid> {
    let mut graph: UnGraphMap<(usize, usize), ()> = UnGraphMap::new();

    let lines: Vec<&str> = contents.lines().collect();

    let n_rows = lines.len();
    let n_cols = lines[0].len();

    let mut origin = (0, 0);

    for (i, line) in lines.iter().enumerate() {
        for (j, c) in line.chars().enumerate() {
            if let Ok(pipe) = Pipe::try_from(c) {
                let i = i as i32;
                let j = j as i32;

                let connected_coords = match pipe {
                    Pipe::Vertical => vec![(i - 1, j), (i + 1, j)],
                    Pipe::Horizontal => vec![(i, j - 1), (i, j + 1)],
                    Pipe::NE => vec![(i, j + 1), (i - 1, j)],
                    Pipe::NW => vec![(i, j - 1), (i - 1, j)],
                    Pipe::SW => vec![(i + 1, j), (i, j - 1)],
                    Pipe::SE => vec![(i + 1, j), (i, j + 1)],
                    Pipe::Origin => vec![(i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1)],
                };

                if matches!(pipe, Pipe::Origin) {
                    origin = (i as usize, j as usize);
                }

                for (i1, j1) in connected_coords {
                    if (i1 < n_rows as i32) && (j1 < n_cols as i32) {
                        graph.add_edge((i as usize, j as usize), (i1 as usize, j1 as usize), ());
                    }
                }
            }
        }
    }

    Ok(Grid { origin, graph })
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let grid = parse_contents(contents)?;
    println!("{:?}", grid);

    let mut bfs = petgraph::visit::Bfs::new(&grid.graph, grid.origin);

    // while let Some(nx) = bfs.next(&grid.graph) {
    //   if nx == grid.origin {
    //        break;
    //    }
    //}

    let main_loop: Vec<(usize, usize)> =
        algo::all_simple_paths(&grid.graph, grid.origin, grid.origin, 2, None)
            .next()
            .ok_or(anyhow!("no simple path found"))?;

    Ok(main_loop.len() / 2)
}

fn main() -> Result<()> {
    println!("{:?}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input.txt");

    #[test]
    fn it_works() {
        assert_eq!(solve_part_one(TEST).unwrap(), 8)
    }
}

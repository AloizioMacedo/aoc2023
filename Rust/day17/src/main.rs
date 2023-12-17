use std::collections::{BinaryHeap, HashMap, HashSet};

use anyhow::{anyhow, Result};
use ndarray::Array2;

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn next_directions(&self, steps: usize) -> Vec<(Direction, usize)> {
        match self {
            Self::Up => {
                if steps >= 3 {
                    vec![(Self::Right, 1), (Self::Left, 1)]
                } else {
                    vec![(Self::Up, steps + 1), (Self::Right, 1), (Self::Left, 1)]
                }
            }
            Self::Right => {
                if steps >= 3 {
                    vec![(Self::Up, 1), (Self::Down, 1)]
                } else {
                    vec![(Self::Up, 1), (Self::Right, steps + 1), (Self::Down, 1)]
                }
            }
            Self::Down => {
                if steps >= 3 {
                    vec![(Self::Right, 1), (Self::Left, 1)]
                } else {
                    vec![(Self::Right, 1), (Self::Down, steps + 1), (Self::Left, 1)]
                }
            }
            Self::Left => {
                if steps >= 3 {
                    vec![(Self::Down, 1), (Self::Up, 1)]
                } else {
                    vec![(Self::Down, 1), (Self::Left, steps + 1), (Self::Up, 1)]
                }
            }
        }
    }

    fn next_directions_ultra(&self, steps: usize) -> Vec<(Direction, usize)> {
        match self {
            Self::Up => {
                if steps >= 10 {
                    vec![(Self::Right, 1), (Self::Left, 1)]
                } else if steps < 4 {
                    vec![(Self::Up, steps + 1)]
                } else {
                    vec![(Self::Up, steps + 1), (Self::Right, 1), (Self::Left, 1)]
                }
            }
            Self::Right => {
                if steps >= 10 {
                    vec![(Self::Up, 1), (Self::Down, 1)]
                } else if steps < 4 {
                    vec![(Self::Right, steps + 1)]
                } else {
                    vec![(Self::Up, 1), (Self::Right, steps + 1), (Self::Down, 1)]
                }
            }
            Self::Down => {
                if steps >= 10 {
                    vec![(Self::Right, 1), (Self::Left, 1)]
                } else if steps < 4 {
                    vec![(Self::Down, steps + 1)]
                } else {
                    vec![(Self::Right, 1), (Self::Down, steps + 1), (Self::Left, 1)]
                }
            }
            Self::Left => {
                if steps >= 10 {
                    vec![(Self::Down, 1), (Self::Up, 1)]
                } else if steps < 4 {
                    vec![(Self::Left, steps + 1)]
                } else {
                    vec![(Self::Down, 1), (Self::Left, steps + 1), (Self::Up, 1)]
                }
            }
        }
    }
}

fn parse_contents(contents: &str) -> Result<Array2<u32>> {
    let lines: Vec<&str> = contents.lines().collect();

    let nrows = lines.len();
    let ncols = lines[0].len();

    let contents: Vec<u32> = lines
        .join("")
        .chars()
        .map(|n| n.to_digit(10))
        .collect::<Option<Vec<_>>>()
        .ok_or(anyhow!("Not a number"))?;

    Ok(Array2::from_shape_vec((nrows, ncols), contents)?)
}

fn get_distance(matrix: &Array2<u32>) -> Result<i32> {
    let mut distances: HashMap<VisitedWithSteps, i32> = HashMap::new();
    for i in 0..matrix.nrows() {
        for j in 0..matrix.ncols() {
            for k in 0..4 {
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps: k,
                        direction: Direction::Up,
                    },
                    i32::MAX,
                );
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps: k,
                        direction: Direction::Right,
                    },
                    i32::MAX,
                );
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps: k,
                        direction: Direction::Down,
                    },
                    i32::MAX,
                );
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps: k,
                        direction: Direction::Left,
                    },
                    i32::MAX,
                );
            }
        }
    }

    distances.insert(
        VisitedWithSteps {
            node: (0, 0),
            steps: 0,
            direction: Direction::Right,
        },
        0,
    );

    let mut queue: BinaryHeap<(i32, VisitedWithSteps)> = BinaryHeap::new();
    queue.push((
        0,
        VisitedWithSteps {
            node: (0, 0),
            steps: 0,
            direction: Direction::Right,
        },
    ));

    let mut visited = HashSet::new();

    while let Some((_, current)) = queue.pop() {
        if !visited.insert(current) {
            continue;
        };
        let next_directions = current.direction.next_directions(current.steps);

        for (direction, steps) in next_directions {
            let coords = match (direction, steps) {
                (Direction::Up, _) => {
                    if current.node.0 > 0 {
                        let i = current.node.0 - 1;
                        let j = current.node.1;
                        Some((i, j))
                    } else {
                        None
                    }
                }
                (Direction::Right, _) => {
                    if current.node.1 < matrix.ncols() - 1 {
                        let i = current.node.0;
                        let j = current.node.1 + 1;
                        Some((i, j))
                    } else {
                        None
                    }
                }
                (Direction::Down, _) => {
                    if current.node.0 < matrix.nrows() - 1 {
                        let i = current.node.0 + 1;
                        let j = current.node.1;
                        Some((i, j))
                    } else {
                        None
                    }
                }
                (Direction::Left, _) => {
                    if current.node.1 > 0 {
                        let i = current.node.0;
                        let j = current.node.1 - 1;
                        Some((i, j))
                    } else {
                        None
                    }
                }
            };

            if let Some((i, j)) = coords {
                let distance = distances[&VisitedWithSteps {
                    node: (i, j),
                    steps,
                    direction,
                }]
                    .min(distances[&current] + matrix[(i, j)] as i32);
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps,
                        direction,
                    },
                    distance,
                );
                queue.push((
                    -distance,
                    VisitedWithSteps {
                        node: (i, j),
                        steps,
                        direction,
                    },
                ));
            }
        }
    }

    distances
        .iter()
        .filter(|(k, _)| k.node == (matrix.nrows() - 1, matrix.ncols() - 1))
        .map(|(_, v)| *v)
        .min()
        .ok_or(anyhow!("Not able to reach destination"))
}

fn get_distance_ultra(matrix: &Array2<u32>) -> Result<i32> {
    let mut distances: HashMap<VisitedWithSteps, i32> = HashMap::new();
    for i in 0..matrix.nrows() {
        for j in 0..matrix.ncols() {
            for k in 0..11 {
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps: k,
                        direction: Direction::Up,
                    },
                    i32::MAX,
                );
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps: k,
                        direction: Direction::Right,
                    },
                    i32::MAX,
                );
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps: k,
                        direction: Direction::Down,
                    },
                    i32::MAX,
                );
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps: k,
                        direction: Direction::Left,
                    },
                    i32::MAX,
                );
            }
        }
    }

    distances.insert(
        VisitedWithSteps {
            node: (0, 0),
            steps: 0,
            direction: Direction::Right,
        },
        0,
    );
    distances.insert(
        VisitedWithSteps {
            node: (0, 0),
            steps: 0,
            direction: Direction::Down,
        },
        0,
    );

    let mut queue: BinaryHeap<(i32, VisitedWithSteps)> = BinaryHeap::new();
    queue.push((
        0,
        VisitedWithSteps {
            node: (0, 0),
            steps: 0,
            direction: Direction::Right,
        },
    ));
    queue.push((
        0,
        VisitedWithSteps {
            node: (0, 0),
            steps: 0,
            direction: Direction::Down,
        },
    ));

    let mut visited = HashSet::new();

    while let Some((_, current)) = queue.pop() {
        if !visited.insert(current) {
            continue;
        };
        let next_directions = current.direction.next_directions_ultra(current.steps);

        for (direction, steps) in next_directions {
            let coords = match (direction, steps) {
                (Direction::Up, _) => {
                    if current.node.0 > 0 {
                        let i = current.node.0 - 1;
                        let j = current.node.1;
                        Some((i, j))
                    } else {
                        None
                    }
                }
                (Direction::Right, _) => {
                    if current.node.1 < matrix.ncols() - 1 {
                        let i = current.node.0;
                        let j = current.node.1 + 1;
                        Some((i, j))
                    } else {
                        None
                    }
                }
                (Direction::Down, _) => {
                    if current.node.0 < matrix.nrows() - 1 {
                        let i = current.node.0 + 1;
                        let j = current.node.1;
                        Some((i, j))
                    } else {
                        None
                    }
                }
                (Direction::Left, _) => {
                    if current.node.1 > 0 {
                        let i = current.node.0;
                        let j = current.node.1 - 1;
                        Some((i, j))
                    } else {
                        None
                    }
                }
            };

            if let Some((i, j)) = coords {
                let distance = distances[&VisitedWithSteps {
                    node: (i, j),
                    steps,
                    direction,
                }]
                    .min(distances[&current] + matrix[(i, j)] as i32);
                distances.insert(
                    VisitedWithSteps {
                        node: (i, j),
                        steps,
                        direction,
                    },
                    distance,
                );
                queue.push((
                    -distance,
                    VisitedWithSteps {
                        node: (i, j),
                        steps,
                        direction,
                    },
                ));
            }
        }
    }

    distances
        .iter()
        .filter(|(k, _)| k.node == (matrix.nrows() - 1, matrix.ncols() - 1) && k.steps >= 4)
        .map(|(_, v)| *v)
        .min()
        .ok_or(anyhow!("Not able to reach destination"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct VisitedWithSteps {
    node: (usize, usize),
    steps: usize,
    direction: Direction,
}

fn solve_part_one(contents: &str) -> Result<i32> {
    let matrix = parse_contents(contents)?;

    get_distance(&matrix)
}

fn solve_part_two(contents: &str) -> Result<i32> {
    let matrix = parse_contents(contents)?;

    get_distance_ultra(&matrix)
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 102);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST).unwrap(), 94);
    }

    #[test]
    fn part_two_simple() {
        let test = "111111111111\n999999999991\n999999999991\n999999999991\n999999999991";
        assert_eq!(solve_part_two(test).unwrap(), 71);
    }
}

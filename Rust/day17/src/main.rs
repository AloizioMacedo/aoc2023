use std::collections::{BinaryHeap, HashMap, HashSet};

use anyhow::{anyhow, Result};
use ndarray::Array2;

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
}

fn parse_line(line: &str) {}

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

fn get_distance(matrix: &Array2<u32>) -> usize {
    let destination = (matrix.nrows() - 1, matrix.ncols() - 1);

    let mut distances: HashMap<(usize, usize), u32> =
        HashMap::from_iter(matrix.indexed_iter().map(|((i, j), _)| ((i, j), u32::MAX)));
    distances.insert((0, 0), 0);

    // let mut distances: HashMap<VisitedWithSteps, u32> = HashMap::new();
    // for i in 0..matrix.nrows() {
    //     for j in 0..matrix.ncols() {
    //         for k in 0..4 {
    //             distances.insert(
    //                 VisitedWithSteps {
    //                     node: (i, j),
    //                     steps: k,
    //                     direction: Direction::Up,
    //                     distance: u32::MAX,
    //                 },
    //                 u32::MAX,
    //             )
    //         }
    //         distances.insert(
    //             VisitedWithSteps {
    //                 node: (i, j),
    //                 steps: 0,
    //                 direction: Direction::Right,
    //                 distance: u32::MAX,
    //             },
    //             u32::MAX,
    //         );
    //     }
    // }

    // distances.insert(
    //     VisitedWithSteps {
    //         node: (0, 0),
    //         steps: 0,
    //         direction: Direction::Right,
    //         distance: 0,
    //     },
    //     0,
    // );

    let mut queue: BinaryHeap<VisitedWithSteps> = BinaryHeap::new();
    queue.push(VisitedWithSteps {
        node: (0, 0),
        steps: 0,
        direction: Direction::Right,
        distance: 0,
    });

    let mut visited = HashSet::new();

    while let Some(current) = queue.pop() {
        if !visited.insert(current) {
            continue;
        };
        let next_directions = current.direction.next_directions(current.steps);

        for (direction, steps) in next_directions {
            match (direction, steps) {
                (Direction::Up, steps) => {
                    if current.node.0 > 0 {
                        let i = current.node.0 - 1;
                        let j = current.node.1;

                        let distance = distances[&(i, j)].min(current.distance + matrix[(i, j)]);
                        distances.insert((i, j), distance);
                        queue.push(VisitedWithSteps {
                            node: (i, j),
                            steps,
                            direction,
                            distance,
                        });
                    }
                }
                (Direction::Right, steps) => {
                    if current.node.1 < matrix.ncols() - 1 {
                        let i = current.node.0;
                        let j = current.node.1 + 1;

                        let distance = distances[&(i, j)].min(current.distance + matrix[(i, j)]);
                        distances.insert((i, j), distance);
                        queue.push(VisitedWithSteps {
                            node: (i, j),
                            steps,
                            direction,
                            distance,
                        });
                    }
                }
                (Direction::Down, steps) => {
                    if current.node.0 < matrix.nrows() - 1 {
                        let i = current.node.0 + 1;
                        let j = current.node.1;

                        let distance = distances[&(i, j)].min(current.distance + matrix[(i, j)]);
                        distances.insert((i, j), distance);
                        queue.push(VisitedWithSteps {
                            node: (i, j),
                            steps,
                            direction,
                            distance,
                        });
                    }
                }
                (Direction::Left, steps) => {
                    if current.node.1 > 0 {
                        let i = current.node.0;
                        let j = current.node.1 - 1;

                        let distance = distances[&(i, j)].min(current.distance + matrix[(i, j)]);
                        distances.insert((i, j), distance);
                        queue.push(VisitedWithSteps {
                            node: (i, j),
                            steps,
                            direction,
                            distance,
                        });
                    }
                }
            }
        }
        if distances[&destination] < u32::MAX {
            break;
        }
    }

    distances[&destination] as usize
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct VisitedWithSteps {
    node: (usize, usize),
    steps: usize,
    direction: Direction,
    distance: u32,
}

impl PartialOrd for VisitedWithSteps {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for VisitedWithSteps {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.distance.cmp(&self.distance)
    }
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let matrix = parse_contents(contents)?;

    let distance = get_distance(&matrix);

    Ok(distance)
}

fn solve_part_two(contents: &str) {}

fn main() -> Result<()> {
    println!("{}", solve_part_one(TEST)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        println!("{}", solve_part_one(TEST).unwrap());
    }

    #[test]
    fn inequality() {
        assert!(
            VisitedWithSteps {
                node: (0, 0),
                steps: 3,
                direction: Direction::Right,
                distance: 5
            } > VisitedWithSteps {
                node: (0, 0),
                steps: 3,
                direction: Direction::Right,
                distance: 12
            }
        );
    }
}

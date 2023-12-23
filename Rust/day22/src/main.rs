use std::{cmp::Ordering, collections::HashSet, fmt::Debug};

use anyhow::{anyhow, Result};
use itertools::Itertools;
use std::hash::Hash;

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

#[derive(Clone, Copy, Eq)]
struct Brick {
    a: Point,
    b: Point,
}

impl PartialEq for Brick {
    fn eq(&self, other: &Self) -> bool {
        (self.a == other.a && self.b == other.b) || (self.a == other.b && self.b == other.a)
    }
}

impl Debug for Brick {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let a = self.a;
        let b = self.b;

        write!(
            f,
            "{{[{}, {}, {}] : [{}, {}, {}]}}",
            a.x, a.y, a.z, b.x, b.y, b.z
        )
    }
}

impl Brick {
    fn get_all(&self) -> Vec<Point> {
        match self.a.x.cmp(&self.b.x) {
            Ordering::Less => {
                return (self.a.x..=self.b.x)
                    .map(|x| Point {
                        x,
                        y: self.a.y,
                        z: self.a.z,
                    })
                    .collect()
            }
            Ordering::Greater => {
                return (self.b.x..=self.a.x)
                    .map(|x| Point {
                        x,
                        y: self.a.y,
                        z: self.a.z,
                    })
                    .collect()
            }
            Ordering::Equal => (),
        }

        match self.a.y.cmp(&self.b.y) {
            Ordering::Less => {
                return (self.a.y..=self.b.y)
                    .map(|y| Point {
                        x: self.a.x,
                        y,
                        z: self.a.z,
                    })
                    .collect()
            }
            Ordering::Greater => {
                return (self.b.y..=self.a.y)
                    .map(|y| Point {
                        x: self.a.x,
                        y,
                        z: self.a.z,
                    })
                    .collect()
            }
            Ordering::Equal => (),
        }

        match self.a.z.cmp(&self.b.z) {
            Ordering::Less => (self.a.z..=self.b.z)
                .map(|z| Point {
                    x: self.a.x,
                    y: self.a.y,
                    z,
                })
                .collect(),
            Ordering::Greater => (self.b.z..=self.a.z)
                .map(|z| Point {
                    x: self.a.x,
                    y: self.a.y,
                    z,
                })
                .collect(),
            Ordering::Equal => vec![Point {
                x: self.a.x,
                y: self.a.y,
                z: self.a.z,
            }],
        }
    }

    /// Will only project if the other brick is below
    fn get_projection_intersection(&self, other_brick: &Brick) -> Brick {
        let my_points = self.get_all();
        let other_points = other_brick.get_all();

        let mut highest_projection = 0;
        for Point {
            x: my_x,
            y: my_y,
            z: my_z,
        } in &my_points
        {
            for Point {
                x: other_x,
                y: other_y,
                z: other_z,
            } in &other_points
            {
                if other_z >= my_z {
                    continue;
                }

                if my_x == other_x && my_y == other_y && other_z > &highest_projection {
                    highest_projection = *other_z;
                }
            }
        }

        let Point {
            x: ax,
            y: ay,
            z: az,
        } = self.a;

        let Point {
            x: bx,
            y: by,
            z: bz,
        } = self.b;

        let a = Point {
            x: ax,
            y: ay,
            z: highest_projection + az.max(bz) - az + 1,
        };
        let b = Point {
            x: bx,
            y: by,
            z: highest_projection + az.max(bz) - bz + 1,
        };

        Brick { a, b }
    }

    fn get_lowest_height(&self) -> i32 {
        self.a.z.min(self.b.z)
    }

    fn get_highest_height(&self) -> i32 {
        self.a.z.max(self.b.z)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
}

fn parse_line(line: &str) -> Result<Brick> {
    let (point1, point2) = line.split_once('~').ok_or(anyhow!("Syntax error"))?;

    let (x1, y1, z1) = point1
        .splitn(3, ',')
        .collect_tuple()
        .ok_or(anyhow!("Syntax error"))?;

    let x1 = x1.parse()?;
    let y1 = y1.parse()?;
    let z1 = z1.parse()?;

    let (x2, y2, z2) = point2
        .splitn(3, ',')
        .collect_tuple()
        .ok_or(anyhow!("Syntax error"))?;

    let x2 = x2.parse()?;
    let y2 = y2.parse()?;
    let z2 = z2.parse()?;

    Ok(Brick {
        a: Point {
            x: x1,
            y: y1,
            z: z1,
        },
        b: Point {
            x: x2,
            y: y2,
            z: z2,
        },
    })
}

#[derive(Debug)]
struct Wall {
    bricks: Vec<Brick>,
}

impl Wall {
    fn get_lowest(&self) -> Vec<Brick> {
        let min = self.bricks.iter().map(|b| b.a.z.min(b.b.z)).min();

        if let Some(min) = min {
            self.bricks
                .iter()
                .copied()
                .filter(|b| b.a.z.min(b.b.z) == min)
                .collect()
        } else {
            vec![]
        }
    }

    fn drop(&mut self, brick: Brick) {
        let new_brick_after_dropping = self
            .bricks
            .iter()
            .map(|brick_in_wall| brick.get_projection_intersection(brick_in_wall))
            .max_by_key(|possible_brick_pos| possible_brick_pos.get_highest_height());

        if let Some(new_brick_after_dropping) = new_brick_after_dropping {
            self.bricks.push(new_brick_after_dropping);
        } else {
            let Brick {
                a: Point { x: ax, y: ay, .. },
                b: Point { x: bx, y: by, .. },
            } = brick;

            self.bricks.push(Brick {
                a: Point { x: ax, y: ay, z: 1 },
                b: Point { x: bx, y: by, z: 1 },
            })
        }
    }

    fn get_safe_count(&self) -> usize {
        let original_bricks = self.bricks.clone();

        let mut count = 0;
        for brick in &original_bricks {
            let mut bricks_without_this_brick = original_bricks
                .iter()
                .copied()
                .filter(|x| x != brick)
                .collect::<Vec<_>>();

            bricks_without_this_brick.sort_by_key(|b| b.get_lowest_height());

            let mut hypothetical_wall = Wall { bricks: Vec::new() };

            for &brick in &bricks_without_this_brick {
                hypothetical_wall.drop(brick);
            }

            if hypothetical_wall.bricks == bricks_without_this_brick {
                count += 1;
            }
        }

        count
    }

    fn get_drop_counts(&self) -> usize {
        let original_bricks = self.bricks.clone();

        let mut count = 0;
        for brick in &original_bricks {
            let mut bricks_without_this_brick = original_bricks
                .iter()
                .copied()
                .filter(|x| x != brick)
                .collect::<Vec<_>>();

            bricks_without_this_brick.sort_by_key(|b| b.get_lowest_height());

            let mut hypothetical_wall = Wall { bricks: Vec::new() };

            for &brick in &bricks_without_this_brick {
                hypothetical_wall.drop(brick);
            }

            let bricks1: HashSet<Brick> =
                HashSet::from_iter(hypothetical_wall.bricks.iter().copied());
            let bricks2: HashSet<Brick> = HashSet::from_iter(bricks_without_this_brick);

            println!("{}", count);
            count += bricks2.difference(&bricks1).count()
        }

        count
    }
}

fn parse_contents(contents: &str) -> Result<Wall> {
    let mut bricks_to_add: Vec<Brick> = contents
        .lines()
        .map(parse_line)
        .collect::<Result<Vec<_>>>()?;

    bricks_to_add.sort_by_key(|b| b.get_lowest_height());

    let mut wall = Wall { bricks: Vec::new() };

    bricks_to_add.into_iter().for_each(|b| wall.drop(b));

    Ok(wall)
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let wall = parse_contents(contents)?;

    Ok(wall.get_safe_count())
}

fn solve_part_two(contents: &str) -> Result<usize> {
    let wall = parse_contents(contents)?;

    Ok(wall.get_drop_counts())
}

fn main() -> Result<()> {
    println!("{:?}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 5);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST).unwrap(), 7);
    }
}

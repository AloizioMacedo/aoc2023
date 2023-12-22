use std::cmp::Ordering;

use anyhow::{anyhow, Result};
use itertools::Itertools;

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone, Copy)]
struct Brick {
    a: Point,
    b: Point,
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
            Ordering::Less => (self.a.y..=self.b.y)
                .map(|z| Point {
                    x: self.a.x,
                    y: self.a.y,
                    z,
                })
                .collect(),
            Ordering::Greater => (self.b.y..=self.a.y)
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
}

#[derive(Debug, Clone, Copy)]
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

    fn highest_below(&self, brick: &Brick) -> i32 {
        todo!()
    }
}

fn parse_contents(contents: &str) -> Result<Wall> {
    let bricks = contents.lines().map(parse_line).collect::<Result<_>>()?;

    Ok(Wall { bricks })
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}

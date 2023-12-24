mod hailstone;
mod linalg;

use anyhow::{anyhow, Result};
use hailstone::{get_intersection_at_xy, parse_line, Hailstone, Intersection, PositionGround};
use itertools::Itertools;

use crate::hailstone::{get_shift_velocity, is_point_in_past, Vector};

const INPUT: &str = include_str!("../input.txt");

fn parse_contents(contents: &str) -> Result<Vec<Hailstone>> {
    contents.lines().map(parse_line).collect()
}

fn solve_part_one(contents: &str, bound1: i64, bound2: i64) -> Result<usize> {
    let hailstones = parse_contents(contents)?;

    Ok(hailstones
        .iter()
        .combinations(2)
        .map(|v| (v[0], v[1]))
        .filter(|(&hailstone1, &hailstone2)| {
            if let Intersection::One((px, py, _)) = get_intersection_at_xy(hailstone1, hailstone2) {
                // println!("{:?}", (px, py));
                px >= bound1 as f64
                    && px <= bound2 as f64
                    && py >= bound1 as f64
                    && py <= bound2 as f64
                    && !(is_point_in_past(hailstone1, PositionGround { x: px, y: py }))
                    && !(is_point_in_past(hailstone2, PositionGround { x: px, y: py }))
            } else if let Intersection::Coincident = get_intersection_at_xy(hailstone1, hailstone2)
            {
                false // Seems to never happen
            } else {
                false
            }
        })
        .count())
}

fn solve_part_two(contents: &str) -> Result<f64> {
    let hailstones = parse_contents(contents)?;

    let (hailstone1, hailstone2, hailstone3) = (hailstones[0], hailstones[1], hailstones[2]);
    let velocity_to_shift = get_shift_velocity(hailstone1, hailstone2, hailstone3)?;

    let Hailstone {
        position: position1,
        velocity: Vector {
            x: vx1,
            y: vy1,
            z: vz1,
        },
    } = hailstone1;
    let Hailstone {
        position: position2,
        velocity: Vector {
            x: vx2,
            y: vy2,
            z: vz2,
        },
    } = hailstone2;

    let intersection = get_intersection_at_xy(
        Hailstone {
            position: position1,
            velocity: Vector {
                x: 100_000_000 * vx1 - velocity_to_shift[0],
                y: 100_000_000 * vy1 - velocity_to_shift[1],
                z: 100_000_000 * vz1 - velocity_to_shift[2],
            },
        },
        Hailstone {
            position: position2,
            velocity: Vector {
                x: 100_000_000 * vx2 - velocity_to_shift[0],
                y: 100_000_000 * vy2 - velocity_to_shift[1],
                z: 100_000_000 * vz2 - velocity_to_shift[2],
            },
        },
    );

    if let Intersection::One((x, y, z)) = intersection {
        Ok((x + y + z).round())
    } else {
        Err(anyhow!("No intersection found"))
    }
}

fn main() -> Result<()> {
    println!(
        "{}",
        solve_part_one(INPUT, 200000000000000, 400000000000000)?
    );
    println!("{}", solve_part_two(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input.txt");

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST, 7, 27).unwrap(), 2);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST).unwrap(), 47.0);
    }
}

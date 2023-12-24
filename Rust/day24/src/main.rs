mod hailstone;

use anyhow::{anyhow, Result};
use hailstone::{get_intersection_at_xy, parse_line, Hailstone, Intersection, PositionGround};
use itertools::Itertools;

use crate::hailstone::is_point_in_past;

const TEST: &str = include_str!("../test_input.txt");
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
            println!("{:?}", get_intersection_at_xy(hailstone1, hailstone2));
            if let Intersection::One((px, py)) = get_intersection_at_xy(hailstone1, hailstone2) {
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

fn main() -> Result<()> {
    println!(
        "{}",
        solve_part_one(INPUT, 200000000000000, 400000000000000)?
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST, 7, 27).unwrap(), 2);
    }
}

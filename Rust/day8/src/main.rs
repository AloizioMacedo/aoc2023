use std::collections::HashMap;

use anyhow::{anyhow, Result};
use itertools::Itertools;

const INPUT: &str = include_str!("../input.txt");

#[derive(Debug)]
enum Direction {
    L,
    R,
}

impl TryFrom<char> for Direction {
    type Error = anyhow::Error;

    fn try_from(value: char) -> Result<Direction> {
        match value {
            'L' => Ok(Direction::L),
            'R' => Ok(Direction::R),
            _ => Err(anyhow!("Invalid char {value}")),
        }
    }
}

#[derive(Debug)]
struct Possibilities<'a> {
    left: &'a str,
    right: &'a str,
}

fn parse_line(line: &str) -> Result<(&str, Possibilities)> {
    let (origin, destinations) = line
        .split_once(" = ")
        .ok_or(anyhow!("Syntax error at {line}"))?;

    let (left, right) = destinations
        .trim_end_matches(')')
        .trim_start_matches('(')
        .split_once(", ")
        .ok_or(anyhow!("Syntax error at {line}"))?;

    Ok((origin, Possibilities { left, right }))
}

fn parse_contents(contents: &str) -> Result<(Vec<Direction>, HashMap<&str, Possibilities>)> {
    let (first_line, rest) = contents.split_once("\n\n").ok_or(anyhow!("Syntax error"))?;

    let directions: Vec<Direction> = first_line.chars().flat_map(Direction::try_from).collect();
    let parsed_lines: Vec<(&str, Possibilities)> = rest.lines().flat_map(parse_line).collect();

    let transitions = HashMap::from_iter(parsed_lines);

    Ok((directions, transitions))
}

fn go_one_step<'a>(
    from: &'a str,
    direction: &Direction,
    map: &HashMap<&'a str, Possibilities<'a>>,
) -> &'a str {
    match direction {
        Direction::L => map[from].left,
        Direction::R => map[from].right,
    }
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let (directions, transitions) = parse_contents(contents)?;

    let mut current = "AAA";
    Ok(directions
        .iter()
        .cycle()
        .map(|d| {
            let next = go_one_step(current, d, &transitions);

            if next == "ZZZ" {
                None
            } else {
                current = next;
                Some(current)
            }
        })
        .while_some()
        .count()
        + 1)
}

fn solve_part_two(contents: &str) -> Result<usize> {
    let (directions, transitions) = parse_contents(contents)?;

    let subpaths: Vec<&str> = transitions
        .keys()
        .copied()
        .filter(|k| k.ends_with('A'))
        .collect();

    let values: Vec<usize> = subpaths
        .iter()
        .map(|s| {
            let mut current = *s;

            directions
                .iter()
                .cycle()
                .map(|d| {
                    let next = go_one_step(current, d, &transitions);

                    if next.ends_with('Z') {
                        None
                    } else {
                        current = next;
                        Some(current)
                    }
                })
                .while_some()
                .count()
                + 1
        })
        .collect();

    Ok(values.iter().fold(1, |acc, x| num::integer::lcm(acc, *x)))
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT: &str = include_str!("../test_input.txt");

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 6)
    }

    #[test]
    fn part_two() {
        let hey = r#"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"#;
        assert_eq!(solve_part_two(hey).unwrap(), 6)
    }
}

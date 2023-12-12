use std::num::ParseIntError;

use anyhow::{anyhow, Result};
use itertools::Itertools;

const INPUT: &str = include_str!("../input.txt");
const TEST_INPUT: &str = include_str!("../test_input.txt");

#[derive(Debug, Clone, Copy)]
enum Spring {
    Unknown,
    Damaged,
    Operational,
}

impl TryFrom<char> for Spring {
    type Error = anyhow::Error;

    fn try_from(value: char) -> Result<Self> {
        match value {
            '#' => Ok(Self::Damaged),
            '.' => Ok(Self::Operational),
            '?' => Ok(Self::Unknown),
            _ => Err(anyhow!("Invalid spring: {value}")),
        }
    }
}

#[derive(Debug)]
struct Row {
    springs: Vec<Spring>,
    groups: Vec<usize>,
}

impl Row {
    fn is_valid(&self, possibility: &[Spring]) -> bool {
        if !self.springs.iter().zip(possibility).all(|(x, y)| match x {
            Spring::Damaged => matches!(y, Spring::Damaged),
            Spring::Unknown => true,
            Spring::Operational => matches!(y, Spring::Operational),
        }) {
            return false;
        }

        let sums = possibility
            .iter()
            .group_by(|x| matches!(x, Spring::Damaged))
            .into_iter()
            .filter_map(|(k, g)| if k { Some(g.count()) } else { None })
            .collect::<Vec<_>>();

        sums == self.groups
    }
}

fn generate_possibilities(springs: &[Spring]) -> Vec<Vec<Spring>> {
    let first_unknown = springs.iter().position(|x| matches!(x, Spring::Unknown));

    let mut possibilities = Vec::new();
    if let Some(i) = first_unknown {
        let mut poss1 = springs.to_vec();
        let mut poss2 = springs.to_vec();

        poss1[i] = Spring::Damaged;
        poss2[i] = Spring::Operational;

        possibilities.extend(generate_possibilities(&poss1));
        possibilities.extend(generate_possibilities(&poss2));
    } else {
        possibilities.push(springs.to_vec())
    }

    possibilities
}

fn parse_line(line: &str) -> Result<Row> {
    let (springs, groups) = line.split_once(' ').ok_or(anyhow!("Syntax Error"))?;

    let springs = springs
        .chars()
        .map(Spring::try_from)
        .collect::<Result<Vec<_>>>()?;
    let groups = groups
        .split(',')
        .map(|x| x.parse())
        .collect::<Result<Vec<_>, ParseIntError>>()?;

    Ok(Row { springs, groups })
}

fn parse_line2(line: &str) -> Result<Row> {
    let (springs, groups) = line.split_once(' ').ok_or(anyhow!("Syntax Error"))?;
    let springs = [springs, springs, springs, springs, springs].join("?");
    let groups = [groups, groups, groups, groups, groups].join(",");

    let springs = springs
        .chars()
        .map(Spring::try_from)
        .collect::<Result<Vec<_>>>()?;
    let groups = groups
        .split(',')
        .map(|x| x.parse())
        .collect::<Result<Vec<_>, ParseIntError>>()?;

    Ok(Row { springs, groups })
}

fn parse_contents(contents: &str) -> Result<Vec<Row>> {
    contents.lines().map(parse_line).collect()
}

fn parse_contents_part2(contents: &str) -> Result<Vec<Row>> {
    contents.lines().map(parse_line2).collect()
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let rows = parse_contents(contents)?;

    let valid_arrangements = rows
        .iter()
        .map(|r| {
            generate_possibilities(&r.springs)
                .iter()
                .filter(|p| r.is_valid(p))
                .count()
        })
        .sum();

    Ok(valid_arrangements)
}

fn solve_part_two(contents: &str) -> Result<usize> {
    let rows = parse_contents_part2(contents)?;

    let valid_arrangements = rows
        .iter()
        .map(|r| {
            generate_possibilities(&r.springs)
                .iter()
                .filter(|p| r.is_valid(p))
                .count()
        })
        .sum();

    Ok(valid_arrangements)
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT: &str = include_str!("../test_input.txt");

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 21)
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT).unwrap(), 525152)
    }
}

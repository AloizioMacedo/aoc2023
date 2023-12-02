use std::{collections::HashMap, str::FromStr};

use anyhow::{anyhow, Result};

const INPUT: &str = include_str!("../input.txt");

const RED_CUBES: usize = 12;
const GREEN_CUBES: usize = 13;
const BLUE_CUBES: usize = 14;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
enum Color {
    Red,
    Green,
    Blue,
}

impl FromStr for Color {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "red" => Ok(Self::Red),
            "green" => Ok(Self::Green),
            "blue" => Ok(Self::Blue),
            _ => Err(anyhow!("Invalid color")),
        }
    }
}

impl Color {
    fn get_max(&self) -> usize {
        match self {
            Self::Red => RED_CUBES,
            Self::Green => GREEN_CUBES,
            Self::Blue => BLUE_CUBES,
        }
    }
}

#[derive(Debug)]
struct Set {
    cubes: HashMap<Color, usize>,
}

impl Set {
    fn is_possible(&self) -> bool {
        self.cubes.iter().all(|(k, v)| v <= &k.get_max())
    }

    fn get_power(&self) -> usize {
        self.cubes.values().product()
    }
}

#[derive(Debug)]
struct Game<'a> {
    id: &'a str,
    sets: Vec<Set>,
}

impl<'a> Game<'a> {
    fn is_possible(&self) -> bool {
        self.sets.iter().all(|set| set.is_possible())
    }

    fn get_max_of_each_set(&self) -> Set {
        let mut result = HashMap::new();
        let max_red = self
            .sets
            .iter()
            .map(|set| set.cubes.get(&Color::Red).unwrap_or(&0))
            .max()
            .unwrap_or(&0);
        let max_blue = self
            .sets
            .iter()
            .map(|set| set.cubes.get(&Color::Blue).unwrap_or(&0))
            .max()
            .unwrap_or(&0);
        let max_green = self
            .sets
            .iter()
            .map(|set| set.cubes.get(&Color::Green).unwrap_or(&0))
            .max()
            .unwrap_or(&0);

        result.insert(Color::Red, *max_red);
        result.insert(Color::Blue, *max_blue);
        result.insert(Color::Green, *max_green);

        Set { cubes: result }
    }
}

fn parse_line(line: &str) -> Game {
    let line: Vec<&str> = line.split(&[':', ';']).collect();

    let id = line[0].split(' ').nth(1).expect("Game should have id");

    let mut sets = Vec::new();

    for set in &line[1..] {
        let mut cubes = HashMap::new();
        let cubes_as_strings: Vec<&str> = set.split(", ").map(|x| x.trim()).collect();

        for cube in cubes_as_strings {
            let (number, color) = cube
                .split_once(' ')
                .expect("Should have pair number, color");

            let color: Color = color.parse().expect("Color should exist");
            let number: usize = number.parse().expect("Number of cubes should make sense");

            cubes.insert(color, number);
        }

        let set = Set { cubes };
        sets.push(set);
    }

    Game { id, sets }
}

fn solve_part_one(contents: &str) -> usize {
    contents
        .lines()
        .map(parse_line)
        .filter(|game| game.is_possible())
        .map(|game| {
            game.id
                .parse::<usize>()
                .expect("Id should be convertible to int")
        })
        .sum()
}

fn solve_part_two(contents: &str) -> usize {
    contents
        .lines()
        .map(parse_line)
        .map(|game| game.get_max_of_each_set())
        .map(|game| game.get_power())
        .sum()
}
fn main() {
    println!("{}", solve_part_one(INPUT));
    println!("{}", solve_part_two(INPUT));
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input.txt");

    #[test]
    fn it_works() {
        let line = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        println!("{:?}", parse_line(line));
    }

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST), 8)
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST), 2286)
    }
}

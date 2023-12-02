use std::str::FromStr;

use anyhow::{anyhow, Result};

const INPUT: &str = include_str!("../input.txt");

const RED_CUBES: usize = 12;
const GREEN_CUBES: usize = 13;
const BLUE_CUBES: usize = 14;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
enum Color {
    Red(usize),
    Green(usize),
    Blue(usize),
}

impl FromStr for Color {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let (num, color) = s
            .split_once(' ')
            .ok_or(anyhow!("Incorrect syntax for parsing {s}"))?;

        let num = num.parse()?;

        match color {
            "red" => Ok(Self::Red(num)),
            "green" => Ok(Self::Green(num)),
            "blue" => Ok(Self::Blue(num)),
            _ => Err(anyhow!("Invalid color")),
        }
    }
}

impl Color {
    fn get_max(&self) -> usize {
        match self {
            Self::Red(_) => RED_CUBES,
            Self::Green(_) => GREEN_CUBES,
            Self::Blue(_) => BLUE_CUBES,
        }
    }
}

#[derive(Debug)]
struct Set {
    red: usize,
    green: usize,
    blue: usize,
}

impl Set {
    fn is_possible(&self) -> bool {
        self.red <= RED_CUBES && self.green <= GREEN_CUBES && self.blue <= BLUE_CUBES
    }

    fn get_power(&self) -> usize {
        self.red * self.green * self.blue
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
        let max_red = self.sets.iter().map(|set| set.red).max().unwrap_or(0);
        let max_blue = self.sets.iter().map(|set| set.blue).max().unwrap_or(0);
        let max_green = self.sets.iter().map(|set| set.green).max().unwrap_or(0);

        Set {
            red: max_red,
            blue: max_blue,
            green: max_green,
        }
    }
}

fn parse_line(line: &str) -> Game {
    let line: Vec<&str> = line.split(&[':', ';']).collect();

    let id = line[0].split(' ').nth(1).expect("Game should have id");

    let mut sets = Vec::new();

    for set in &line[1..] {
        let mut red = 0;
        let mut green = 0;
        let mut blue = 0;

        let cubes_as_strings: Vec<&str> = set.split(", ").map(|x| x.trim()).collect();

        for cube in cubes_as_strings {
            let color: Color = cube
                .parse()
                .expect("Cube should be a pair (number, color) but was {cube}");

            match color {
                Color::Red(x) => red = x,
                Color::Green(x) => green = x,
                Color::Blue(x) => blue = x,
            }
        }

        let set = Set { red, green, blue };
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

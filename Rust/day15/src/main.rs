use std::collections::{LinkedList, VecDeque};

use anyhow::{anyhow, Result};

const TEST_INPUT: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

fn parse_contents_removing_nl(contents: &str) -> Vec<String> {
    contents
        .split(',')
        .map(|g| {
            let s: Vec<u8> = g
                .as_bytes()
                .iter()
                .copied()
                .filter(|&x| x != b'\n')
                .collect();
            String::from_utf8_lossy(&s).to_string()
        })
        .collect()
}

fn parse_contents(contents: &str) -> Vec<&str> {
    contents.split(',').map(|s| s.trim()).collect()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Lens<'a> {
    label: &'a str,
    focal_length: u8,
}

#[derive(Debug, Clone, Copy)]
enum Instruction<'a> {
    Insertion(Lens<'a>),
    Removal(&'a str),
}

fn parse_instructions(contents: &str) -> Vec<Instruction> {
    contents
        .split(',')
        .map(|s| s.trim())
        .map(|s| {
            let split = s.split_once('=');

            if let Some((label, focal_length)) = split {
                let focal_length = focal_length.parse().expect("Should be parseable");

                Instruction::Insertion(Lens {
                    label,
                    focal_length,
                })
            } else {
                Instruction::Removal(s.trim_end_matches('-'))
            }
        })
        .collect()
}

fn hash(s: &str) -> u32 {
    s.chars().fold(0, |acc, c| {
        let ascii = c as u32;

        let mut new_acc = acc + ascii;
        new_acc *= 17;
        new_acc = new_acc.rem_euclid(256);

        new_acc
    })
}

fn solve_part_one(contents: &str) -> u32 {
    let strings = parse_contents(contents);

    strings.iter().map(|s| hash(s)).sum()
}

fn solve_part_two(contents: &str) -> usize {
    let instructions = parse_instructions(contents);

    let mut boxes: Vec<Vec<Lens>> = vec![Vec::new(); 256];

    for instruction in instructions {
        match instruction {
            Instruction::Insertion(lens) => {
                let bx = hash(lens.label);
                if let Some(i) = boxes[bx as usize]
                    .iter()
                    .position(|&x| x.label == lens.label)
                {
                    boxes[bx as usize].remove(i);
                    boxes[bx as usize].insert(i, lens);
                } else {
                    boxes[bx as usize].push(lens);
                }
            }
            Instruction::Removal(label) => {
                let bx = hash(label);
                if let Some(i) = boxes[bx as usize].iter().position(|x| x.label == label) {
                    boxes[bx as usize].remove(i);
                }
            }
        }
    }

    boxes
        .iter()
        .enumerate()
        .map(|(box_number, b)| {
            b.iter()
                .enumerate()
                .map(|(slot, l)| (box_number + 1) * (slot + 1) * l.focal_length as usize)
                .sum::<usize>()
        })
        .sum()
}

fn main() {
    println!("{}", solve_part_one(INPUT));
    println!("{}", solve_part_two(INPUT));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT), 1320);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT), 145);
    }
}

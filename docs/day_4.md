# Day 4

## Part One

Part one is straightforward: you just parse your card numbers and the
numbers of the winning card and check to see which of your numbers are contained
in the winning card.

## Part Two

For part two, you just need to avoid increasing one by one and instead calculate
directly how many will be added to the successive cards.

The full code can be seen below.

```rust
use anyhow::{anyhow, Result};

const INPUT: &str = include_str!("../input.txt");

#[derive(Debug)]
struct Card {
    _id: i32,
    winning_numbers: Vec<i32>,
    my_numbers: Vec<i32>,
}

impl Card {
    fn get_my_winning(&self) -> Vec<i32> {
        self.my_numbers
            .iter()
            .copied()
            .filter(|my_number| self.winning_numbers.contains(my_number))
            .collect()
    }
}

fn parse_line(line: &str) -> Result<Card> {
    let (card_identification, numbers) = line
        .split_once(':')
        .ok_or(anyhow!("ERROR: Syntax error at line '{line}''"))?;

    let (_, id) = card_identification.split_once(' ').ok_or(anyhow!(
        "ERROR: Syntax error with identification '{card_identification}'"
    ))?;

    let _id: i32 = id.trim().parse()?;

    let (winning_numbers, my_numbers) = numbers.split_once('|').ok_or(anyhow!(
        "ERROR: Syntax error when splitting numbers '{line}'"
    ))?;

    let winning_numbers = winning_numbers
        .split_whitespace()
        .flat_map(|n| {
            n.parse().map_err(|e| {
                eprintln!("ERROR: Parse error when parsing {n} to i32");
                e
            })
        })
        .collect();

    let my_numbers = my_numbers
        .split_whitespace()
        .flat_map(|n| {
            n.parse().map_err(|e| {
                eprintln!("ERROR: Parse error when parsing {n} to i32");
                e
            })
        })
        .collect();

    Ok(Card {
        _id,
        winning_numbers,
        my_numbers,
    })
}

fn parse_contents(contents: &str) -> Result<Vec<Card>> {
    contents.lines().map(parse_line).collect()
}

fn solve_part_one(contents: &str) -> Result<u32> {
    let cards = parse_contents(contents)?;

    Ok(cards
        .iter()
        .map(|card| {
            let number_of_winning = card.get_my_winning().len();

            if number_of_winning == 0 {
                0
            } else {
                2_u32.pow(number_of_winning as u32 - 1)
            }
        })
        .sum())
}

fn solve_part_two(contents: &str) -> Result<u32> {
    let cards = parse_contents(contents)?;

    let mut cards_amounts: Vec<usize> = vec![1; cards.len()];

    for i in 0..cards.len() {
        let n = cards[i].get_my_winning().len();

        for j in 0..n.min(cards.len() - 1) {
            cards_amounts[(i + 1) + j] += cards_amounts[i];
        }
    }

    Ok(cards_amounts.iter().map(|x| *x as u32).sum())
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
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 13)
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT).unwrap(), 30)
    }
}
```

use anyhow::{anyhow, Result};

const INPUT: &str = include_str!("../input.txt");

#[derive(Debug)]
struct Card {
    id: i32,
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
    let (card_identification, numbers) = line.split_once(':').ok_or(anyhow!("Syntax error"))?;

    let (_, id) = card_identification
        .split_once(' ')
        .ok_or(anyhow!("Syntax error"))?;

    let id: i32 = id.trim().parse()?;

    let (winning_numbers, my_numbers) = numbers.split_once('|').ok_or(anyhow!("Syntax error"))?;

    let winning_numbers = winning_numbers
        .split_whitespace()
        .flat_map(|n| n.parse())
        .collect();

    let my_numbers = my_numbers
        .split_whitespace()
        .flat_map(|n| n.parse())
        .collect();

    Ok(Card {
        id,
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
        let amounts_at_i = *cards_amounts.get(i).unwrap_or(&0);

        for j in 0..n.min(cards.len() - 1) {
            cards_amounts[(i + 1) + j] += amounts_at_i;
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

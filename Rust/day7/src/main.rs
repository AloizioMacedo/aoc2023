mod card;
mod card_with_joker;
mod hand;

use anyhow::{anyhow, Result};
use hand::Hand;
use itertools::Itertools;

const INPUT: &str = include_str!("../input.txt");

fn parse_contents<T>(contents: &str) -> Result<Vec<Hand<T>>>
where
    T: TryFrom<char>,
{
    contents.lines().map(parse_line).collect()
}

fn parse_line<T>(line: &str) -> Result<Hand<T>>
where
    T: TryFrom<char>,
{
    let (cards, bid) = line
        .split_once(' ')
        .ok_or(anyhow!("Syntax error in '{line}'"))?;

    let (card1, card2, card3, card4, card5) = cards
        .chars()
        .flat_map(T::try_from)
        .collect_tuple()
        .ok_or(anyhow!("Syntax error in '{line}'"))?;
    let cards = [card1, card2, card3, card4, card5];

    let bid = bid.parse()?;

    Ok(Hand::new(cards, bid))
}

fn solve_part_one(contents: &str) -> Result<u64> {
    let mut hands = parse_contents::<card::Card>(contents)?;

    hands.sort();

    Ok(hands
        .iter()
        .enumerate()
        .map(|(i, hand)| (i as u64 + 1) * hand.bid)
        .sum::<u64>())
}

fn solve_part_two(contents: &str) -> Result<u64> {
    let mut hands = parse_contents::<card_with_joker::CardWithJoker>(contents)?;

    hands.sort();

    Ok(hands
        .iter()
        .enumerate()
        .map(|(i, hand)| (i as u64 + 1) * hand.bid)
        .sum::<u64>())
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
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 6440);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT).unwrap(), 5905);
    }
}

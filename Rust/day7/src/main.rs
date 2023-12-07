use anyhow::{anyhow, Result};
use itertools::Itertools;

const INPUT: &str = include_str!("../input.txt");

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy, Debug, Hash)]
enum Card {
    Number(u64),
    T,
    J,
    Q,
    K,
    A,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy, Debug)]
enum Type {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

fn get_type(cards: [Card; 5]) -> Type {
    if cards.iter().all(|c| *c == cards[0]) {
        return Type::FiveOfAKind;
    }

    if cards.iter().counts().values().any(|v| *v == 4) {
        return Type::FourOfAKind;
    }

    if let Some((x, y)) = cards.iter().counts().values().collect_tuple() {
        if (*x, *y) == (2, 3) || (*x, *y) == (3, 2) {
            return Type::FullHouse;
        }
    }

    if cards.iter().counts().values().any(|v| *v == 3) {
        return Type::ThreeOfAKind;
    }

    if cards.iter().counts().values().filter(|v| **v == 2).count() == 2 {
        return Type::TwoPair;
    }

    if cards.iter().counts().values().filter(|v| **v == 2).count() == 1 {
        return Type::OnePair;
    }

    Type::HighCard
}

impl TryFrom<char> for Card {
    type Error = anyhow::Error;

    fn try_from(value: char) -> std::prelude::v1::Result<Self, Self::Error> {
        if let Some(x) = value.to_digit(10) {
            return Ok(Self::Number(x as u64));
        }

        match value {
            'T' => Ok(Card::T),
            'J' => Ok(Card::J),
            'Q' => Ok(Card::Q),
            'K' => Ok(Card::K),
            'A' => Ok(Card::A),
            _ => Err(anyhow!("Invalid card: '{value}'")),
        }
    }
}

#[derive(Debug)]
struct Hand {
    cards: [Card; 5],
    bid: u64,
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_type = get_type(self.cards);
        let other_type = get_type(other.cards);

        match self_type.cmp(&other_type) {
            std::cmp::Ordering::Equal => self.cards.cmp(&other.cards),
            x => x,
        }
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        let mut my_cards = self.cards;
        let mut your_cards = self.cards;

        my_cards.sort();
        your_cards.sort();

        my_cards == your_cards
    }
}

impl Eq for Hand {}

fn parse_contents(contents: &str) -> Result<Vec<Hand>> {
    contents.lines().map(parse_line).collect()
}

fn parse_line(line: &str) -> Result<Hand> {
    let (cards, bid) = line
        .split_once(' ')
        .ok_or(anyhow!("Syntax error in '{line}'"))?;

    let (card1, card2, card3, card4, card5) = cards
        .chars()
        .flat_map(Card::try_from)
        .collect_tuple()
        .ok_or(anyhow!("Syntax error in '{line}'"))?;
    let cards = [card1, card2, card3, card4, card5];

    let bid = bid.parse()?;

    Ok(Hand { cards, bid })
}

fn solve_part_one(contents: &str) -> Result<u64> {
    let mut hands = parse_contents(contents)?;

    hands.sort();

    println!("{:?}", &hands[(hands.len() - 3)..]);

    Ok(hands
        .iter()
        .enumerate()
        .map(|(i, hand)| (i as u64 + 1) * hand.bid)
        .sum::<u64>())
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
    fn ord() {
        assert!(Card::Number(5) < Card::Number(7));
        assert!(Card::Number(5) < Card::K);
        assert!(Card::Number(6) > Card::Number(3));
        assert!(Card::A > Card::T);
    }

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 6440);
    }
}

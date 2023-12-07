use std::fmt::{Debug, Display, Formatter};

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

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy, Hash)]
enum CardWithJoker {
    J,
    Number(u64),
    T,
    Q,
    K,
    A,
}

impl Display for CardWithJoker {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::J => f.write_str("J"),
            Self::Number(x) => f.write_fmt(format_args!("{}", x)),
            Self::T => f.write_str("T"),
            Self::Q => f.write_str("Q"),
            Self::K => f.write_str("K"),
            Self::A => f.write_str("A"),
        }
    }
}

impl Debug for CardWithJoker {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::J => f.write_str("J"),
            Self::Number(x) => f.write_fmt(format_args!("{}", x)),
            Self::T => f.write_str("T"),
            Self::Q => f.write_str("Q"),
            Self::K => f.write_str("K"),
            Self::A => f.write_str("A"),
        }
    }
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

fn get_type_with_joker(cards: [CardWithJoker; 5]) -> Type {
    let jokers = cards
        .iter()
        .filter(|x| matches!(x, CardWithJoker::J))
        .count();

    let non_jokers: Vec<CardWithJoker> = cards
        .iter()
        .copied()
        .filter(|x| !matches!(x, CardWithJoker::J))
        .collect();

    if non_jokers
        .iter()
        .counts()
        .values()
        .any(|v| *v + jokers == 5)
        || jokers == 5
    {
        return Type::FiveOfAKind;
    }

    if non_jokers
        .iter()
        .counts()
        .values()
        .any(|v| *v + jokers == 4)
    {
        return Type::FourOfAKind;
    }

    if let Some((x, y)) = non_jokers.iter().counts().values().collect_tuple() {
        let mut counts = vec![*x, *y, jokers];
        counts.sort();

        if counts == vec![1, 2, 2] || counts == vec![0, 2, 3] {
            return Type::FullHouse;
        }
    }

    if non_jokers
        .iter()
        .counts()
        .values()
        .any(|v| *v + jokers == 3)
    {
        return Type::ThreeOfAKind;
    }

    // Two pair can't happen with a joker, because otherwise it would at least turn
    // into a three of a kind.
    if non_jokers
        .iter()
        .counts()
        .values()
        .filter(|v| **v == 2)
        .count()
        == 2
    {
        return Type::TwoPair;
    }

    if non_jokers
        .iter()
        .counts()
        .values()
        .any(|v| *v + jokers == 2)
    {
        return Type::OnePair;
    }

    Type::HighCard
}

impl TryFrom<char> for Card {
    type Error = anyhow::Error;

    fn try_from(value: char) -> std::prelude::v1::Result<Self, Self::Error> {
        if let Some(x) = value.to_digit(10) {
            return Ok(Card::Number(x as u64));
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

impl TryFrom<char> for CardWithJoker {
    type Error = anyhow::Error;

    fn try_from(value: char) -> std::prelude::v1::Result<Self, Self::Error> {
        if let Some(x) = value.to_digit(10) {
            return Ok(CardWithJoker::Number(x as u64));
        }

        match value {
            'T' => Ok(CardWithJoker::T),
            'J' => Ok(CardWithJoker::J),
            'Q' => Ok(CardWithJoker::Q),
            'K' => Ok(CardWithJoker::K),
            'A' => Ok(CardWithJoker::A),
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
        let mut your_cards = other.cards;

        my_cards.sort();
        your_cards.sort();

        my_cards == your_cards
    }
}

impl Eq for Hand {}

#[derive(Debug)]
struct HandWithJoker {
    cards: [CardWithJoker; 5],
    bid: u64,
}

impl PartialOrd for HandWithJoker {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HandWithJoker {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_type = get_type_with_joker(self.cards);
        let other_type = get_type_with_joker(other.cards);

        match self_type.cmp(&other_type) {
            std::cmp::Ordering::Equal => self.cards.cmp(&other.cards),
            x => x,
        }
    }
}

impl PartialEq for HandWithJoker {
    fn eq(&self, other: &Self) -> bool {
        let mut my_cards = self.cards;
        let mut your_cards = other.cards;

        my_cards.sort();
        your_cards.sort();

        my_cards == your_cards
    }
}

impl Eq for HandWithJoker {}

fn parse_contents(contents: &str) -> Result<Vec<Hand>> {
    contents.lines().map(parse_line).collect()
}

fn parse_contents_with_joker(contents: &str) -> Result<Vec<HandWithJoker>> {
    contents.lines().map(parse_line_with_joker).collect()
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

fn parse_line_with_joker(line: &str) -> Result<HandWithJoker> {
    let (cards, bid) = line
        .split_once(' ')
        .ok_or(anyhow!("Syntax error in '{line}'"))?;

    let (card1, card2, card3, card4, card5) = cards
        .chars()
        .flat_map(CardWithJoker::try_from)
        .collect_tuple()
        .ok_or(anyhow!("Syntax error in '{line}'"))?;
    let cards = [card1, card2, card3, card4, card5];

    let bid = bid.parse()?;

    Ok(HandWithJoker { cards, bid })
}

fn solve_part_one(contents: &str) -> Result<u64> {
    let mut hands = parse_contents(contents)?;

    hands.sort();

    Ok(hands
        .iter()
        .enumerate()
        .map(|(i, hand)| (i as u64 + 1) * hand.bid)
        .sum::<u64>())
}

fn solve_part_two(contents: &str) -> Result<u64> {
    let mut hands = parse_contents_with_joker(contents)?;

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
    fn ord() {
        assert!(Card::Number(5) < Card::Number(7));
        assert!(Card::Number(5) < Card::K);
        assert!(Card::Number(6) > Card::Number(3));
        assert!(Card::A > Card::T);

        assert!(Card::J > Card::Number(3));
        assert!(CardWithJoker::J < CardWithJoker::Number(3));
    }

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 6440);
    }

    #[test]
    fn test_matches_with_joker() {
        let cards: [CardWithJoker; 5] = [
            'A'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            'T'.try_into().unwrap(),
            '2'.try_into().unwrap(),
            '7'.try_into().unwrap(),
        ];

        println!("{:?}", get_type_with_joker(cards));
        assert!(matches!(get_type_with_joker(cards), Type::OnePair));

        let cards: [CardWithJoker; 5] = [
            'A'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            'A'.try_into().unwrap(),
        ];

        println!("{:?}", get_type_with_joker(cards));
        assert!(matches!(get_type_with_joker(cards), Type::FiveOfAKind));

        let cards: [CardWithJoker; 5] = [
            'A'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            '2'.try_into().unwrap(),
            '7'.try_into().unwrap(),
        ];

        println!("{:?}", get_type_with_joker(cards));
        assert!(matches!(get_type_with_joker(cards), Type::ThreeOfAKind));

        let cards: [CardWithJoker; 5] = [
            'A'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            '2'.try_into().unwrap(),
            '2'.try_into().unwrap(),
            '7'.try_into().unwrap(),
        ];

        println!("{:?}", get_type_with_joker(cards));
        assert!(matches!(get_type_with_joker(cards), Type::ThreeOfAKind));

        let cards: [CardWithJoker; 5] = [
            'J'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            'T'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            '7'.try_into().unwrap(),
        ];

        println!("{:?}", get_type_with_joker(cards));
        assert!(matches!(get_type_with_joker(cards), Type::FourOfAKind));

        let cards: [CardWithJoker; 5] = [
            '2'.try_into().unwrap(),
            'J'.try_into().unwrap(),
            '2'.try_into().unwrap(),
            '7'.try_into().unwrap(),
            '7'.try_into().unwrap(),
        ];

        println!("{:?}", get_type_with_joker(cards));
        assert!(matches!(get_type_with_joker(cards), Type::FullHouse));

        let cards: [CardWithJoker; 5] = [
            '2'.try_into().unwrap(),
            '1'.try_into().unwrap(),
            '2'.try_into().unwrap(),
            '1'.try_into().unwrap(),
            '7'.try_into().unwrap(),
        ];

        println!("{:?}", get_type_with_joker(cards));
        assert!(matches!(get_type_with_joker(cards), Type::TwoPair));
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT).unwrap(), 5905);
    }

    #[test]
    fn test_count() {
        let oi = [3, 3, 2, 10, 1, 1];

        assert_eq!(oi.iter().counts().values().len(), 4);
    }
}

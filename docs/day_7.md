# Day 7

Not really much to comment here in regards to the algorithm itself, other than the
main idea being to define an ordering so that the solution becomes just sorting
the hands at the end.

## Part One

In `hand.rs`, we define the `Hand` and hand types

```rust
#[derive(Debug)]
pub(crate) struct Hand<T> {
    pub(crate) cards: [T; 5],
    pub(crate) bid: u64,
}

impl<T> Hand<T> {
    pub(crate) fn new(cards: [T; 5], bid: u64) -> Hand<T> {
        Hand { cards, bid }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy, Debug)]
pub(crate) enum Type {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}
```

In `card.rs`, we define things related to the cards themselves, including the ordering

```rust
use anyhow::anyhow;
use itertools::Itertools;

use crate::hand::{Hand, Type};

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum Card {
    Number(u64),
    T,
    J,
    Q,
    K,
    A,
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

impl PartialOrd for Hand<Card> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand<Card> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_type = get_type(self.cards);
        let other_type = get_type(other.cards);

        match self_type.cmp(&other_type) {
            std::cmp::Ordering::Equal => self.cards.cmp(&other.cards),
            x => x,
        }
    }
}

impl PartialEq for Hand<Card> {
    fn eq(&self, other: &Self) -> bool {
        let mut my_cards = self.cards;
        let mut your_cards = other.cards;

        my_cards.sort();
        your_cards.sort();

        my_cards == your_cards
    }
}

impl Eq for Hand<Card> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ord() {
        assert!(Card::Number(5) < Card::Number(7));
        assert!(Card::Number(5) < Card::K);
        assert!(Card::Number(6) > Card::Number(3));
        assert!(Card::A > Card::T);
        assert!(Card::J > Card::Number(3));
    }
}
```

## Part Two

Part two consists of making some adaptations related to the Joker. I created a lot
of tests for debugging cases that were not taken into consideration for the
given test cases.

```rust
use anyhow::anyhow;
use itertools::Itertools;

use crate::hand::{Hand, Type};

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy, Hash)]
pub(crate) enum CardWithJoker {
    J,
    Number(u64),
    T,
    Q,
    K,
    A,
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

impl PartialOrd for Hand<CardWithJoker> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand<CardWithJoker> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_type = get_type_with_joker(self.cards);
        let other_type = get_type_with_joker(other.cards);

        match self_type.cmp(&other_type) {
            std::cmp::Ordering::Equal => self.cards.cmp(&other.cards),
            x => x,
        }
    }
}

impl PartialEq for Hand<CardWithJoker> {
    fn eq(&self, other: &Self) -> bool {
        let mut my_cards = self.cards;
        let mut your_cards = other.cards;

        my_cards.sort();
        your_cards.sort();

        my_cards == your_cards
    }
}

impl Eq for Hand<CardWithJoker> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ord() {
        assert!(CardWithJoker::Number(5) < CardWithJoker::Number(7));
        assert!(CardWithJoker::Number(5) < CardWithJoker::K);
        assert!(CardWithJoker::Number(6) > CardWithJoker::Number(3));
        assert!(CardWithJoker::A > CardWithJoker::T);
        assert!(CardWithJoker::J < CardWithJoker::Number(3));
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
}
```

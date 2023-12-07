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

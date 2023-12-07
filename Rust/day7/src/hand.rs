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

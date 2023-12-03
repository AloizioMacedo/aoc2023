use itertools::Itertools;

const INPUT: &str = include_str!("../input.txt");

mod parsing {
    use itertools::Itertools;

    use crate::schematic::{Entry, PositionNumber, PositionSymbol, Schematic};

    pub(crate) fn parse_contents(contents: &str) -> Schematic {
        let mut numbers = vec![];
        let mut symbols = vec![];

        let columns = contents
            .lines()
            .next()
            .expect("File should not be empty")
            .len();

        let mut digits_buffer = Vec::with_capacity(columns);

        for (i, line) in contents.lines().enumerate() {
            let entries = parse_line(line);

            let pos_symbols = entries.iter().enumerate().filter_map(|(j, e)| {
                if let Entry::Symbol(x) = e {
                    Some(PositionSymbol {
                        line: i,
                        col: j,
                        symbol: *x,
                    })
                } else {
                    None
                }
            });

            let grouped_digits = entries
                .iter()
                .copied()
                .enumerate()
                .group_by(|(_, e)| matches!(e, Entry::Digit(_)));

            let pos_numbers = grouped_digits.into_iter().flat_map(|(k, g)| {
                if !k {
                    None
                } else {
                    digits_buffer.extend(g);

                    let g = &digits_buffer;
                    let n = g.len();

                    let start_col = g[0].0;

                    let mut number = 0;
                    for (k, entry) in g.iter().enumerate() {
                        match entry.1 {
                            Entry::Digit(x) => {
                                number += 10_usize.pow((n - k - 1) as u32) * x as usize
                            }
                            _ => unreachable!("Groups have been filtered to only contain digits"),
                        }
                    }

                    digits_buffer.clear();

                    Some(PositionNumber {
                        len: n,
                        start_col,
                        line: i,
                        number: number as u32,
                    })
                }
            });

            numbers.extend(pos_numbers);
            symbols.extend(pos_symbols);
        }

        Schematic { numbers, symbols }
    }

    pub(crate) fn parse_line(line: &str) -> Vec<Entry> {
        line.chars().map(Entry::from).collect()
    }
}

mod schematic {
    #[derive(Clone, Copy)]
    pub(crate) enum Entry {
        Empty,
        Symbol(char),
        Digit(u32),
    }

    impl From<char> for Entry {
        fn from(value: char) -> Self {
            let parsed_value = value.to_digit(10);

            if let Some(x) = parsed_value {
                Self::Digit(x)
            } else {
                match value {
                    '.' => Entry::Empty,
                    x => Entry::Symbol(x),
                }
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub(crate) struct PositionNumber {
        pub(crate) start_col: usize,
        pub(crate) line: usize,
        pub(crate) len: usize,
        pub(crate) number: u32,
    }

    impl PositionNumber {
        pub(crate) fn is_close(&self, symbol: &PositionSymbol) -> bool {
            for i in 0..self.len {
                if (self.start_col + i).abs_diff(symbol.col) <= 1
                    && (self.line).abs_diff(symbol.line) <= 1
                {
                    return true;
                }
            }

            false
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub(crate) struct PositionSymbol {
        pub(crate) col: usize,
        pub(crate) line: usize,
        pub(crate) symbol: char,
    }

    impl PositionSymbol {
        pub(crate) fn get_numbers_close(
            self,
            pns: &[PositionNumber],
        ) -> impl Iterator<Item = PositionNumber> + '_ {
            pns.iter().copied().filter(move |pn| pn.is_close(&self))
        }
    }

    #[derive(Debug)]
    pub(crate) struct Schematic {
        pub(crate) numbers: Vec<PositionNumber>,
        pub(crate) symbols: Vec<PositionSymbol>,
    }

    impl Schematic {
        pub(crate) fn get_part_numbers(&self) -> impl Iterator<Item = PositionNumber> + '_ {
            self.numbers
                .iter()
                .copied()
                .filter(|pn| self.symbols.iter().any(|s| pn.is_close(s)))
        }
    }
}

fn solve_part_one(contents: &str) -> u32 {
    let schematic = parsing::parse_contents(contents);

    schematic.get_part_numbers().map(|pn| pn.number).sum()
}

fn solve_part_two(contents: &str) -> u32 {
    let schematic = parsing::parse_contents(contents);

    let symbols = schematic.symbols;

    symbols
        .iter()
        .filter(|symbol| symbol.symbol == '*')
        .filter_map(|symbol| {
            let numbers_close = symbol.get_numbers_close(&schematic.numbers);

            if let Some((x, y)) = numbers_close.collect_tuple() {
                Some((x, y))
            } else {
                None
            }
        })
        .map(|numbers_close| numbers_close.0.number * numbers_close.1.number)
        .sum()
}

fn main() {
    println!("{}", solve_part_one(INPUT));
    println!("{}", solve_part_two(INPUT));
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT: &str = include_str!("../test_input.txt");

    #[test]
    fn it_works() {
        assert_eq!(solve_part_one(TEST_INPUT), 4361);
    }

    #[test]
    fn it_works2() {
        assert_eq!(solve_part_two(TEST_INPUT), 467835);
    }
}

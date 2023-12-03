# Day 3

The code for this day ended up being very unwieldy and will require heavy refactoring.

Before displaying it, let's first think about the main problems/insights:

- First of all, while building a matrix might seem like the first idea, it was much
  simpler to store the position of numbers and symbols separately.
- Building the numbers from the parsing was a little bit uncomfortable, particularly
  considering that we wanted to parse everything: the numbers, the symbols and the
  empty spaces.
- The full time complexity of the algorithm, if you simply disregard optimizing how
  we check who is close, is \\(O(n^2)\\). Gladly this was not a problem at all.

The next step is going to be refactoring the whole thing, both for performance and
readability. This is the version of the code used for the submission itself:

```rust
use itertools::Itertools;

const INPUT: &str = include_str!("../input.txt");

#[derive(Clone, Copy)]
enum Entry {
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
struct PositionNumber {
    start_col: usize,
    line: usize,
    len: usize,
    number: u32,
}

impl PositionNumber {
    fn is_close(&self, symbol: &PositionSymbol) -> bool {
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

#[derive(Debug)]
struct PositionSymbol {
    col: usize,
    line: usize,
    symbol: char,
}

impl PositionSymbol {
    fn get_numbers_close(&self, pns: &[PositionNumber]) -> Vec<PositionNumber> {
        pns.iter().copied().filter(|pn| pn.is_close(self)).collect()
    }
}

#[derive(Debug)]
struct Schematic {
    numbers: Vec<PositionNumber>,
    symbols: Vec<PositionSymbol>,
}

impl Schematic {
    fn get_part_numbers(&self) -> Vec<PositionNumber> {
        self.numbers
            .iter()
            .copied()
            .filter(|pn| self.symbols.iter().any(|s| pn.is_close(s)))
            .collect()
    }
}

fn parse_contents(contents: &str) -> Schematic {
    let mut numbers = vec![];
    let mut symbols = vec![];

    for (i, line) in contents.lines().enumerate() {
        let entries = parse_line(line);

        let pos_symbols: Vec<PositionSymbol> = entries
            .iter()
            .enumerate()
            .filter(|(_, e)| matches!(e, Entry::Symbol(_)))
            .map(|(j, e)| {
                if let Entry::Symbol(x) = e {
                    PositionSymbol {
                        line: i,
                        col: j,
                        symbol: *x,
                    }
                } else {
                    unreachable!()
                }
            })
            .collect();

        let pos_numbers: Vec<PositionNumber> = entries
            .iter()
            .enumerate()
            .group_by(|(_, e)| matches!(e, Entry::Digit(_)))
            .into_iter()
            .flat_map(|(k, g)| {
                if !k {
                    None
                } else {
                    let g: Vec<(usize, &Entry)> = g.collect();
                    let n = g.len();

                    let mut g = g.iter().peekable();
                    let start_col = g.peek().unwrap().0;

                    let mut number = 0;
                    for (k, entry) in g.enumerate() {
                        match entry.1 {
                            Entry::Digit(x) => {
                                number += 10_usize.pow((n - k - 1) as u32) * *x as usize
                            }
                            _ => unreachable!(),
                        }
                    }

                    Some(PositionNumber {
                        len: n,
                        start_col,
                        line: i,
                        number: number as u32,
                    })
                }
            })
            .collect();

        numbers.extend(pos_numbers);
        symbols.extend(pos_symbols);
    }

    Schematic { numbers, symbols }
}

fn parse_line(line: &str) -> Vec<Entry> {
    line.chars().map(Entry::from).collect()
}

fn solve_part_one(contents: &str) -> u32 {
    let schematic = parse_contents(contents);

    schematic
        .get_part_numbers()
        .iter()
        .map(|pn| pn.number)
        .sum()
}

fn solve_part_two(contents: &str) -> u32 {
    let schematic = parse_contents(contents);

    let symbols = schematic.symbols;

    symbols
        .iter()
        .filter(|symbol| symbol.symbol == '*')
        .filter_map(|symbol| {
            let numbers_close = symbol.get_numbers_close(&schematic.numbers);

            if numbers_close.len() == 2 {
                Some(numbers_close)
            } else {
                None
            }
        })
        .map(|numbers_close| numbers_close.iter().map(|x| x.number).product::<u32>())
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
```

## Refactoring

The refactored code is below.

```rust
use itertools::Itertools;

const INPUT: &str = include_str!("../input.txt");

#[derive(Clone, Copy)]
enum Entry {
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
struct PositionNumber {
    start_col: usize,
    line: usize,
    len: usize,
    number: u32,
}

impl PositionNumber {
    fn is_close(&self, symbol: &PositionSymbol) -> bool {
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
struct PositionSymbol {
    col: usize,
    line: usize,
    symbol: char,
}

impl PositionSymbol {
    fn get_numbers_close(
        self,
        pns: &[PositionNumber],
    ) -> impl Iterator<Item = PositionNumber> + '_ {
        pns.iter().copied().filter(move |pn| pn.is_close(&self))
    }
}

#[derive(Debug)]
struct Schematic {
    numbers: Vec<PositionNumber>,
    symbols: Vec<PositionSymbol>,
}

impl Schematic {
    fn get_part_numbers(&self) -> impl Iterator<Item = PositionNumber> + '_ {
        self.numbers
            .iter()
            .copied()
            .filter(|pn| self.symbols.iter().any(|s| pn.is_close(s)))
    }
}

fn parse_contents(contents: &str) -> Schematic {
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
                        Entry::Digit(x) => number += 10_usize.pow((n - k - 1) as u32) * x as usize,
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

fn parse_line(line: &str) -> Vec<Entry> {
    line.chars().map(Entry::from).collect()
}

fn solve_part_one(contents: &str) -> u32 {
    let schematic = parse_contents(contents);

    schematic.get_part_numbers().map(|pn| pn.number).sum()
}

fn solve_part_two(contents: &str) -> u32 {
    let schematic = parse_contents(contents);

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
```

I opted not to optimize for the time complexity as
the runtime is negligible with the full input and it would require us to create the
whole matrix, i.e. we would be doing a space-time complexity trade-off which is not
even clear if it would be worth it for the use case.

But it might be worthwhile to say how we would accomplish this:

1. First, build a full matrix together with the empty entries.
1. When checking for who is close, we just iterate over of the matrix
   indices of the neighbors to check for numbers.

This reduces the complexity since get_numbers_close would now be \\(O(1)\\) instead of
\\(O(n)\\).

> If we decided to check adjacencies while parsing, we would be able to avoid the
> creation of the matrix, thus getting the time complexity improvement "for free".
> However, I want to keep the transformational aspect of the algorithm, i.e. preserve
> the transformation of input into internal data structure and only then processing it.

In the refactoring, however, we removed a lot of heap allocation and one unnecessary
unreachable.

I ultimately wanted to remove the unreachable that is left, but I all solutions that
I attempted by using the type system directly was obfuscating the solution much
more than clarifying it. So I decided to leave as is. If someone has a better idea
that is idiomatic, please let me know!

## Extracting modules

To keep things cleaner, I decided to make another refactoring extracting parts of
the code into modules.

Our `main.rs` became:

```rust
use itertools::Itertools;

mod parsing;
mod schematics;

const INPUT: &str = include_str!("../input.txt");

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
```

Parsing functionalities were put into `parsing.rs`:

```rust
use itertools::Itertools;

use crate::schematics::{Entry, PositionNumber, PositionSymbol, Schematic};

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
                        Entry::Digit(x) => number += 10_usize.pow((n - k - 1) as u32) * x as usize,
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
```

and things related to the schematics themselves were put into `schematics.rs`:

```rust
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
```

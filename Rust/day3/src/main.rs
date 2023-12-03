use anyhow::Result;

const INPUT: &str = include_str!("../test_input.txt");

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

        let mut digit_buffer: Vec<u32> = vec![];
        for (j, entry) in entries.iter().enumerate() {
            match entry {
                Entry::Empty => (),
                Entry::Symbol(_) => {
                    let pos = PositionSymbol { col: j, line: i };
                    symbols.push(pos);
                }
                Entry::Digit(x) => {
                    let len = (*x as f64).log10() as usize;

                    let pos = PositionNumber {
                        start_col: j,
                        len,
                        line: i,
                        number: *x,
                    };
                    numbers.push(pos);
                }
            }
        }
    }

    Schematic { numbers, symbols }
}

fn parse_line(line: &str) -> Vec<Entry> {
    line.chars().map(Entry::from).collect()
}

fn solve_part_one(contents: &str) -> u32 {
    let schematic = parse_contents(contents);
    println!("{:?}", schematic);

    schematic
        .get_part_numbers()
        .iter()
        .map(|pn| pn.number)
        .sum()
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT: &str = include_str!("../test_input.txt");

    #[test]
    fn it_works() {
        assert_eq!(solve_part_one(TEST_INPUT), 4361);
    }
}

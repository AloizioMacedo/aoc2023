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

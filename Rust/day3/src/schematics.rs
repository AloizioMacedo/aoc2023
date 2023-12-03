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

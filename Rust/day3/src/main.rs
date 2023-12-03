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

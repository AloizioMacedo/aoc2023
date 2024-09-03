use anyhow::{anyhow, Result};

const INPUT: &str = include_str!("../input.txt");

const SPELLED_OUT_NUMBERS: [&str; 10] = [
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

fn parse_line(line: &str) -> Result<u32> {
    let mut numbers = line.chars().filter_map(|c| c.to_digit(10)).peekable();

    let first = *numbers
        .peek()
        .ok_or_else(|| anyhow!("Line missing a number."))?;
    let last = numbers
        .last()
        .ok_or_else(|| anyhow!("Line missing a number."))?;

    Ok(10 * first + last)
}

fn parse_line_two(line: &str) -> Result<u32> {
    let mut numbers = line
        .char_indices()
        .filter_map(|(i, c)| {
            for (j, number) in SPELLED_OUT_NUMBERS.iter().enumerate() {
                if line[i..].starts_with(number) {
                    return Some(j as u32);
                }
            }

            c.to_digit(10)
        })
        .peekable();

    let first = *numbers
        .peek()
        .ok_or_else(|| anyhow!("Line missing a number."))?;
    let last = numbers
        .last()
        .ok_or_else(|| anyhow!("Line missing a number."))?;

    Ok(10 * first + last)
}

fn solve_part_one(contents: &str) -> Result<u32> {
    contents.lines().map(parse_line).sum()
}

fn solve_part_two(contents: &str) -> Result<u32> {
    contents.lines().map(parse_line_two).sum()
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_P1: &str = include_str!("../test_input_p1.txt");
    const TEST_P2: &str = include_str!("../test_input_p2.txt");

    #[test]
    fn parsing() {
        assert_eq!(parse_line("1abc2").unwrap(), 12);
        assert_eq!(parse_line("pqr3stu8vwx").unwrap(), 38);
        assert_eq!(parse_line("a1b2c3d4e5f").unwrap(), 15);
        assert_eq!(parse_line("treb7uchet").unwrap(), 77);
    }

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_P1).unwrap(), 142);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_P2).unwrap(), 281);
    }
}

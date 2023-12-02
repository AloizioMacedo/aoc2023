use anyhow::{anyhow, Result};

const INPUT: &str = include_str!("../input.txt");

const SPELLED_OUT_NUMBERS: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

// To avoid having to allocate Strings from the integers.
const _ONE_TO_9: [&str; 9] = ["1", "2", "3", "4", "5", "6", "7", "8", "9"];

fn parse_line(line: &str) -> Result<u32> {
    let mut numbers = line.chars().filter_map(|c| c.to_digit(10)).peekable();

    let first = *numbers
        .peek()
        .ok_or(anyhow!("Line {line} missing a number."))?;
    let last = numbers
        .last()
        .ok_or(anyhow!("Line {line} missing a number."))?;

    Ok(10 * first + last)
}

fn transform_line(line: &str) -> Vec<u32> {
    let mut numbers = Vec::new();

    for i in 0..line.len() {
        for (j, spelled_out_number) in SPELLED_OUT_NUMBERS.iter().enumerate() {
            if line[i..].starts_with(spelled_out_number) {
                numbers.push(j as u32 + 1);
                break;
            }
        }

        if let Some(x) = line[i..]
            .chars()
            .next()
            .expect("Should not be empty given that i < line.len()")
            .to_digit(10)
        {
            numbers.push(x);
        }
    }

    numbers
}

/// This trick doesn't work for some reason that is still not clear.
fn _adjust_line(line: &str) -> String {
    let mut line = line.to_string();

    for (spelled_out_number, number) in SPELLED_OUT_NUMBERS.iter().zip(_ONE_TO_9) {
        line = line.replace(
            spelled_out_number,
            &(spelled_out_number.to_string() + number), // It is necessary to keep the spelled out
                                                        // version in order not to lose chars.
        );
    }

    line
}

fn solve_part_one(contents: &str) -> Result<u32> {
    contents.lines().map(parse_line).sum()
}

fn solve_part_two(contents: &str) -> Result<u32> {
    contents
        .lines()
        .map(transform_line)
        .map(|numbers| match (numbers.first(), numbers.last()) {
            (Some(x), Some(y)) => Ok(10 * x + y),
            _ => Err(anyhow!("Line with no numbers")),
        })
        .sum()
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

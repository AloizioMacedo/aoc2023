use anyhow::{anyhow, Result};

const INPUT: &str = include_str!("../input.txt");

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

fn solve_part_one(contents: &str) -> Result<u32> {
    contents.lines().map(parse_line).sum()
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input.txt");

    #[test]
    fn parsing() {
        assert_eq!(parse_line("1abc2").unwrap(), 12);
        assert_eq!(parse_line("pqr3stu8vwx").unwrap(), 38);
        assert_eq!(parse_line("a1b2c3d4e5f").unwrap(), 15);
        assert_eq!(parse_line("treb7uchet").unwrap(), 77);
    }

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 142);
    }
}

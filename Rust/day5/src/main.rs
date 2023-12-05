use anyhow::{anyhow, Result};
use itertools::Itertools;

const INPUT: &str = include_str!("../input.txt");

fn parse_contents(contents: &str) -> Result<(Vec<i64>, Vec<BlockData>)> {
    let mut blocks = contents.split("\n\n").peekable();

    let first_block = blocks.peek().ok_or(anyhow!("ERROR: No seeds block."))?;
    let (_, numbers) = first_block
        .split_once(':')
        .ok_or(anyhow!("ERROR: Syntax error at block {first_block}"))?;

    let numbers: Vec<i64> = numbers.split_whitespace().flat_map(|n| n.parse()).collect();

    let block_datas = blocks.skip(1).flat_map(parse_block).collect();

    Ok((numbers, block_datas))
}

#[derive(Debug)]
struct BlockData<'a> {
    _from: &'a str,
    _to: &'a str,
    numbers: Vec<Numbers>,
}

impl<'a> BlockData<'a> {
    fn map(&self, from: i64) -> i64 {
        for number in &self.numbers {
            if from >= number.source && from < number.source + number.range as i64 {
                return from - number.source + number.destination;
            }
        }

        from
    }
}

fn parse_block(block: &str) -> Result<BlockData> {
    let first_line = block
        .lines()
        .next()
        .ok_or(anyhow!("ERROR: Block does not have lines"))?;

    let (x_to_y, _) = first_line
        .split_once(' ')
        .ok_or(anyhow!("ERROR: Syntax error at '{block}'"))?;

    let (from, _, to) = x_to_y
        .splitn(3, '-')
        .collect_tuple()
        .ok_or(anyhow!("ERROR: Syntax error at '{block}'"))?;

    let numbers: Vec<Numbers> = block.lines().skip(1).flat_map(parse_numbers).collect();

    Ok(BlockData {
        _from: from,
        _to: to,
        numbers,
    })
}

#[derive(Debug)]
struct Numbers {
    destination: i64,
    source: i64,
    range: usize,
}

fn parse_numbers(line: &str) -> Result<Numbers> {
    let (destination, source, range) = line
        .splitn(3, ' ')
        .collect_tuple()
        .ok_or(anyhow!("ERROR: Syntax error at '{line}'"))?;

    let destination = destination.parse()?;
    let source = source.parse()?;
    let range = range.parse()?;

    Ok(Numbers {
        destination,
        source,
        range,
    })
}

pub fn solve_part_one(contents: &str) -> Result<i64> {
    let (seeds, blocks) = parse_contents(contents)?;

    let mut final_locations = Vec::new();

    for seed in seeds {
        let mut seed_mapping = seed;

        for block in &blocks {
            seed_mapping = block.map(seed_mapping);
        }

        final_locations.push(seed_mapping);
    }

    final_locations
        .iter()
        .copied()
        .min()
        .ok_or(anyhow!("Empty locations"))
}

pub fn solve_part_two(contents: &str) -> Result<i64> {
    let (seeds, blocks) = parse_contents(contents)?;
    // let seeds = convert_seed_ranges_into_seeds(&seeds);

    let mut final_locations = i64::MAX;

    for seed_chunk in seeds.chunks_exact(2) {
        let (start, range) = seed_chunk
            .iter()
            .collect_tuple()
            .expect("Chunk should be two-sized");

        for i in 0..(*range as usize) {
            let seed = start + i as i64;
            let mut seed_mapping = seed;

            for block in &blocks {
                seed_mapping = block.map(seed_mapping);
            }

            if seed_mapping < final_locations {
                final_locations = seed_mapping
            }
        }
    }

    Ok(final_locations)
}

// Leaving this here as it illustrates that allocation messed up the brute force
// approach.
fn _convert_seed_ranges_into_seeds(seed_ranges: &[i64]) -> Vec<i64> {
    seed_ranges.chunks_exact(2).fold(vec![], |mut acc, chunk| {
        let (start, range) = chunk
            .iter()
            .collect_tuple()
            .expect("Chunks should be two-sized");

        for i in 0..(*range as usize) {
            acc.push(start + i as i64)
        }

        acc
    })
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT: &str = include_str!("../test_input.txt");

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 35);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST_INPUT).unwrap(), 46);
    }
}

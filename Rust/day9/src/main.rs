use std::collections::VecDeque;

use anyhow::Result;

const INPUT: &str = include_str!("../input.txt");

struct History {
    values: VecDeque<i32>,
}

impl History {
    fn get_diffs(&self) -> VecDeque<i32> {
        self.values
            .iter()
            .skip(1)
            .zip(&self.values)
            .map(|(next, current)| next - current)
            .collect()
    }

    fn forecast_base(&self, parents: &[i32]) -> Option<i32> {
        let last_parent = parents.last()?;

        let last_value = self.values.back()?;

        Some(last_parent + last_value)
    }

    fn forecast(&self) -> Option<i32> {
        let mut diffs: Vec<History> = vec![History {
            values: self.values.clone(),
        }];

        while !diffs.last().unwrap().values.iter().all(|&x| x == 0) {
            let last_diffs = diffs.last()?;
            let values = last_diffs.get_diffs();

            diffs.push(History { values })
        }

        diffs.last_mut().map(|x| x.values.push_back(0));

        for i in 0..(diffs.len() - 1) {
            let idx = diffs.len() - i - 1;
            let seeing = &diffs[idx].values;

            let last_from_seeing = *seeing.back()?;
            let last_from_parents = *diffs[idx - 1].values.back()?;

            diffs[idx - 1]
                .values
                .push_back(last_from_seeing + last_from_parents);
        }

        diffs[0].values.back().map(|x| *x)
    }
}

fn parse_line(line: &str) -> Result<History> {
    let values = line.split(' ').flat_map(|c| c.parse()).collect();

    Ok(History { values })
}

fn parse_contents(contents: &str) -> Result<Vec<History>> {
    contents.lines().map(parse_line).collect()
}

fn solve_part_one(contents: &str) -> Result<i32> {
    let histories = parse_contents(contents)?;

    Ok(histories
        .iter()
        .flat_map(|history| history.forecast())
        .sum())
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT: &str = include_str!("../test_input.txt");

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 114)
    }
}

# Day 9

Straightforward day: just create the diffs until you hit all zeroes, then go back
adding things bottom up.

Part two can even be done by simply reversing the input. However, we do it by
implementing the lines as `VecDeque`s.

## Part One and Two

```rust
use std::collections::VecDeque;

use anyhow::Result;

const INPUT: &str = include_str!("../input.txt");

enum TimeDirection {
    Future,
    Past,
}

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

    fn forecast_base_back(&self, parents: &[i32]) -> Option<i32> {
        let last_parent = parents.first()?;

        let last_value = self.values.front()?;

        Some(last_parent + last_value)
    }

    fn forecast(&self, time_direction: TimeDirection) -> Option<i32> {
        let mut diffs: Vec<History> = vec![History {
            values: self.values.clone(),
        }];

        while !diffs
            .last()
            .expect("Diffs have at least the initial history")
            .values
            .iter()
            .all(|&x| x == 0)
        {
            let last_diffs = diffs.last()?;
            let values = last_diffs.get_diffs();

            diffs.push(History { values })
        }

        match time_direction {
            TimeDirection::Future => diffs.last_mut().map(|x| x.values.push_back(0)),
            TimeDirection::Past => diffs.last_mut().map(|x| x.values.push_front(0)),
        };

        for i in 0..(diffs.len() - 1) {
            let idx = diffs.len() - i - 1;
            let seeing = &diffs[idx].values;

            match time_direction {
                TimeDirection::Future => {
                    let last_from_seeing = *seeing.back()?;
                    let last_from_parents = *diffs[idx - 1].values.back()?;

                    diffs[idx - 1]
                        .values
                        .push_back(last_from_parents + last_from_seeing);
                }
                TimeDirection::Past => {
                    let last_from_seeing = *seeing.front()?;
                    let last_from_parents = *diffs[idx - 1].values.front()?;

                    diffs[idx - 1]
                        .values
                        .push_front(last_from_parents - last_from_seeing);
                }
            }
        }

        match time_direction {
            TimeDirection::Future => diffs[0].values.back().map(|x| *x),
            TimeDirection::Past => diffs[0].values.front().map(|x| *x),
        }
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
        .flat_map(|history| history.forecast(TimeDirection::Future))
        .sum())
}

fn solve_part_two(contents: &str) -> Result<i32> {
    let histories = parse_contents(contents)?;

    Ok(histories
        .iter()
        .flat_map(|history| history.forecast(TimeDirection::Past))
        .sum())
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
        assert_eq!(solve_part_one(TEST_INPUT).unwrap(), 114)
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two("10  13  16  21  30  45").unwrap(), 5)
    }
}
```

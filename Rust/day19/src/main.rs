mod interval_arithmetic;

use std::str::FromStr;

use anyhow::{anyhow, Result};
use itertools::Itertools;

use interval_arithmetic::interval_diff;

const INPUT: &str = include_str!("../input.txt");

#[derive(Debug)]
struct Workflow<'a> {
    name: &'a str,
    conditionals: Vec<Conditional<'a>>,
    finally: Outcome<'a>,
}

#[derive(Debug)]
struct Conditional<'a> {
    comparison: Comparison,
    category: Category,
    value: i64,
    outcome: Outcome<'a>,
}

#[derive(Debug, Clone, Copy)]
enum Category {
    X,
    M,
    A,
    S,
}

impl FromStr for Category {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "x" => Ok(Category::X),
            "m" => Ok(Category::M),
            "a" => Ok(Category::A),
            "s" => Ok(Category::S),
            _ => Err(anyhow!("Category does not exist")),
        }
    }
}

impl From<Category> for usize {
    fn from(value: Category) -> Self {
        match value {
            Category::X => 0,
            Category::M => 1,
            Category::A => 2,
            Category::S => 3,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Comparison {
    Greater,
    Lesser,
}

#[derive(Debug, Clone, Copy)]
enum Outcome<'a> {
    Destination(&'a str),
    Accepted,
    Rejected,
}

fn parse_workflow_block(block: &str) -> Result<Vec<Workflow>> {
    block.lines().map(parse_workflow_line).collect()
}

fn parse_workflow_line(line: &str) -> Result<Workflow> {
    let (name, rest) = line
        .split_once('{')
        .ok_or(anyhow!("Syntax error in line {line}"))?;

    let rest = rest.trim_end_matches('}');

    let conditionals = rest.split(',').collect::<Vec<_>>();

    let (finally, raw_conditionals) = conditionals
        .split_last()
        .ok_or(anyhow!("Syntax error in line {line}"))?;

    let finally = match *finally {
        "A" => Outcome::Accepted,
        "R" => Outcome::Rejected,
        x => Outcome::Destination(x),
    };

    let mut conditionals = Vec::new();

    for conditional in raw_conditionals {
        let (category, value, destination) = conditional
            .splitn(3, &['>', '<', ':'])
            .collect_tuple()
            .ok_or(anyhow!("Syntax error in line {line}"))?;

        let category = category.parse()?;

        let outcome = match destination {
            "A" => Outcome::Accepted,
            "R" => Outcome::Rejected,
            x => Outcome::Destination(x),
        };

        let comparison = if conditional.contains('>') {
            Comparison::Greater
        } else {
            Comparison::Lesser
        };

        let value = value.parse()?;

        let conditional = Conditional {
            comparison,
            category,
            value,
            outcome,
        };

        conditionals.push(conditional);
    }

    Ok(Workflow {
        conditionals,
        name,
        finally,
    })
}

fn parse_part_block(block: &str) -> Result<Vec<[i64; 4]>> {
    block.lines().map(parse_part_line).collect()
}

fn parse_part_line(line: &str) -> Result<[i64; 4]> {
    let line = line.trim_matches(|c| c == '{' || c == '}');

    let (value1, value2, value3, value4) = line
        .splitn(4, ',')
        .collect_tuple()
        .ok_or(anyhow!("Syntax error in line {line}"))?;

    let (_, value1) = value1
        .split_once('=')
        .ok_or(anyhow!("Syntax error in line {line}"))?;
    let (_, value2) = value2
        .split_once('=')
        .ok_or(anyhow!("Syntax error in line {line}"))?;
    let (_, value3) = value3
        .split_once('=')
        .ok_or(anyhow!("Syntax error in line {line}"))?;
    let (_, value4) = value4
        .split_once('=')
        .ok_or(anyhow!("Syntax error in line {line}"))?;

    let value1 = value1.parse()?;
    let value2 = value2.parse()?;
    let value3 = value3.parse()?;
    let value4 = value4.parse()?;

    Ok([value1, value2, value3, value4])
}

fn parse_contents(contents: &str) -> Result<(Vec<Workflow>, Vec<[i64; 4]>)> {
    let (workflow_block, part_block) = contents
        .split_once("\n\n")
        .ok_or(anyhow!("Blocks not separated by a line"))?;

    let workflows = parse_workflow_block(workflow_block)?;
    let parts = parse_part_block(part_block)?;

    Ok((workflows, parts))
}

fn is_accepted(part: &[i64; 4], workflows: &[Workflow]) -> bool {
    let mut candidate = workflows.iter().find(|x| x.name == "in");

    'outer: while let Some(next) = candidate {
        for Conditional {
            comparison,
            category,
            value,
            outcome,
        } in &next.conditionals
        {
            let num_to_compare: i64 = part[usize::from(*category)];
            let should_go_to_outcome = match comparison {
                Comparison::Greater => num_to_compare > *value,
                Comparison::Lesser => num_to_compare < *value,
            };

            if should_go_to_outcome {
                match outcome {
                    Outcome::Destination(name) => {
                        candidate = workflows.iter().find(|x| &x.name == name);
                        continue 'outer;
                    }
                    Outcome::Accepted => return true,
                    Outcome::Rejected => return false,
                }
            }
        }

        match next.finally {
            Outcome::Destination(name) => {
                candidate = workflows.iter().find(|x| x.name == name);
                continue 'outer;
            }
            Outcome::Accepted => return true,
            Outcome::Rejected => return false,
        }
    }

    false
}

trait IntervalLength {
    fn size(&self) -> usize;
}

impl IntervalLength for (i64, i64) {
    fn size(&self) -> usize {
        (self.1 - self.0 + 1) as usize
    }
}

fn get_diff(intervals: &[(i64, i64)], b: (i64, i64)) -> Vec<(i64, i64)> {
    if b == (0, -1) {
        return intervals.to_vec();
    }

    let mut result = Vec::new();
    for interval in intervals {
        if b.0 <= interval.0 && b.1 >= interval.1 {
            result.push((0, -1));
        } else if b.0 > interval.0 && b.1 >= interval.1 {
            result.push((interval.0, b.0 - 1));
        } else if b.0 > interval.0 && b.1 < interval.1 {
            result.push((interval.0, b.0 - 1));
            result.push((b.1 + 1, interval.1));
        } else if b.0 <= interval.0 && b.1 < interval.1 {
            result.push((b.1 + 1, interval.1));
        }
    }

    result
}

fn calculate_total(part_range: &[(i64, i64); 4], workflows: &[Workflow], start_at: &str) -> usize {
    let candidate = workflows.iter().find(|x| x.name == start_at);

    if part_range.iter().any(|p| p.size() == 0) {
        return 0;
    }

    let mut x_range = part_range[0];
    let mut m_range = part_range[1];
    let mut a_range = part_range[2];
    let mut s_range = part_range[3];

    let mut x_exc_ranges = vec![part_range[0]];
    let mut m_exc_ranges = vec![part_range[1]];
    let mut a_exc_ranges = vec![part_range[2]];
    let mut s_exc_ranges = vec![part_range[3]];

    let mut total = 0;

    if let Some(next) = candidate {
        for Conditional {
            comparison,
            category,
            value,
            outcome,
        } in &next.conditionals
        {
            if start_at == "m" {
                println!("{:?}", m_range);
            }

            let value = *value;

            let original_range = part_range[usize::from(*category)];

            let (left_bound, right_bound) = match category {
                Category::X => (x_range.0, x_range.1),
                Category::M => (m_range.0, m_range.1),
                Category::A => (a_range.0, a_range.1),
                Category::S => (s_range.0, s_range.1),
            };

            let relevant_inc_range = match category {
                Category::X => &mut x_range,
                Category::M => &mut m_range,
                Category::A => &mut a_range,
                Category::S => &mut s_range,
            };

            let relevant_exc_ranges = match category {
                Category::X => &mut x_exc_ranges,
                Category::M => &mut m_exc_ranges,
                Category::A => &mut a_exc_ranges,
                Category::S => &mut s_exc_ranges,
            };

            match comparison {
                Comparison::Greater => {
                    if value >= left_bound && value < right_bound {
                        (relevant_inc_range.0, relevant_inc_range.1) = (value + 1, right_bound);
                    } else if value < left_bound {
                        (relevant_inc_range.0, relevant_inc_range.1) = (left_bound, right_bound);
                    } else {
                        (relevant_inc_range.0, relevant_inc_range.1) = (0, -1);
                    }
                }
                Comparison::Lesser => {
                    if value > left_bound && value <= right_bound {
                        (relevant_inc_range.0, relevant_inc_range.1) = (left_bound, value - 1);
                    } else if value > right_bound {
                        (relevant_inc_range.0, relevant_inc_range.1) = (left_bound, right_bound);
                    } else {
                        (relevant_inc_range.0, relevant_inc_range.1) = (0, -1);
                    }
                }
            };

            let diffs = get_diff(&[original_range], *relevant_inc_range);
            relevant_exc_ranges.clear();
            relevant_exc_ranges.extend(diffs);

            match outcome {
                Outcome::Destination(next_start) => match category {
                    Category::X => {
                        for (&m_exc_range, &a_exc_range, &s_exc_range) in
                            itertools::iproduct!(&m_exc_ranges, &a_exc_ranges, &s_exc_ranges)
                        {
                            total += calculate_total(
                                &[x_range, m_exc_range, a_exc_range, s_exc_range],
                                workflows,
                                next_start,
                            );
                        }
                    }
                    Category::M => {
                        for (&x_exc_range, &a_exc_range, &s_exc_range) in
                            itertools::iproduct!(&x_exc_ranges, &a_exc_ranges, &s_exc_ranges)
                        {
                            total += calculate_total(
                                &[x_exc_range, m_range, a_exc_range, s_exc_range],
                                workflows,
                                next_start,
                            );
                        }
                    }
                    Category::A => {
                        for (&x_exc_range, &m_exc_range, &s_exc_range) in
                            itertools::iproduct!(&x_exc_ranges, &m_exc_ranges, &s_exc_ranges)
                        {
                            total += calculate_total(
                                &[x_exc_range, m_exc_range, a_range, s_exc_range],
                                workflows,
                                next_start,
                            );
                        }
                    }
                    Category::S => {
                        for (&x_exc_range, &m_exc_range, &a_exc_range) in
                            itertools::iproduct!(&x_exc_ranges, &m_exc_ranges, &a_exc_ranges)
                        {
                            total += calculate_total(
                                &[x_exc_range, m_exc_range, a_exc_range, s_range],
                                workflows,
                                next_start,
                            );
                        }
                    }
                },
                Outcome::Accepted => match category {
                    Category::X => {
                        for (&m_exc_range, &a_exc_range, &s_exc_range) in
                            itertools::iproduct!(&m_exc_ranges, &a_exc_ranges, &s_exc_ranges)
                        {
                            total += x_range.size()
                                * m_exc_range.size()
                                * a_exc_range.size()
                                * s_exc_range.size();
                        }
                    }
                    Category::M => {
                        for (&x_exc_range, &a_exc_range, &s_exc_range) in
                            itertools::iproduct!(&x_exc_ranges, &a_exc_ranges, &s_exc_ranges)
                        {
                            total += x_exc_range.size()
                                * m_range.size()
                                * a_exc_range.size()
                                * s_exc_range.size();
                        }
                    }
                    Category::A => {
                        for (&x_exc_range, &m_exc_range, &s_exc_range) in
                            itertools::iproduct!(&x_exc_ranges, &m_exc_ranges, &s_exc_ranges)
                        {
                            total += x_exc_range.size()
                                * m_exc_range.size()
                                * a_range.size()
                                * s_exc_range.size();
                        }
                    }
                    Category::S => {
                        for (&x_exc_range, &m_exc_range, &a_exc_range) in
                            itertools::iproduct!(&x_exc_ranges, &m_exc_ranges, &a_exc_ranges)
                        {
                            total += x_exc_range.size()
                                * m_exc_range.size()
                                * a_exc_range.size()
                                * s_range.size();
                        }
                    }
                },
                Outcome::Rejected => (),
            }
        }

        match next.finally {
            Outcome::Destination(next_start) => {
                for (x_exc_range, m_exc_range, a_exc_range, s_exc_range) in
                    itertools::iproduct!(x_exc_ranges, m_exc_ranges, a_exc_ranges, s_exc_ranges)
                {
                    total += calculate_total(
                        &[x_exc_range, m_exc_range, a_exc_range, s_exc_range],
                        workflows,
                        next_start,
                    );
                }
            }
            Outcome::Accepted => {
                for (x_exc_range, m_exc_range, a_exc_range, s_exc_range) in
                    itertools::iproduct!(x_exc_ranges, m_exc_ranges, a_exc_ranges, s_exc_ranges)
                {
                    total += x_exc_range.size()
                        * m_exc_range.size()
                        * a_exc_range.size()
                        * s_exc_range.size();
                }
            }
            Outcome::Rejected => (),
        }
    }

    total
}

fn solve_part_one(contents: &str) -> Result<i64> {
    let (workflows, parts) = parse_contents(contents)?;

    Ok(parts
        .iter()
        .filter(|p| is_accepted(p, &workflows))
        .map(|p| p.iter().sum::<i64>())
        .sum())
}

fn solve_part_two(contents: &str) -> Result<usize> {
    let (workflows, _) = parse_contents(contents)?;

    Ok(calculate_total(
        &[(1, 4000), (1, 4000), (1, 4000), (1, 4000)],
        &workflows,
        "in",
    ))
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);
    println!("{}", solve_part_two(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input.txt");

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 19114);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST).unwrap(), 167409079868000);
    }
}

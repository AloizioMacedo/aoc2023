use anyhow::{anyhow, Result};

const TIMES: [u64; 4] = [40, 81, 77, 72];
const DISTANCES: [u64; 4] = [219, 1012, 1365, 1089];

fn parse_line(line: &str) {}

fn parse_contents(contents: &str) {}

fn get_number_of_ways(max_time: u64, min_distance: u64) -> usize {
    (0..max_time)
        .filter(|h| (max_time - h) * h > min_distance)
        .count()
}

fn solve_part_one(times: &[u64], distances: &[u64]) -> usize {
    times
        .iter()
        .zip(distances)
        .map(|(t, d)| {
            let n = get_number_of_ways(*t, *d);
            println!("{}", n);
            n
        })
        .product()
}

fn main() {
    println!("{}", solve_part_one(&TIMES, &DISTANCES));
    println!("{}", get_number_of_ways(40817772, 219101213651089));
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT: &str = include_str!("../test_input.txt");
    const TEST_TIMES: [u64; 3] = [7, 15, 30];
    const TEST_DISTANCES: [u64; 3] = [9, 40, 200];

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(&TEST_TIMES, &TEST_DISTANCES), 288);
    }
}

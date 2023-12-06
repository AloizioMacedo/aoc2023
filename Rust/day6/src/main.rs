const TIMES: [u64; 4] = [40, 81, 77, 72];
const DISTANCES: [u64; 4] = [219, 1012, 1365, 1089];

fn get_number_of_ways(max_time: u64, min_distance: u64) -> usize {
    (0..max_time)
        .filter(|h| (max_time - h) * h > min_distance)
        .count()
}

fn solve_part_one(times: &[u64], distances: &[u64]) -> usize {
    times
        .iter()
        .zip(distances)
        .map(|(t, d)| get_number_of_ways(*t, *d))
        .product()
}

fn solve_part_two(max_time: u64, min_distance: u64) -> usize {
    get_number_of_ways(max_time, min_distance)
}

fn main() {
    println!("{}", solve_part_one(&TIMES, &DISTANCES));
    println!("{}", solve_part_two(40817772, 219101213651089));
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_TIMES: [u64; 3] = [7, 15, 30];
    const TEST_DISTANCES: [u64; 3] = [9, 40, 200];

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(&TEST_TIMES, &TEST_DISTANCES), 288);
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(71530, 940200), 71503);
    }
}

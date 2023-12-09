# Day 5

Day 5 is essentially a problem about how to efficiently compose functions.

## Part One

Each block determines a remapping of integers. Conveniently, the blocks are ordered
correctly and thus simplify potentially having to path-find the correct steps
of transition.

The time complexity for our implementation is \\(O(n\\cdot k \\cdot p)\\), where
\\(n\\) is the number of seeds, \\(k\\) is the number of mappings and \\(p\\) is the
maximum number of lines of each block.

The main parts of the algorithm are the block's implementation:

```rust
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

    fn reverse_map(&self, to: i64) -> i64 {
        for number in &self.numbers {
            if to >= number.destination && to < number.destination + number.range as i64 {
                return to - number.destination + number.source;
            }
        }

        to
    }
}
```

and the following iteration:

```rust
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
```

## Part Two

Part two flips the problem in its head by establishing that the "seeds" are actually
pairs, where each first element is the start of a range and the second is the range
length. This significantly increases the input size (i.e., \\(n\\) in our time
complexity consideration), but does not change neither \\(k\\) or \\(p\\). For all
intents and purposes, the algorithm is simply \\(O(n)\\).

Notice that our input is in the order of a few billions. Therefore, brute force should
not be out of the picture. And indeed, we can run the following function

```rust
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
```

and get a solution in a few minutes. If we reverse the logic and try to find the
first value that is the image of something in the input, we can get the response
in a few seconds.

```rust
pub fn solve_part_two_reversing(contents: &str) -> Result<i64> {
    let (seeds, blocks) = parse_contents(contents)?;
    // let seeds = convert_seed_ranges_into_seeds(&seeds);

    let chunks: Vec<(i64, i64)> = seeds
        .chunks_exact(2)
        .map(|chunk| {
            chunk
                .iter()
                .copied()
                .collect_tuple()
                .expect("Chunk should be two-sized")
        })
        .collect();

    for i in 0.. {
        let mut attempt = i;

        for block in blocks.iter().rev() {
            attempt = block.reverse_map(attempt);
        }

        for (start, range) in &chunks {
            if attempt >= *start && attempt < start + range {
                return Ok(i);
            }
        }
    }

    Err(anyhow!("Unable to find best location"))
}
```

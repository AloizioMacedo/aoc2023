# Day 8

This day is essentially implementing a specific walk (or _walks_ in part two) over
a binary tree. Conveniently, we have labels for the nodes, so we can just store them
as a `HashMap`.

Part two has some problematics aspect to it, but let's get into that later.

## Part One

For our parsing, we do

```rust
#[derive(Debug)]
struct Possibilities<'a> {
    left: &'a str,
    right: &'a str,
}

fn parse_line(line: &str) -> Result<(&str, Possibilities)> {
    let (origin, destinations) = line
        .split_once(" = ")
        .ok_or(anyhow!("Syntax error at {line}"))?;

    let (left, right) = destinations
        .trim_end_matches(')')
        .trim_start_matches('(')
        .split_once(", ")
        .ok_or(anyhow!("Syntax error at {line}"))?;

    Ok((origin, Possibilities { left, right }))
}

fn parse_contents(contents: &str) -> Result<(Vec<Direction>, HashMap<&str, Possibilities>)> {
    let (first_line, rest) = contents.split_once("\n\n").ok_or(anyhow!("Syntax error"))?;

    let directions: Vec<Direction> = first_line.chars().flat_map(Direction::try_from).collect();
    let parsed_lines: Vec<(&str, Possibilities)> = rest.lines().flat_map(parse_line).collect();

    let transitions = HashMap::from_iter(parsed_lines);

    Ok((directions, transitions))
}
```

We then define how the algorithm operates, and that's it!

```rust
fn go_one_step<'a>(
    from: &'a str,
    direction: &Direction,
    map: &HashMap<&'a str, Possibilities<'a>>,
) -> &'a str {
    match direction {
        Direction::L => map[from].left,
        Direction::R => map[from].right,
    }
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let (directions, transitions) = parse_contents(contents)?;

    let mut current = "AAA";
    Ok(directions
        .iter()
        .cycle()
        .map(|d| {
            let next = go_one_step(current, d, &transitions);

            if next == "ZZZ" {
                None
            } else {
                current = next;
                Some(current)
            }
        })
        .while_some()
        .count()
        + 1)
}
```

## Part Two

Part two operates with multiple walks. We want to see where those walks coordinate
to finish at something that ends with `Z` at the same time.

Since that can take a lot of time, the idea is to get the cycle lengths of each of the
walks and get the least common multiple to find the cycle length of the whole ensemble.

Consider the following example as an illustration:

> 1 2 3 4 5 **6** 1 2 3 4 5 **6** 1 2 3 4 5 **6**  
> 1 2 3 **4** 1 2 3 **4** 1 2 3 **4** 1 2 3 **4** 1 2  
> 1 2 **3** 1 2 **3** 1 2 **3** 1 2 **3** 1 2 **3** 1 2 **3**

As you can see, the cycles coordinate at the twelfth position.

However, the structure above is not necessarily what we have given the problem's
description. Indeed, even the examples given do not reflect this structure.

We could have the following:

> 1 2 _3_ 4 5 **6** _3_ 4 5 **6** _3_ 4 5 **6** _3_ 4 5 **6**  
> 1 2 3 4 5 6 _7_ 8 **9** _7_ 8 **9** _7_ 8 **9** _7_ 8 **9**  
> 1 2 _3_ **4** _3_ **4** _3_ **4** _3_ **4** _3_ **4** _3_ **4** _3_ **4** _3_ **4**

The real solution to the problem would involve:

1. Detecting the cycle length of each walk
1. Reducing those cycle lengths to coprime factors
1. Applying the [Chinese Remainder Theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem)
   to find the beginning of the joint cycle

This way, we reduce the problem to one of a similar structure as the previous
illustration, the only difference being the existence of an initial offset that is
found using the Chinese Remainder Theorem.

Note that 2. assumes that the cycle lengths are coprime. This is yet another thing
that the problem assumes which might not be true for a generic input. If that is
not true, the following situation could happen:

> 1 2 3 **4** 1 2 3 **4** 1 2 3 **4** 1  
> \_ 1 2 3 **4** 1 2 3 **4** 1 2 3 **4**

The cycles would never coordinate in the scenario above, and the problem wouldn't
have a solution.

"Luckily", the input is built in such a way that just taking the LCM of the cycles
work out well to define the cycle. My solution to this day assumes that the LCM works
too. If you want to have a solution that would apply to all possible inputs, you'd
have to consider the points mentioned above, including the possibility of a solution
not existing.

# Day 10

This day is very interesting, and boils down to two things:

1. Loop finding (Part One)
1. Flood filling (Part Two)

## Part One

We create a graph from the given grid. Pasting the full code here would not be very
enlightening, since it is a lot of specific case handling.

It is useful, though, to show the loop-finding algorithm. I did not implement it as
a BFS. It is more of a DFS. The code is below.

```rust
pub(crate) fn find_loop(
    graph: &UnGraphMap<(usize, usize), ()>,
    beginning: (usize, usize),
) -> Result<Vec<(usize, usize)>> {
    let mut queue = vec![vec![beginning]];

    while let Some(next) = queue.pop() {
        let last = *next.last().expect("Paths should not be empty");

        if last == beginning && next.len() > 1 {
            return Ok(next);
        }

        let neighbors: Vec<(usize, usize)> = graph
            .neighbors(last)
            .filter(|n| {
                if next.len() < 2 {
                    return true;
                }
                if let Some(x) = next.get(next.len() - 2) {
                    n != x
                } else {
                    true
                }
            })
            .collect();

        for neighbor in neighbors {
            let mut path = next.clone();
            path.push(neighbor);

            queue.push(path);
        }
    }

    Err(anyhow!("ERROR: Loop not found"))
}
```

The solution function is then:

```rust
fn solve_part_one(contents: &str) -> Result<usize> {
    let grid = parse_contents(contents)?;
    let lp = pipe::find_loop(&grid.get_graph(), grid.origin)?;

    Ok((lp.len() - 1) / 2) // Subtract one to remove the origin.
}
```

## Part Two

Part two is implemented as a simple flood-filling algorithm. There is a complication
though, since we allow fluid to "squeeze" through pipes such as the following

> . . . .  
> . | | .  
> . | | .  
> F J L 7  
> | . . |  
> L - - J

To be very explicit, filling this would result in the following grid

> O O O O  
> O | | O  
> O | | O  
> F J L 7  
> | O O |  
> L - - J

The way that we solved this is by zooming into the grid: every tile got transformed
into a 3x3 grid itself.

For instance, "J"s get transformed into

> . | .  
> \_ J .  
> . . .

We then flood fill this new grid and then scale it down again.

The main part of the code is given below

```rust
fn solve_part_two(contents: &str) -> Result<usize> {
    let grid = parse_contents(contents)?;
    let lp = pipe::find_loop(&grid.get_graph(), grid.origin)?;

    let scaled_up = grid::scale_up(&grid.matrix);
    let transformed_matrix = grid::transform_matrix(&scaled_up, &lp);

    let filled = grid::flood_fill(&transformed_matrix);
    let scaled_down = grid::scale_down(&filled);

    Ok(scaled_down
        .iter()
        .filter(|e| matches!(e, grid::Filled::Empty))
        .count())
}
```

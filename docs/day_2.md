# Day 2

This is a great problem for setting up some structure!

First of all, our cubes have three colors: "red", "green" and "blue".

Those cubes are revealed as sets in a given game, where each game might have an
arbitrary number of such sets.

Then, those games are communicated as lines in the following format:

```
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
```

So we need some parsing as well. All in all, the idea is to build:

1. A Color enum
1. A `Set` struct containing information about the colors present in the `Set`
1. A Game struct with a vec of such sets
1. Parsing of each line

For point number 2 above, I decided to go with a `HashMap` directly for simplicity and
in order to be able to think separately about the parsing and the rest, but there
is no need for extra memory allocation: we should be able to handle this by embedding
the count in the enum itself. Let's refactor this later, though, together with the error
handling and for now let's show the submission how it was done.

## Part One

Setting up the whole structure is the most problematic aspect of part one, as the logic
is trivial: we want to check that the constraints are valid in each set of a game.

For the cubes, we have the following:

```rust
const RED_CUBES: usize = 12;
const GREEN_CUBES: usize = 13;
const BLUE_CUBES: usize = 14;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
enum Color {
    Red,
    Green,
    Blue,
}

impl FromStr for Color {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "red" => Ok(Self::Red),
            "green" => Ok(Self::Green),
            "blue" => Ok(Self::Blue),
            _ => Err(anyhow!("Invalid color")),
        }
    }
}

impl Color {
    fn get_max(&self) -> usize {
        match self {
            Self::Red => RED_CUBES,
            Self::Green => GREEN_CUBES,
            Self::Blue => BLUE_CUBES,
        }
    }
}
```

For the `Set`, we have this:

```rust

#[derive(Debug)]
struct Set {
    cubes: HashMap<Color, usize>,
}

impl Set {
    fn is_possible(&self) -> bool {
        self.cubes.iter().all(|(k, v)| v <= &k.get_max())
    }
}
```

And for the `Game`, this:

```rust
#[derive(Debug)]
struct Game<'a> {
    id: &'a str,
    sets: Vec<Set>,
}

impl<'a> Game<'a> {
    fn is_possible(&self) -> bool {
        self.sets.iter().all(|set| set.is_possible())
    }
}
```

The above structure seems sensible, but the parsing below could use some improvement
so that the syntax required from the lines is more self-evident.

```rust
fn parse_line(line: &str) -> Game {
    let line: Vec<&str> = line.split(&[':', ';']).collect();

    let id = line[0].split(' ').nth(1).expect("Game should have id");

    let mut sets = Vec::new();

    for set in &line[1..] {
        let mut cubes = HashMap::new();
        let cubes_as_strings: Vec<&str> = set.split(", ").map(|x| x.trim()).collect();

        for cube in cubes_as_strings {
            let (number, color) = cube
                .split_once(' ')
                .expect("Should have pair number, color");

            let color: Color = color.parse().expect("Color should exist");
            let number: usize = number.parse().expect("Number of cubes should make sense");

            cubes.insert(color, number);
        }

        let set = Set { cubes };
        sets.push(set);
    }

    Game { id, sets }
}
```

With our structure set up, the solution to part one becomes the following
self-explanatory function:

```rust
fn solve_part_one(contents: &str) -> usize {
    contents
        .lines()
        .map(parse_line)
        .filter(|game| game.is_possible())
        .map(|game| {
            game.id
                .parse::<usize>()
                .expect("Id should be convertible to int")
        })
        .sum()
}
```

## Part Two

Due to our structure, extending the code to contemplate part two is simple:

1. We need to extend `Game` to be able to return a `Set` with the max of each color.
1. `Set` should have a calculation of power.

For 1., we create the following method:

```rust
fn get_max_of_each_set(&self) -> Set {
    let mut result = HashMap::new();
    let max_red = self
        .sets
        .iter()
        .map(|set| set.cubes.get(&Color::Red).unwrap_or(&0))
        .max()
        .unwrap_or(&0);
    let max_blue = self
        .sets
        .iter()
        .map(|set| set.cubes.get(&Color::Blue).unwrap_or(&0))
        .max()
        .unwrap_or(&0);
    let max_green = self
        .sets
        .iter()
        .map(|set| set.cubes.get(&Color::Green).unwrap_or(&0))
        .max()
        .unwrap_or(&0);

    result.insert(Color::Red, *max_red);
    result.insert(Color::Blue, *max_blue);
    result.insert(Color::Green, *max_green);

    Set { cubes: result }
}
```

It is a little verbose but straightforward.

For 2., we just add the following method to `Set`:

```rust
fn get_power(&self) -> usize {
    self.cubes.values().fold(1, |acc, e| acc * e) // Should just be .product().
}
```

And now for the solution we can just run:

```rust
fn solve_part_two(contents: &str) -> usize {
    contents
        .lines()
        .map(parse_line)
        .map(|game| game.get_max_of_each_set())
        .map(|set| set.get_power())
        .sum()
}
```

### Refactoring Part Two

Let's now change the two points that I alluded to before:

1. Remove the `HashMap`.
1. Better error handling.

> Before moving on, it is worth mentioning that removing the HashMap is a non-trivial
> trade-off. Indeed, if we ever needed to accept arbitrary color names at run time,
> we would need to go back to `HashMap`.
>
> But getting rid of the `HashMap` allows us to reduce heap usage and optimize our
> code in general.

For the first part, we do the following changes:

```diff
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
enum Color {
-    Red,
-    Green,
-    Blue,
+    Red(usize),
+    Green(usize),
+    Blue(usize),
}
```

We will encode the number of cubes, and parse the relevant strings into this
format.

Indeed, this is our new parsing:

```diff
    fn from_str(s: &str) -> Result<Self> {
-        match s {
-            "red" => Ok(Self::Red),
-            "green" => Ok(Self::Green),
-            "blue" => Ok(Self::Blue),
-            _ => Err(anyhow!("Invalid color")),
-        }
+        let (num, color) = s
+            .split_once(' ')
+            .ok_or(anyhow!("Incorrect syntax for parsing {s}"))?;
+
+        let num = num.parse()?;
+
+        match color {
+            "red" => Ok(Self::Red(num)),
+            "green" => Ok(Self::Green(num)),
+            "blue" => Ok(Self::Blue(num)),
+            _ => Err(anyhow!("Invalid color")),
+        }
    }
```

And our set now changes to this:

```diff
#[derive(Debug)]
struct Set {
-    cubes: HashMap<Color, usize>,
+    red: usize,
+    green: usize,
+    blue: usize,
}

impl Set {
    fn is_possible(&self) -> bool {
-        self.cubes.iter().all(|(k, v)| v <= &k.get_max())
+        self.red <= RED_CUBES && self.green <= GREEN_CUBES && self.blue <= BLUE_CUBES
    }

    fn get_power(&self) -> usize {
-        self.cubes.values().product()
+        self.red * self.green * self.blue
    }
}
```

Our `get_max_of_each_set` changes to the the following:

```diff
    fn get_max_of_each_set(&self) -> Set {
-        let mut result = HashMap::new();
-        let max_red = self
-            .sets
-            .iter()
-            .map(|set| set.cubes.get(&Color::Red).unwrap_or(&0))
-            .max()
-            .unwrap_or(&0);
-        let max_blue = self
-            .sets
-            .iter()
-            .map(|set| set.cubes.get(&Color::Blue).unwrap_or(&0))
-            .max()
-            .unwrap_or(&0);
-        let max_green = self
-            .sets
-            .iter()
-            .map(|set| set.cubes.get(&Color::Green).unwrap_or(&0))
-            .max()
-            .unwrap_or(&0);
-
-        result.insert(Color::Red, *max_red);
-        result.insert(Color::Blue, *max_blue);
-        result.insert(Color::Green, *max_green);
-
-        Set { cubes: result }
+        let max_red = self.sets.iter().map(|set| set.red).max().unwrap_or(0);
+        let max_blue = self.sets.iter().map(|set| set.blue).max().unwrap_or(0);
+        let max_green = self.sets.iter().map(|set| set.green).max().unwrap_or(0);
+
+        Set {
+            red: max_red,
+            blue: max_blue,
+            green: max_green,
+        }
    }
```

And the bulk of our parsing changes like this:

```diff
    for set in &line[1..] {
-        let mut cubes = HashMap::new();
+        let mut red = 0;
+        let mut green = 0;
+        let mut blue = 0;

         let cubes_as_strings: Vec<&str> = set.split(", ").map(|x| x.trim()).collect();

         for cube in cubes_as_strings {
-            let (number, color) = cube
-                .split_once(' ')
-                .expect("Should have pair number, color");
-
-            let color: Color = color.parse().expect("Color should exist");
-            let number: usize = number.parse().expect("Number of cubes should make sense");
-
-            cubes.insert(color, number);
+            let color: Color = cube
+                .parse()
+                .expect("Cube should be a pair (number, color) but was {cube}");
+
+            match color {
+                Color::Red(x) => red = x,
+                Color::Green(x) => green = x,
+                Color::Blue(x) => blue = x,
+            }
        }

-        let set = Set { cubes };
+        let set = Set { red, green, blue };
         sets.push(set);
    }
```

For the error handling part, it is not very worth it to show the diffs, but it is
worth it to mention that by flattening our iterators we avoid crashing during
runtime. Errors are then logged so that we can see what may have gone wrong.

The whole code now reads as follows:

```rust
use std::str::FromStr;

use anyhow::{anyhow, Result};

const INPUT: &str = include_str!("../input.txt");

const RED_CUBES: usize = 12;
const GREEN_CUBES: usize = 13;
const BLUE_CUBES: usize = 14;

#[derive(Debug)]
enum Color {
    Red(usize),
    Green(usize),
    Blue(usize),
}

impl FromStr for Color {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let (num, color) = s
            .split_once(' ')
            .ok_or(anyhow!("Incorrect syntax for parsing {s}"))?;

        let num = num.parse()?;

        match color {
            "red" => Ok(Self::Red(num)),
            "green" => Ok(Self::Green(num)),
            "blue" => Ok(Self::Blue(num)),
            _ => Err(anyhow!("Invalid color")),
        }
    }
}

#[derive(Debug)]
struct Set {
    red: usize,
    green: usize,
    blue: usize,
}

impl Set {
    fn is_possible(&self) -> bool {
        self.red <= RED_CUBES && self.green <= GREEN_CUBES && self.blue <= BLUE_CUBES
    }

    fn get_power(&self) -> usize {
        self.red * self.green * self.blue
    }
}

#[derive(Debug)]
struct Game<'a> {
    id: &'a str,
    sets: Vec<Set>,
}

impl<'a> Game<'a> {
    fn is_possible(&self) -> bool {
        self.sets.iter().all(|set| set.is_possible())
    }

    fn get_max_of_each_set(&self) -> Set {
        let max_red = self.sets.iter().map(|set| set.red).max().unwrap_or(0);
        let max_blue = self.sets.iter().map(|set| set.blue).max().unwrap_or(0);
        let max_green = self.sets.iter().map(|set| set.green).max().unwrap_or(0);

        Set {
            red: max_red,
            blue: max_blue,
            green: max_green,
        }
    }
}

fn parse_line(line: &str) -> Result<Game> {
    let line: Vec<&str> = line.split(&[':', ';']).collect();

    let id = line[0]
        .split(' ')
        .nth(1)
        .ok_or(anyhow!("Error parsing id"))?;

    let mut sets = Vec::new();

    for set in &line[1..] {
        let mut red = 0;
        let mut green = 0;
        let mut blue = 0;

        let cubes_as_strings: Vec<&str> = set.split(", ").map(|x| x.trim()).collect();

        for cube in cubes_as_strings {
            let color: Color = cube.parse()?;

            match color {
                Color::Red(x) => red = x,
                Color::Green(x) => green = x,
                Color::Blue(x) => blue = x,
            }
        }

        let set = Set { red, green, blue };
        sets.push(set);
    }

    Ok(Game { id, sets })
}

pub fn solve_part_one(contents: &str) -> usize {
    contents
        .lines()
        .flat_map(|line| {
            parse_line(line).map_err(|e| {
                eprintln!("ERROR: Failed to parse line '{line}': {e}");
                e
            })
        })
        .filter(|game| game.is_possible())
        .flat_map(|game| {
            game.id.parse::<usize>().map_err(|e| {
                eprintln!("ERROR: Id '{}' can't be parsed into int: {e}", game.id);
                e
            })
        })
        .sum()
}

pub fn solve_part_two(contents: &str) -> usize {
    contents
        .lines()
        .flat_map(|line| {
            parse_line(line).map_err(|e| {
                eprintln!("ERROR: Failed to parse line '{line}': {e}");
                e
            })
        })
        .map(|game| game.get_max_of_each_set())
        .map(|game| game.get_power())
        .sum()
}
fn main() {
    println!("{}", solve_part_one(INPUT));
    println!("{}", solve_part_two(INPUT));
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../test_input.txt");

    #[test]
    fn it_works() {
        let line = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        println!("{:?}", parse_line(line));
    }

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST), 8)
    }

    #[test]
    fn part_two() {
        assert_eq!(solve_part_two(TEST), 2286)
    }
}
```

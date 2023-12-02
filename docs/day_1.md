# Day 1

Day 1 is mostly related to extracting numbers from a stream of chars.

## Part One

This is a simple filter, iterating over the chars and getting those that can be parsed
to digits.

```rust
use anyhow::{anyhow, Result};

fn parse_line(line: &str) -> Result<u32> {
    let mut numbers = line.chars().filter_map(|c| c.to_digit(10)).peekable();

    let first = *numbers
        .peek()
        .ok_or(anyhow!("Line {line} missing a number."))?;
    let last = numbers
        .last()
        .ok_or(anyhow!("Line {line} missing a number."))?;

    Ok(10 * first + last)
}
```

The only way to mess up the first part would be to do something like filtering out
all values between the first and the last, because then things like `akjsdk3ksdj` would
not become "33", but only "3" instead. But most organic solutions will naturally
deal with this edge case.

## Part Two

This part is more problematic than it appears. The first immediate thought is just
replacing the spelled out names with the numbers, e.g. `onelvjxcthree` -> `1lvjxc3`,
and then proceeding with the first part.

However, this fails due to the main friction of part two: characters can be reused
for different numbers, e.g. `sevenine` becomes `79`.

So, the initial idea that I had was to replace the spelled out number with the spelled
out number + the number itself. It wasn't apparently clear why it didn't work, but
the very same example above should make it obvious: `sevenine` would become
`seven7ine` in the first replacement, which would not generate the nine. Curiously,
if 9 came before seven in the iteration of the replacemens, there would be no problem!

Then next idea would be to maybe put the number in the middle of the word, but that
has some problems:

- It relies fundamentally in happenstances of the English language which might have
  to result in refactorings in case we switched the language;
- It creates new Strings every time we replace, with needless memory allocation.

Instead of doing that, I just ran a window through the string, capturing if it starts
with a number, and pushed the numbers coming from those windows to one single `Vec`.
If the window began with a char that is a number, I'd also add that to the `Vec`.

```rust
fn transform_line(line: &str) -> Vec<u32> {
    let mut numbers = Vec::new();

    // Adding gibberish at the end just to guarantee comfortable buffering.
    let fake_line = line.to_string() + "AAAAA";

    for i in 0..line.len() {
        for (j, spelled_out_number) in SPELLED_OUT_NUMBERS.iter().enumerate() {
            if fake_line[i..(i + 5)].starts_with(spelled_out_number) {
                numbers.push(j as u32 + 1);
                break;
            }
        }

        if let Some(x) = fake_line[i..(i + 5)]
            .chars()
            .next()
            .expect("Should not be empty given that it has AAAAA at the end")
            .to_digit(10)
        {
            numbers.push(x);
        }
    }

    numbers
}
```

This solves the problem, but let's make a change before moving on: the whole `AAAAA`
addition together with windows of length five is completely unnecessary, of course.
Also, using windows of length five couples our solution to english as well;
indeed, "quatro", which is "four" in Portuguese, already would violate that.

```rust
fn transform_line(line: &str) -> Vec<u32> {
    let mut numbers = Vec::new();

    for i in 0..line.len() {
        for (j, spelled_out_number) in SPELLED_OUT_NUMBERS.iter().enumerate() {
            if line[i..].starts_with(spelled_out_number) {
                numbers.push(j as u32 + 1);
                break;
            }
        }

        if let Some(x) = line[i..]
            .chars()
            .next()
            .expect("Should not be empty given that i < line.len()")
            .to_digit(10)
        {
            numbers.push(x);
        }
    }

    numbers
}
```

> Note that even though we are slicing for the whole rest of the `&str`, this does not
> give any performance deficit: we are dealing with references and `starts_with` only
> iterates on what it needs. Getting the first char also only goes into the very
> beginning of the slice.

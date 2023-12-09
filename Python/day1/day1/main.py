NUMBERS = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


def parse_line(line: str) -> int:
    digits = [c for c in line if c.isdigit()]
    return 10 * int(digits[0]) + int(digits[-1])


def parse_line2(line: str) -> int:
    for i, number in enumerate(NUMBERS):
        line = line.replace(number, f"{number[0]}{i+1}{number[1:]}")

    return parse_line(line)


def solve_part_one(path: str) -> int:
    with open(path) as file:
        lines = file.read().splitlines()

    return sum(parse_line(line) for line in lines)


def solve_part_two(path: str) -> int:
    with open(path) as file:
        lines = file.read().splitlines()

    return sum(parse_line2(line) for line in lines)


def main():
    print(solve_part_one("input.txt"))
    print(solve_part_two("input.txt"))


if __name__ == "__main__":
    main()

from dataclasses import dataclass
import re


@dataclass
class Set:
    red: int
    green: int
    blue: int


@dataclass
class Game:
    id: int
    sets: list[Set]


def parse_line(line: str) -> Game:
    game, rest = line.split(":", 2)

    id = int(game.split(" ", 2)[1])

    sets: list[Set] = []
    for new_set_str in rest.strip().split("; "):
        matches = re.finditer(
            r"(?P<red>\d+\sred)|(?P<green>\d+\sgreen)|(?P<blue>\d+\sblue)", new_set_str
        )

        if matches is None:
            raise ValueError

        red = 0
        green = 0
        blue = 0
        for match in matches:
            d = match.groupdict()

            updated_red = d.get("red")
            updated_green = d.get("green")
            updated_blue = d.get("blue")

            if updated_red is not None:
                red = int(updated_red.split()[0])
            if updated_green is not None:
                green = int(updated_green.split()[0])
            if updated_blue is not None:
                blue = int(updated_blue.split()[0])

        sets.append(Set(red, green, blue))

    return Game(id, sets)


def solve_part_one(file_name: str) -> int:
    with open(file_name) as file:
        lines = file.read().splitlines()

    games = [parse_line(line) for line in lines]
    valid_games = [
        game
        for game in games
        if all(set.red <= 12 for set in game.sets)
        and all(set.green <= 13 for set in game.sets)
        and all(set.blue <= 14 for set in game.sets)
    ]

    return sum(game.id for game in valid_games)


def solve_part_two(file_name: str) -> int:
    with open(file_name) as file:
        lines = file.read().splitlines()

    games = [parse_line(line) for line in lines]
    return sum(
        max(set.green for set in game.sets)
        * max(set.red for set in game.sets)
        * max(set.blue for set in game.sets)
        for game in games
    )


def main():
    print(solve_part_one("input.txt"))
    print(solve_part_two("input.txt"))


if __name__ == "__main__":
    main()

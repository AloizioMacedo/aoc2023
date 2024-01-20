import { readFileSync } from "fs";

type GameSet = {
    red: number;
    green: number;
    blue: number;
};

type Game = {
    gameSets: GameSet[];
};

function readFile(filename: string): string {
    return readFileSync(filename, "utf8");
}

function parse_line(line: string): Game {
    let [_, rest] = line.split(":");

    rest = rest.trim();

    const gameSets = [];

    for (const set of rest.split(";")) {
        gameSets.push(parse_set(set));
    }

    return { gameSets };
}

function parse_set(set: string): GameSet {
    let red = 0;
    let green = 0;
    let blue = 0;

    for (const entry of set.split(",")) {
        let [n, color] = entry.trim().split(" ");
        n = n.trim();
        color = color.trim();

        if (color == "red") {
            red = Number(n);
        } else if (color == "green") {
            green = Number(n);
        } else if (color == "blue") {
            blue = Number(n);
        }
    }

    return {
        red,
        green,
        blue,
    };
}

function is_game_possible(game: Game): boolean {
    for (let i = 0; i < game.gameSets.length; i++) {
        const set = game.gameSets[i];

        if (set.red > 12 || set.green > 13 || set.blue > 14) {
            return false;
        }
    }

    return true;
}

function solve_part_one(contents: string): number {
    const games = contents.split("\n").map((line) => parse_line(line));

    let total = 0;

    for (let i = 0; i < games.length; i++) {
        if (is_game_possible(games[i])) {
            total += i + 1;
        }
    }

    return total;
}

function get_min_set(game: Game): GameSet {
    let red = 0;
    let green = 0;
    let blue = 0;

    for (const set of game.gameSets) {
        red = Math.max(red, set.red);
        green = Math.max(green, set.green);
        blue = Math.max(blue, set.blue);
    }

    return { red, green, blue };
}

function solve_part_two(contents: string): number {
    const games = contents.split("\n").map((line) => parse_line(line));

    let total = 0;

    for (let i = 0; i < games.length; i++) {
        let min_set = get_min_set(games[i]);

        total += min_set.red * min_set.green * min_set.blue;
    }

    return total;
}

function main() {
    const input = readFile("input.txt");

    console.log(`Part 1: ${solve_part_one(input)}`);
    console.log(`Part 2: ${solve_part_two(input)}`);
}

main();

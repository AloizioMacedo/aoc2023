import { readFileSync } from "fs";

const numbers = [
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
];

function read_to_string(filename: string): string {
    return readFileSync(filename, "utf-8");
}

function get_numbers_from_line(line: string): number {
    let first_number = 0;
    let last_number = 0;
    for (const ch of line) {
        if (
            ch.charCodeAt(0) >= "0".charCodeAt(0) &&
            ch.charCodeAt(0) <= "9".charCodeAt(0)
        ) {
            if (first_number == 0) {
                first_number = ch.charCodeAt(0) - "0".charCodeAt(0);
            }
            last_number = ch.charCodeAt(0) - "0".charCodeAt(0);
        }
    }

    return first_number * 10 + last_number;
}

function get_numbers_from_line_p2(line: string): number {
    let first_number = 0;
    let last_number = 0;

    for (let i = 0; i < line.length; i++) {
        for (let j = 0; j < 9; j++) {
            if (line.slice(i).startsWith(numbers[j])) {
                if (first_number == 0) {
                    first_number = j + 1;
                }
                last_number = j + 1;
            }

            if (
                line[i].charCodeAt(0) - "0".charCodeAt(0) >= 0 &&
                line[i].charCodeAt(0) - "0".charCodeAt(0) <= 9
            ) {
                if (first_number == 0) {
                    first_number = line[i].charCodeAt(0) - "0".charCodeAt(0);
                }
                last_number = line[i].charCodeAt(0) - "0".charCodeAt(0);
            }
        }
    }

    return first_number * 10 + last_number;
}

function solve_part_one(filename: string): number {
    let result = 0;

    let contents = read_to_string(filename);

    for (const line of contents.split("\n")) {
        result += get_numbers_from_line(line);
    }

    return result;
}

function solve_part_two(filename: string): number {
    let result = 0;

    let contents = read_to_string(filename);

    for (const line of contents.split("\n")) {
        result += get_numbers_from_line_p2(line);
    }

    return result;
}

function main() {
    let result = solve_part_one("input.txt");
    console.log(result);

    result = solve_part_two("input.txt");
    console.log(result);
}

main();

package main

import (
	"os"
	"strings"
)

func parse_line(line string) int {
	numbers := []int{}

	for _, char := range line {
		x := int(char)

		number := x - int('0')
		if number >= 0 && number <= 9 {
			numbers = append(numbers, number)
		}
	}

	return 10*numbers[0] + numbers[len(numbers)-1]
}

func solve_part_one(file_name string) int {
	byte_contents, _ := os.ReadFile(file_name)

	contents := string(byte_contents)

	total := 0
	for _, line := range strings.Split(contents, "\n") {
		if line == "" {
			continue
		}

		parsed_line := parse_line(line)
		total += parsed_line
	}

	return total
}

func main() {
	println(solve_part_one("input.txt"))
}

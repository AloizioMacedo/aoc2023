package main

import (
	"os"
	"regexp"
	"strconv"
	"strings"
	"unicode"
)

func solve_part_one(contents string) int {
	lines := strings.Split(contents, "\n")
	lines = lines[0 : len(lines)-1]

	n_lines := len(lines)
	n_cols := len(lines[0])

	reg, _ := regexp.Compile(`\d+`)

	part_nums := make([]int, 0)
	for i, line := range lines {
		ranges := reg.FindAllIndex([]byte(line), -1)

	out:
		for _, rang := range ranges {
			for k := max(rang[0]-1, 0); k <= min(rang[1], n_cols-1); k++ {
				for j := max(i-1, 0); j <= min(i+1, n_lines-1); j++ {
					if !unicode.IsDigit(rune(lines[j][k])) && lines[j][k] != '.' {
						num, _ := strconv.Atoi(string(line[rang[0]:rang[1]]))

						part_nums = append(part_nums, num)
						continue out
					}
				}
			}
		}
	}

	total := 0
	for _, value := range part_nums {
		total += value
	}

	return total
}

func solve_part_two(contents string) int {
	lines := strings.Split(contents, "\n")
	lines = lines[0 : len(lines)-1]

	n_lines := len(lines)
	n_cols := len(lines[0])

	reg, _ := regexp.Compile(`\d+`)

	nums := make(map[[2]int][]int)
	for i, line := range lines {
		ranges := reg.FindAllIndex([]byte(line), -1)

		for _, rang := range ranges {
			for k := max(rang[0]-1, 0); k <= min(rang[1], n_cols-1); k++ {
				for j := max(i-1, 0); j <= min(i+1, n_lines-1); j++ {
					if !unicode.IsDigit(rune(lines[j][k])) && lines[j][k] != '.' {
						num, _ := strconv.Atoi(string(line[rang[0]:rang[1]]))

						x := nums[[2]int{j, k}]
						nums[[2]int{j, k}] = append(x, num)
					}
				}
			}
		}
	}

	total := 0
	for _, values := range nums {
		if len(values) == 2 {
			total += values[0] * values[1]
		}
	}

	return total
}

func main() {
	b, _ := os.ReadFile("input.txt")
	contents := string(b)

	println(solve_part_one(contents))
	println(solve_part_two(contents))
}

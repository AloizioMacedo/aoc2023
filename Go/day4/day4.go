package main

import (
	"math"
	"os"
	"strconv"
	"strings"
)

func contains(values []int, value int) bool {
	for _, v := range values {
		if value == v {
			return true
		}
	}

	return false
}

type Card struct {
	my_numbers      []int
	winning_numbers []int
}

func (c Card) get_count() int {
	total := 0
	for _, number := range c.my_numbers {
		if contains(c.winning_numbers, number) {
			total += 1
		}
	}

	return total
}

func split_once(s string, sep string) (string, string) {
	separation := strings.SplitN(s, sep, 2)

	return separation[0], separation[1]
}

func parse_line(line string) Card {
	_, rest := split_once(line, ":")

	winning, mine := split_once(rest, "|")

	winning_numbers_as_str := strings.Split(winning, " ")
	my_numbers_as_str := strings.Split(mine, " ")

	winning_numbers := make([]int, 0)
	my_numbers := make([]int, 0)

	for _, v := range winning_numbers_as_str {
		v = strings.TrimSpace(v)
		if v == "" {
			continue
		}

		value, _ := strconv.Atoi(v)
		winning_numbers = append(winning_numbers, value)
	}

	for _, v := range my_numbers_as_str {
		v = strings.TrimSpace(v)
		if v == "" {
			continue
		}

		value, _ := strconv.Atoi(v)
		my_numbers = append(my_numbers, value)
	}

	return Card{my_numbers, winning_numbers}
}

func parse_contents(path string) []Card {
	b, _ := os.ReadFile(path)

	contents := string(b)

	cards := make([]Card, 0)
	for _, line := range strings.Split(contents, "\n") {
		if line == "" {
			continue
		}

		card := parse_line(line)
		cards = append(cards, card)
	}

	return cards
}

func solve_part_one(path string) int {
	cards := parse_contents(path)

	total := 0
	for _, card := range cards {
		count := card.get_count()

		if count != 0 {
			total += int(math.Pow(float64(2), float64(count-1)))
		}
	}

	return total
}

func main() {
	println(solve_part_one("input.txt"))
}

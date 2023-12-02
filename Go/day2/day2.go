package main

import (
	"os"
	"strconv"
	"strings"
)

const (
	MAX_RED   = 12
	MAX_GREEN = 13
	MAX_BLUE  = 14
)

type Color string

const (
	Red   Color = "red"
	Green Color = "green"
	Blue  Color = "blue"
)

type Game struct {
	id   string
	sets []Set
}

func (g Game) is_possible() bool {
	for _, set := range g.sets {
		if !set.is_possible() {
			return false
		}
	}

	return true
}

type Set struct {
	red   int
	green int
	blue  int
}

func (s Set) is_possible() bool {
	return s.red <= MAX_RED && s.green <= MAX_GREEN && s.blue <= MAX_BLUE
}

func parse_line(line string) Game {
	sets := make([]Set, 0)

	game_and_sets := strings.SplitN(line, ":", 2)

	game_as_string := game_and_sets[0]
	sets_as_strings := strings.Split(game_and_sets[1], ";")

	id := strings.SplitN(game_as_string, " ", 2)[1]

	for _, set_string := range sets_as_strings {
		red := 0
		green := 0
		blue := 0

		split_set := strings.Split(set_string, ", ")

		for _, color := range split_set {
			color = strings.TrimSpace(color)
			color_split := strings.Split(color, " ")

			number, _ := strconv.Atoi(color_split[0])
			color_name := color_split[1]

			switch Color(color_name) {
			case Red:
				red = number
			case Green:
				green = number
			case Blue:
				blue = number
			}

		}

		set := Set{red, green, blue}

		sets = append(sets, set)
	}

	return Game{id, sets}
}

func solve_part_one(file_name string) int {
	contents_as_bytes, _ := os.ReadFile(file_name)

	contents := string(contents_as_bytes)

	total := 0
	for _, line := range strings.Split(contents, "\n") {
		if line == "" {
			continue
		}

		game := parse_line(line)

		if game.is_possible() {
			id_as_int, _ := strconv.Atoi(game.id)

			total += id_as_int
		}
	}

	return total
}

func main() {
	println(solve_part_one("input.txt"))
}

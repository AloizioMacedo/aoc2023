package main

import (
	"fmt"
	"testing"
)

func TestParse(t *testing.T) {
	fmt.Println(parse_line("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
}

func TestPartOne(t *testing.T) {
	if solve_part_one("test_input.txt") != 8 {
		t.Fail()
	}
}

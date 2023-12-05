package main

import (
	"testing"
)

func TestParse(t *testing.T) {
	s := "1abc2"

	if parse_line(s) != 12 {
		println(parse_line(s))
		t.Fail()
	}

	s = "treb7uchet"
	if parse_line(s) != 77 {
		println(parse_line(s))
		t.Fail()
	}
}

func TestPartOne(t *testing.T) {
	if solve_part_one("test_input_p1.txt") != 142 {
		t.Fail()
	}
}

func TestNewParse(t *testing.T) {
	if parse_line_with_spelled_out_names("oneight") != 18 {
		t.Fail()
	}
}

func TestPartTwo(t *testing.T) {
	if solve_part_two("test_input_p2.txt") != 281 {
		println(solve_part_two("test_input_p2.txt"))
		t.Fail()
	}
}

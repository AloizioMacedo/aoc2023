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
	if solve_part_one("test_input.txt") != 142 {
		t.Fail()
	}
}

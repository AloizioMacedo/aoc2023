package main

import (
	"fmt"
	"os"
	"testing"
)

func TestTwo(t *testing.T) {
	b, _ := os.ReadFile("test_input.txt")

	contents := string(b)

	nums := solve_part_two(contents)

	fmt.Println(nums)
}

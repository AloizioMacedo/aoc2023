#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string_view>

constexpr std::string_view names[] = {"zero", "one", "two",   "three", "four",
                                      "five", "six", "seven", "eight", "nine"};

std::string read_file(std::string name) {
  std::ifstream f{name};
  f.open(name);
  std::stringstream buffer;
  buffer << f.rdbuf();

  return buffer.str();
}

int get_value(std::string_view word) {
  int first = -1;
  int last = -1;

  for (char c : word) {
    if (c - '0' <= 9 && c >= '0') {
      int value = c - '0';
      if (first == -1) {
        first = value;
      }
      last = value;
    }
  }

  if (first == -1) {
    return 0;
  }

  return 10 * first + last;
}

bool starts_with(std::string_view original, std::string_view to_compare) {
  size_t len1 = original.length();
  size_t len2 = to_compare.length();

  if (len2 > len1) {
    return false;
  }

  for (size_t i = 0; i < len2; i++) {
    if (original[i] != to_compare[i]) {
      return false;
    }
  }

  return true;
}

int get_value_part_two(std::string_view word) {
  int first = -1;
  int last = -1;

  for (size_t i = 0; i < word.length(); i++) {
    for (size_t j = 0; j <= 9; j++) {
      if (starts_with(word.substr(i), names[j])) {
        if (first == -1) {
          first = j;
        }

        last = j;
        continue;
      } else {
        char c = word[i];

        if (c - '0' <= 9 && c >= '0') {
          int value = c - '0';
          if (first == -1) {
            first = value;
          }
          last = value;
        }
      }
    }
  }

  if (first == -1) {
    return 0;
  }

  return 10 * first + last;
}

int solve_part_one(std::string_view contents) {
  size_t p1 = 0;
  size_t p2 = contents.find("\n");
  int total = 0;

  while (true) {
    std::string_view word = contents.substr(p1, p2 - p1);

    total += get_value(word);

    size_t p3;
    p3 = contents.substr(p2 + 1).find("\n");
    p1 = p2 + 1;
    p2 = p2 + p3 + 1;

    if (p3 == std::string::npos) {
      total += get_value(contents.substr(p2 + 1));
      break;
    }
  }

  return total;
}

int solve_part_two(std::string_view contents) {
  size_t p1 = 0;
  size_t p2 = contents.find("\n");
  int total = 0;

  while (true) {
    std::string_view word = contents.substr(p1, p2 - p1);

    total += get_value_part_two(word);

    size_t p3;
    p3 = contents.substr(p2 + 1).find("\n");
    p1 = p2 + 1;
    p2 = p2 + p3 + 1;

    if (p3 == std::string::npos) {
      total += get_value(contents.substr(p2 + 1));
      break;
    }
  }

  return total;
}

int main() {
  std::string contents = read_file("input.txt");

  int result = solve_part_one(contents);
  std::printf("P1 result: %d\n", result);

  result = solve_part_two(contents);
  std::printf("P2 result: %d\n", result);
}
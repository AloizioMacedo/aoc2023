#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string_view>
#include <variant>

std::string read_file(std::string name) {
  std::ifstream f;
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

int sum_values(std::string_view contents) {
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

    if (p3 == std::variant_npos) {
      total += get_value(contents.substr(p2 + 1));
      break;
    }
  }

  return total;
}

int main() {
  std::string contents = read_file("input.txt");

  int result = sum_values(contents);
  std::printf("P1 result: %d\n", result);
}
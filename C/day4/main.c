#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int read_file_into_buffer(char *buffer, FILE *fp) {
  size_t len;
  ssize_t bytes_read = getdelim(&buffer, &len, '\0', fp);

  return bytes_read;
}

struct IntList {
  int *values;
  int len;
};

int contains(struct IntList list, int value) {
  for (int i = 0; i < list.len; i++) {
    if (list.values[i] == value)
      return 1;
  }

  return 0;
}

struct Card {
  struct IntList winning_numbers;
  struct IntList my_numbers;
};

int count_winning(struct Card card) {
  int count = 0;

  for (int i = 0; i < card.my_numbers.len; i++) {
    if (contains(card.winning_numbers, card.my_numbers.values[i])) {
      count++;
    }
  }

  return count;
}

int get_points(struct Card card) {

  int count = count_winning(card);

  if (count > 0) {
    int prod = 1;
    while (count > 1) {
      prod *= 2;
      count--;
    }

    return prod;
  } else {
    return 0;
  }
}

struct IntList parse_numbers(char *ns) {
  char *sp;
  char *numbers = strdup(ns);

  char *number = strtok_r(numbers, " ", &sp);

  int *final_numbers = malloc(strlen(ns) * sizeof(int));

  int i = 0;
  while (number != NULL) {
    if (isspace(*number)) {
      number = strtok_r(NULL, " ", &sp);
      continue;
    }

    final_numbers[i] = atoi(number);
    number = strtok_r(NULL, " ", &sp);
    i++;
  }

  struct IntList numbers_list = {final_numbers, i};
  return numbers_list;
}

struct Card parse_line(char *line) {
  struct Card card;

  char *sp;

  char *line_copy = strdup(line);
  char *_ = strtok_r(line_copy, ":", &sp);

  char *numbers = strtok_r(NULL, "|", &sp);

  int is_winning = 1;
  while (numbers != NULL) {
    struct IntList ns = parse_numbers(numbers);
    if (is_winning) {
      card.winning_numbers = ns;
      is_winning = 0;
    } else {
      card.my_numbers = ns;
    }

    numbers = strtok_r(NULL, "|", &sp);
  }

  return card;
}

int solve_part_one(char *contents) {
  int total = 0;

  char *sp;
  char *line = strtok_r(contents, "\n", &sp);

  while (line != NULL) {
    struct Card card = parse_line(line);

    total += get_points(card);
    line = strtok_r(NULL, "\n", &sp);
  }

  return total;
}

void test() {
  struct Card card1 =
      parse_line("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53");

  printf("%d\n", count_winning(card1));
  printf("%d\n", get_points(card1));

  struct Card card2 =
      parse_line("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19");
  printf("%d\n", count_winning(card2));
}

int main() {
  char *buffer = malloc(26000);
  FILE *fp = fopen("input.txt", "r");

  read_file_into_buffer(buffer, fp);

  printf("Part 1: %d\n", solve_part_one(buffer));
}

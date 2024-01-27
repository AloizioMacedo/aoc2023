#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int read_file_into_buffer(char *buffer, FILE *fp) {
  size_t len;
  ssize_t bytes_read = getdelim(&buffer, &len, '\0', fp);

  return bytes_read;
}

typedef struct {
  int *values;
  int len;
} IntList;

int contains(IntList list, int value) {
  for (int i = 0; i < list.len; i++) {
    if (list.values[i] == value)
      return 1;
  }

  return 0;
}

typedef struct {
  IntList winning_numbers;
  IntList my_numbers;
} Card;

int count_winning(Card card) {
  int count = 0;

  for (int i = 0; i < card.my_numbers.len; i++) {
    if (contains(card.winning_numbers, card.my_numbers.values[i])) {
      count++;
    }
  }

  return count;
}

int get_points(Card card) {

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

IntList parse_numbers(char *ns) {
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

  IntList numbers_list = {final_numbers, i};
  return numbers_list;
}

Card parse_line(char *line) {
  Card card;

  char *sp;

  char *line_copy = strdup(line);
  char *_ = strtok_r(line_copy, ":", &sp);

  char *numbers = strtok_r(NULL, "|", &sp);

  int is_winning = 1;
  while (numbers != NULL) {
    IntList ns = parse_numbers(numbers);
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
    Card card = parse_line(line);

    total += get_points(card);
    line = strtok_r(NULL, "\n", &sp);

    free(card.my_numbers.values);
    free(card.winning_numbers.values);
  }

  return total;
}

int solve_part_two(char *contents) {
  int *card_counts = malloc(300 * sizeof(int));

  char *sp;
  char *line = strtok_r(contents, "\n", &sp);

  int counter = 0;
  while (line != NULL) {
    card_counts[counter] = 1 + card_counts[counter];

    Card card = parse_line(line);

    int count = count_winning(card);

    for (int i = 1; i <= count && i < 300; i++) {
      card_counts[counter + i] += card_counts[counter];
    }

    line = strtok_r(NULL, "\n", &sp);
    counter++;

    free(card.my_numbers.values);
    free(card.winning_numbers.values);
  }

  int total = 0;

  for (int i = 0; i < 300; i++) {
    total += card_counts[i];
  };

  return total;
}

int main() {
  char *buffer = malloc(26000);
  FILE *fp = fopen("input.txt", "r");

  read_file_into_buffer(buffer, fp);

  char *string1 = strdup(buffer);
  printf("Part 1: %d\n", solve_part_one(buffer));
  printf("Part 2: %d\n", solve_part_two(string1));

  free(buffer);
  free(string1);
}

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
} HistoricalValues;

HistoricalValues diff(HistoricalValues values) {
  int *diffs = malloc((values.len - 1) * 4);

  for (int i = 0; i < values.len - 1; i++) {
    diffs[i] = values.values[i + 1] - values.values[i];
  }

  return (HistoricalValues){.values = diffs, .len = values.len - 1};
}

int forecast(HistoricalValues values) {
  int all_are_zero = 1;
  for (int i = 0; i < values.len; i++) {
    if (values.values[i] != 0) {
      all_are_zero = 0;
      break;
    }
  }

  HistoricalValues diffs = diff(values);

  if (all_are_zero) {
    return 0;
  }

  int forecasted_for_diffs = forecast(diffs);

  return values.values[values.len - 1] + forecasted_for_diffs;
}

int parse_line(char *str, int *values) {
  char *sp;
  char *value = strtok_r(str, " ", &sp);

  int i = 0;
  while (value != NULL) {
    int number = atoi(value);

    values[i] = number;

    i++;
    value = strtok_r(NULL, " ", &sp);
  }

  return i;
}

int solve_part_one(char *contents) {
  char *sp;

  char *line = strtok_r(contents, "\n", &sp);

  long sum_extrapolated = 0;

  while (line != NULL) {
    int *values = malloc(strlen(line) * 4);

    int len = parse_line(line, values);

    int forecasted_value = forecast((HistoricalValues){
        .values = values,
        .len = len,
    });

    sum_extrapolated += forecasted_value;

    line = strtok_r(NULL, "\n", &sp);
  }

  return sum_extrapolated;
}

int *reverse(int *values, int len) {
  int *new_values = malloc(len * 4);

  for (int i = 0; i < len; i++) {
    new_values[i] = values[len - i - 1];
  }

  return new_values;
}

int solve_part_two(char *contents) {
  char *sp;

  char *line = strtok_r(contents, "\n", &sp);

  long sum_extrapolated = 0;

  while (line != NULL) {
    int *values = malloc(strlen(line) * 4);

    int len = parse_line(line, values);
    int *reversed_values = reverse(values, len);

    int forecasted_value = forecast((HistoricalValues){
        .values = reversed_values,
        .len = len,
    });

    sum_extrapolated += forecasted_value;

    line = strtok_r(NULL, "\n", &sp);
  }

  return sum_extrapolated;
}

int main() {
  FILE *fp_test = fopen("test_input.txt", "r");
  FILE *fp = fopen("input.txt", "r");

  char *contents_test = malloc(48);
  char *contents = malloc(21726);
  read_file_into_buffer(contents, fp);
  read_file_into_buffer(contents_test, fp_test);

  char *copied_contents_test = strdup(contents_test);
  char *copied_contents = strdup(contents);

  printf("Part 1 (Test): %d\n", solve_part_one(contents_test));
  printf("Part 1: %d\n", solve_part_one(contents));

  printf("Part 2 (Test): %d\n", solve_part_two(copied_contents_test));
  printf("Part 2: %d\n", solve_part_two(copied_contents));
}

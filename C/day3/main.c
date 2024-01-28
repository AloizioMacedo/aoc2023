#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int read_file_into_buffer(char *buffer, FILE *fp) {
  size_t len;
  ssize_t bytes_read = getdelim(&buffer, &len, '\0', fp);

  return bytes_read;
}

typedef struct {
  char *values;
  int nrows;
  int ncols;
} Matrix;

typedef struct {
  int line;
  int beg_col;
  int end_col;
  int number;
} NumberEntry;

typedef struct {
  Matrix m;
  NumberEntry *entries;
  int number_of_entries;
} Engine;

typedef struct {
  int line;
  int col;

} Point;

bool is_digit_or_dot(char c) { return (c >= '0' && c <= '9') || c == '.'; }

bool is_digit(char c) { return c >= '0' && c <= '9'; }

int get_numbers(char *line, int line_no, NumberEntry *np) {
  int int_counter = 0;

  int col = 0;
  int current_int = 0;
  int current_int_len = 0;
  bool currently_parsing_an_int = false;

  for (int i = 0; i <= strlen(line); i++) {
    if (is_digit(line[i])) {
      current_int = current_int * 10 + (line[i] - '0');
      current_int_len++;
      currently_parsing_an_int = true;
    } else {
      if (currently_parsing_an_int) {
        NumberEntry ne = {.line = line_no,
                          .beg_col = i - current_int_len,
                          .end_col = i - 1,
                          .number = current_int};
        np[int_counter] = ne;

        int_counter++;
        current_int = 0;
        current_int_len = 0;
      }

      currently_parsing_an_int = false;
    }
  }

  return int_counter;
}

char entry(Matrix m, int i, int j) { return m.values[i * m.ncols + j]; }

bool is_part(Matrix m, NumberEntry ne) {
  Point positions1[] = {{.col = ne.beg_col - 1, .line = ne.line - 1},
                        {.col = ne.beg_col - 1, .line = ne.line},
                        {.col = ne.beg_col - 1, .line = ne.line + 1}};

  for (int i = 0; i < 3; i++) {
    if (positions1[i].col < 0 || positions1[i].col >= m.ncols ||
        positions1[i].line < 0 || positions1[i].line >= m.nrows) {
      continue;
    }

    if (!is_digit_or_dot(entry(m, positions1[i].line, positions1[i].col))) {
      return 1;
    }
  }

  for (int i = ne.beg_col; i <= ne.end_col; i++) {

    if ((ne.line - 1 >= 0 && ne.line - 1 < m.nrows) &&
        (!is_digit_or_dot(entry(m, ne.line - 1, i)))) {
      return 1;
    }

    if ((ne.line + 1 >= 0 && ne.line + 1 < m.nrows) &&
        (!is_digit_or_dot(entry(m, ne.line + 1, i)))) {
      return 1;
    }
  }

  Point positions2[] = {{.col = ne.end_col + 1, .line = ne.line - 1},
                        {.col = ne.end_col + 1, .line = ne.line},
                        {.col = ne.end_col + 1, .line = ne.line + 1}};

  for (int i = 0; i < 3; i++) {
    if (positions2[i].col < 0 || positions2[i].col >= m.ncols ||
        positions2[i].line < 0 || positions2[i].line >= m.nrows) {
      continue;
    }

    if (!is_digit_or_dot(entry(m, positions2[i].line, positions2[i].col))) {
      return 1;
    }
  }

  return 0;
}

Engine create_engine(char *contents) {
  char *values = malloc(strlen(contents));
  NumberEntry *number_entries = malloc(sizeof(NumberEntry) * strlen(contents));

  char *sp;
  char *line = strtok_r(contents, "\n", &sp);

  int ncols;
  int line_no = 0;
  int total_number_of_entries = 0;

  while (line != NULL) {
    ncols = strlen(line);

    for (int i = 0; i < strlen(line); i++) {
      values[line_no * ncols + i] = line[i];
    }

    NumberEntry *entries = malloc(sizeof(NumberEntry) * strlen(line));

    int n = get_numbers(line, line_no, entries);

    for (int i = 0; i < n; i++) {
      number_entries[total_number_of_entries] = entries[i];
      total_number_of_entries++;
    }

    line_no++;
    line = strtok_r(NULL, "\n", &sp);

    free(entries);
  }

  Matrix m = {.ncols = ncols, .nrows = line_no, .values = values};
  Engine e = {.m = m,
              .entries = number_entries,
              .number_of_entries = total_number_of_entries};

  return e;
}

int solve_part_one(char *contents) {
  Engine e = create_engine(contents);

  int total = 0;
  for (int i = 0; i < e.number_of_entries; i++) {
    if (is_part(e.m, e.entries[i])) {
      total += e.entries[i].number;
    }
  }

  free(e.entries);
  free(e.m.values);

  return total;
}

int main() {
  char *buffer = malloc(19741);
  char *buffer_test = malloc(111);

  FILE *fp = fopen("input.txt", "r");
  FILE *fp_test = fopen("test_input.txt", "r");

  read_file_into_buffer(buffer, fp);
  read_file_into_buffer(buffer_test, fp_test);

  printf("Part 1 (Test): %d\n", solve_part_one(buffer_test));
  printf("Part 1: %d\n", solve_part_one(buffer));
}

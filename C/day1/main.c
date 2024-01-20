#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *numbers[] = {"one", "two",   "three", "four", "five",
                         "six", "seven", "eight", "nine"};

int read_file_into_buffer(char *buffer, FILE *fp) {
  size_t len;
  ssize_t bytes_read = getdelim(&buffer, &len, '\0', fp);

  return bytes_read;
}

int get_number(char *line) {
  int first_number = 0;
  int last_number = 0;

  for (int i = 0; i < strlen(line); i++) {
    if (line[i] >= '0' && line[i] <= '9') {
      if (first_number == 0) {
        first_number = line[i] - '0';
      }
      last_number = line[i] - '0';
    }
  }

  return 10 * first_number + last_number;
}

int starts_with(char *str, const char *prefix) {
  return strncmp(str, prefix, strlen(prefix)) == 0;
}

int get_number_p2(char *line) {
  int first_number = 0;
  int last_number = 0;

  for (int i = 0; i < strlen(line); i++) {
    for (int j = 0; j < 9; j++) {
      if (starts_with(&line[i], numbers[j])) {
        if (first_number == 0) {
          first_number = j + 1;
        }

        last_number = j + 1;
        break;
      }
    }

    if (line[i] >= '0' && line[i] <= '9') {
      if (first_number == 0) {
        first_number = line[i] - '0';
      }
      last_number = line[i] - '0';
    }
  }

  return 10 * first_number + last_number;
}

int solve_part_one(char *input) {
  int total = 0;
  char *line = strtok(input, "\n");

  while (line != NULL) {
    total += get_number(line);

    line = strtok(NULL, "\n");
  }

  return total;
}

int solve_part_two(char *input) {
  int total = 0;
  char *line = strtok(input, "\n");

  while (line != NULL) {
    total += get_number_p2(line);

    line = strtok(NULL, "\n");
  }

  return total;
}

int main() {
  char *buffer = malloc(23000);
  FILE *fp = fopen("test.txt", "r");

  int bytes = read_file_into_buffer(buffer, fp);

  char *copied_string = malloc(23000);

  strcpy(copied_string, buffer);
  printf("Part 1: %d\n", solve_part_one(copied_string));
  free(copied_string);

  printf("Part 2: %d\n", solve_part_two(buffer));
  free(buffer);
}

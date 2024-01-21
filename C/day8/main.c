#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Node {
  char id[3];
  char left[3];
  char right[3];
};

void get_next(struct Node node, char dir, char *buffer) {
  if (dir == 'L') {
    memcpy(buffer, node.left, 3);
  } else {
    memcpy(buffer, node.right, 3);
  }
}

int read_file_into_buffer(char *buffer, FILE *fp) {
  size_t len;
  ssize_t bytes_read = getdelim(&buffer, &len, '\0', fp);

  return bytes_read;
}

char get_dir(char *directions, int index) {
  return directions[index % strlen(directions)];
}

struct Node read_line(char *line) {
  char id[] = {line[0], line[1], line[2]};
  char left[] = {line[7], line[8], line[9]};
  char right[] = {line[12], line[13], line[14]};

  struct Node node;

  memcpy(node.id, id, 3);
  memcpy(node.left, left, 3);
  memcpy(node.right, right, 3);

  return node;
}

int ideq(char *a, char *b) {
  return a[0] == b[0] && a[1] == b[1] && a[2] == b[2];
}

struct Node *find(char *id, struct Node *nodes, int length) {
  for (int i = 0; i < length; i++) {
    if (ideq(id, nodes[i].id)) {
      return &nodes[i];
    }
  }

  return NULL;
}

struct Problem {
  struct Node *nodes;
  int length;
  char *directions;
};

struct Problem build_problem(char *contents) {
  char *contents_copy = strdup(contents);

  char *sp;
  char *next_line = strtok_r(contents_copy, "\n", &sp);

  struct Node *nodes = malloc(1000 * sizeof(struct Node));

  char *directions;
  int counter = 0;
  while (next_line != NULL) {
    if (counter == 0) {
      directions = next_line;
    } else {
      struct Node node = read_line(next_line);
      nodes[counter - 1] = node;
    }

    counter++;
    next_line = strtok_r(NULL, "\n", &sp);
  }

  struct Problem problem = {nodes, counter - 1, directions};

  return problem;
}

int solve_part_one(char *contents) {
  struct Problem problem = build_problem(contents);

  struct Node node = *find("AAA", problem.nodes, problem.length);

  char buffer[3];

  int counter = 0;
  while (ideq(node.id, "ZZZ") != 1) {
    get_next(node, get_dir(problem.directions, counter), buffer);

    node = *find(buffer, problem.nodes, problem.length);

    counter++;
  }

  return counter;
}

void test() {
  FILE *fp = fopen("test_input.txt", "r");
  char *contents = malloc(1000);
  read_file_into_buffer(contents, fp);

  struct Problem problem = build_problem(contents);
}

int main() {
  FILE *fp = fopen("input.txt", "r");
  char *contents = malloc(13306);
  read_file_into_buffer(contents, fp);

  printf("Part 1: %d\n", solve_part_one(contents));
}

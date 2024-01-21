#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Node {
  char id[3];
  char left[3];
  char right[3];
};

long long gcd(long long a, long long b) {
  if (b == 0) {
    return a;
  }

  return gcd(b, a % b);
}

long long lcm(long long a, long long b) { return (a * b) / gcd(a, b); }

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

struct FilteredNodes {
  struct Node **nodes;
  int length;
};

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

struct FilteredNodes find_all_ending_in_char(char c, struct Node *nodes,
                                             int length) {
  struct Node **results = malloc(length * sizeof(struct Node *));

  int counter = 0;
  for (int i = 0; i < length; i++) {
    if (nodes[i].id[2] == c) {
      results[counter] = &nodes[i];
      counter++;
    }
  }

  return (struct FilteredNodes){results, counter};
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

long long lcm_list(long long *list, long long length) {
  long long result = list[0];
  for (int i = 1; i < length; i++) {
    result = lcm(result, list[i]);
  }

  return result;
}

long long solve_part_two(char *contents) {
  struct Problem problem = build_problem(contents);

  struct FilteredNodes nodes_ending_at_a =
      find_all_ending_in_char('A', problem.nodes, problem.length);

  long long *counters_for_each_node_ending_at_a =
      malloc(nodes_ending_at_a.length * sizeof(long));

  char buffer[3];

  for (int i = 0; i < nodes_ending_at_a.length; i++) {
    struct Node *node_ending_at_a = nodes_ending_at_a.nodes[i];

    int counter = 0;
    struct Node node = *node_ending_at_a;
    while (node.id[2] != 'Z') {
      get_next(node, get_dir(problem.directions, counter), buffer);

      node = *find(buffer, problem.nodes, problem.length);

      counter++;
    }

    counters_for_each_node_ending_at_a[i] = counter;
  }

  return lcm_list(counters_for_each_node_ending_at_a, nodes_ending_at_a.length);
}

int main() {
  FILE *fp = fopen("input.txt", "r");
  char *contents = malloc(13306);
  read_file_into_buffer(contents, fp);

  char *contents_copy = strdup(contents);

  printf("Part 1: %d\n", solve_part_one(contents));
  printf("Part 2: %llu\n", solve_part_two(contents_copy));
}

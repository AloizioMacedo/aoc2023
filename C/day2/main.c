#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void slice(char *str, char *result, int start, int end) {
  strncpy(result, str + start, end - start);
}

int read_file_into_buffer(char *buffer, FILE *fp) {
  size_t len;
  ssize_t bytes_read = getdelim(&buffer, &len, '\0', fp);

  return bytes_read;
}

int max(int x, int y) { return x > y ? x : y; }

typedef struct {
  int red;
  int green;
  int blue;
} GameSet;

void print_game_set(GameSet set) {
  printf("GameSet: %d %d %d\n", set.red, set.green, set.blue);
}

typedef struct {
  GameSet *set;
  int length;
} Game;

char *get_colorp(char *set) {
  char *number;
  for (int i = 0; i < strlen(set); i++) {
    if (set[i] == ' ') {
      number = &set[i];
    }
  }

  return number;
};

GameSet build_set(char *set) {
  GameSet game_set = {0, 0, 0};

  char *new_set = malloc(strlen(set) + 1);
  strcpy(new_set, set);

  char *savep;
  char *next = strtok_r(new_set, ",", &savep);
  next = next - 1;

  while (next != NULL) {
    next = &next[1];

    char *colorp = get_colorp(next) + 1;

    char number[5] = "";
    slice(next, number, 0, get_colorp(next) - next);

    int n = atoi(number);

    if (colorp[0] == 'r') {
      game_set.red = n;
    } else if (colorp[0] == 'g') {
      game_set.green = n;
    } else if (colorp[0] == 'b') {
      game_set.blue = n;
    }

    next = strtok_r(NULL, ",", &savep);
  }

  free(new_set);

  return game_set;
}

Game create_game(char *line) {
  GameSet *sets = malloc(4000);

  Game game = {sets, 0};

  char *line_copy = malloc(strlen(line) + 1);
  strcpy(line_copy, line);

  char *savep;
  char *set = strtok_r(line_copy, ":;", &savep);

  int i = -1;
  while (set != NULL) {
    if (i == -1) {
      set = strtok_r(NULL, ":;", &savep);
      i++;
      continue;
    }

    sets[i] = build_set(&set[1]);
    i++;

    set = strtok_r(NULL, ":;", &savep);
  }

  game.length = i;
  free(line_copy);

  return game;
}

int is_possible(Game game) {
  GameSet *set = game.set;
  for (int i = 0; i < game.length; i++) {
    if (set[i].red > 12 || set[i].green > 13 || set[i].blue > 14) {
      return 0;
    }
  }

  return 1;
}

int solve_part_one(char *contents) {
  char *savep;

  char *line = strtok_r(contents, "\n", &savep);
  int total = 0;
  int counter = 1;

  while (line != NULL) {
    Game game = create_game(line);
    if (is_possible(game)) {
      total += counter;
    }

    counter++;
    line = strtok_r(NULL, "\n", &savep);
    free(game.set);
  }

  return total;
}

GameSet get_minimum_set(Game game) {
  GameSet *set = game.set;

  int red = 0;
  int green = 0;
  int blue = 0;

  for (int i = 0; i < game.length; i++) {
    red = max(set[i].red, red);
    green = max(set[i].green, green);
    blue = max(set[i].blue, blue);
  }

  return (GameSet){red, green, blue};
}

int get_power(GameSet set) { return set.red * set.green * set.blue; }

int solve_part_two(char *contents) {
  char *savep;

  char *line = strtok_r(contents, "\n", &savep);
  int total = 0;
  int counter = 1;

  while (line != NULL) {
    Game game = create_game(line);
    GameSet min_set = get_minimum_set(game);

    total += get_power(min_set);

    counter++;
    line = strtok_r(NULL, "\n", &savep);
  }

  return total;
}

int main() {
  FILE *fp = fopen("input.txt", "r");
  char *buffer = malloc(11000);
  read_file_into_buffer(buffer, fp);

  char *string1 = malloc(11000);
  strcpy(string1, buffer);

  printf("Part 1: %d\n", solve_part_one(string1));
  printf("Part 2: %d\n", solve_part_two(buffer));

  free(buffer);
  free(string1);
}

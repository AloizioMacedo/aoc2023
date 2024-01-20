#include <stdio.h>

long get_distance_on_wait_time(long wait, long total) {
  return (total - wait) * wait;
}

int get_total_wait_times_possible(int total, long record) {
  int count = 0;
  for (long wait = 1; wait < total; wait++) {
    if (get_distance_on_wait_time(wait, total) > record) {
      count++;
    }
  }

  return count;
}

int main() {
  int total_times[4] = {40, 81, 77, 72};
  int distances[4] = {219, 1012, 1365, 1089};

  int total = 1;
  for (int i = 0; i < 4; i++) {
    total *= get_total_wait_times_possible(total_times[i], distances[i]);
  }

  printf("Part 1: %d\n", total);

  total = get_total_wait_times_possible(40817772, 219101213651089);
  printf("Part 2: %d\n", total);
}

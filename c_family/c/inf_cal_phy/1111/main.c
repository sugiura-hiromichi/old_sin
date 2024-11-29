#include <stdio.h>
int main() {
  int range = 0, p = 0;
  int dbg = 0; // Flag for unit test
  if (dbg == 0) {
    printf("input number(range)> ");
    scanf("%d", &range);
    printf("input number(power)> ");
    scanf("%d", &p);
    printf(
        "this program lists up all exponents of %d in a range of 0..%d (0 is "
        "included. %d is not included)\n",
        p, range, range);
  } else {
    p = 5;
    range = 10000;
  }

  if (range * p < 0) {
    printf("range*p is minus. This will cause infinite loop");
    printf("Stopping program...");
    return 0;
  }

  if (range <= p) {
    return 0;
  }

  if (p < 0) {
    for (int i = -1, j = 1; i > range; i *= p, j++) {
      printf("%d to the %dth power: %d\n", p, j, i);
    }
  } else if (p > 0) {
    for (int i = 1, j = 1; i < range; i *= p, j++) {
      printf("%d to the %dth power: %d\n", p, j, i);
    }
  } else {
    printf("%d to the %dth power: %d\n", p, 0, 0);
  }
}

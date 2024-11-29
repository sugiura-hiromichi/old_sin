#include <stdio.h>

int main() {
  int fn0 = 0, fn1 = 1, fn2;
  int N, sum = 0;
  int handle_input = 1;

  // If you want to avoid annoying prompt, set handle_input=0
  if (handle_input == 0) {
    N = 5;
  } else {
    printf("\t|Enter a size of progression : ");
    scanf("%d", &N);
  }

  for (int i = 0; i < N; i++) {
    printf("% d : % d\n", i + 1, fn1);
    sum = fn1 + sum;
    fn2 = fn0 + fn1;
    fn0 = fn1;
    fn1 = fn2;
  }

  printf("\t|sum of fn1~fn%d is %d", N, sum);
}

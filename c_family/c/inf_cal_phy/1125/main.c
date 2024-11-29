#include <math.h>
#include <stdio.h>

#define N 101
#define X 0.25
#define A_INT 0.5
#define B_INT 15.0

double weierstrass(double a, double b, int n, double x) {
  double sum = 0.0, w_a[n], w_b[n];
  for (int i = 0; i < n; i++) {
    if (i == 0) {
      w_a[i] = 1.0;
      w_b[i] = M_PI;
    } else {
      w_a[i] = A_INT * w_a[i - 1];
      w_b[i] = B_INT * w_b[i - 1];
    }

    sum += w_a[i] * cos(w_b[i] * x);
  }

  return sum;
}

int main() {
  double sum = weierstrass(A_INT, B_INT, N, X);
  printf("[ a = %f, b = %d, N = %d]\nW(%f)=%f", A_INT, (int)B_INT, N - 1, X,
         sum);
}

#include <math.h>
#include <stdio.h>

double r(double x, double y) { return sqrt(x * x + y * y); }
double p(double u, double v) { return sqrt(u * u + v * v); }

double energy(double r, double p) { return p * p / 2.0 - 1.0 / r; }

int main(void) {
  double e = 0.5, dt = .001, t = .0;
  double x = 1.0 - e, y = .0, u = .0, v = sqrt((1 + e) / (1 - e));
  int N = 2.0 * M_PI / dt;

  for (int i = 0; i <= N; i++) {
    printf("%e %e %e %e %e %e \n", t, x, y, u, v, energy(r(x, y), p(u, v)));
    t += dt;
    x += dt * u;
    y += dt * v;
    u -= dt * x / pow(r(x, y), 3.0);
    v -= dt * y / pow(r(x, y), 3.0);
  }
}

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define EPSILON .000000001
#define DELTA .00000001
#define DELTA_T .05
#define ECCENTRICITY .5

// report6
double f(double theta, double t) {
  return theta - ECCENTRICITY * sin(theta) - t;
}

// report6
double f_dash(double theta, double t) {
  double right = f(theta + EPSILON, t);
  double left = f(theta, t);
  return (right - left) / EPSILON;
}

// question1
double newton_kepler(double theta, double t) {
  double prev;
  do {
    prev = theta;
    theta = -f(theta, t) / f_dash(theta, t) + theta;
  } while (fabs(theta - prev) > EPSILON);

  return theta;
}

int main() {
  // question1
  double t = .25, cur = .5;
  double sol = newton_kepler(cur, t);
  assert(sol - 0.4815980 < DELTA);

  // question2
  // NOTE: T=2π
  cur = .0, t = .0;
  double cycle_len = 2.0 * M_PI;
  for (int i = 0; t < cycle_len; i++) {
    printf("%d %e %e %e %e\n", i, t, cur, cos(cur) - ECCENTRICITY,
           sqrt(1 - ECCENTRICITY * ECCENTRICITY) * sin(cur));

    t += DELTA_T;
    cur = newton_kepler(cur, t);
  }
}

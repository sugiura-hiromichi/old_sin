#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

double integrated(double t) {
	return (2 * (1 + t * t)) / (pow(1 - t * t, 2.0) + t * t);
}

int main() {
	srand((unsigned)time(NULL));

	// Question 1
	int n = 10;
	double rslt = .0;
	for (int i = 0; i < 4; i++) {
		n *= 10;
		for (int j = 0; j < n; j++) {
			rslt += integrated((double)rand() / RAND_MAX) / (double)n;
		}

		printf("%d %f %f\n", n, rslt, rslt - M_PI);
		rslt = .0;
	}
}

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define COUNT 100
#define STEP 1e6
#define N_TOTAL 1e5

int main() {
	srand(time(NULL));
	int n1 = N_TOTAL, n2 = 0, n3 = 0;
	for (int i = 0; i < STEP; i++) {
		double r = (double)rand() / (double)RAND_MAX;
		double p1 = (double)n1 / N_TOTAL;
		double p2 = (double)n2 / N_TOTAL;
		double p3 = (double)n3 / N_TOTAL;

		if (r <= p1) {
			n1--;
			n2++;
		}
		if (r <= p2) {
			if (n2 < 2) {
				continue;
			} else {
				n1++;
				n2 -= 2;
				n3++;
			}
		}
		if (r <= p3) {
			if (n3 < 1) {
				continue;
			} else {
				n2++;
				n3--;
			}
		}

		if (i % COUNT == 0) {
			printf("%d %f %f %f\n", i / 100, (double)n1 / N_TOTAL,
					 (double)n2 / N_TOTAL, (double)n3 / N_TOTAL);
		}
	}
}

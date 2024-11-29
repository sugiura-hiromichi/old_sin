#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define COUNT 1e5
#define OUT 100

void simulator(int n_total) {
	int n1 = n_total;
	for (int i = 0; i < COUNT; i++) {
		double r = (double)rand() / (double)RAND_MAX;
		if (r <= (double)n1 / n_total) {
			n1--;
		} else {
			n1++;
		}
		if (i % OUT == 0) {
			printf("%f\n", (double)n1 / n_total);
		}
	}
}

int main() {
	// 8-2
	srand(time(NULL));
	simulator(1e6);
}

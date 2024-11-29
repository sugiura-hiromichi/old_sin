#include "impl.h"
#include <stdlib.h>

int main(int argc, char *argv[]) {
	if (argc > 2) {
		int m = 0, n = 0;
		m = atoi(argv[1]);
		n = atoi(argv[2]);
		float *A = malloc(sizeof(float) * m * n);
		float *b = malloc(sizeof(float) * m);

		load("test.dat", m, n, A, b);
		print(m * n, A);
		print(n, b);

		free(A);
		free(b);
	}

	return 0;
}

#include "impl.h"
#include <stdlib.h>

int main(int argc, char *argv[]) {
	if (argc > 2) {
		int m = 0, n = 0;
		m = atoi(argv[1]);
		n = atoi(argv[2]);
		float *A = malloc(sizeof(float) * m * n);
		float *b = malloc(sizeof(float) * m);

		init(n * m, 1.0, A);
		init(m, 2.0, b);
		print(m * n, A);
		print(n, b);
		save("test.dat", m, n, A, b);

		free(A);
		free(b);
	}

	return 0;
}

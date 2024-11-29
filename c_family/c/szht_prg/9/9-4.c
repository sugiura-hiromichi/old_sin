#include "impl.h"

int main(int argc, char *argv[]) {
	int n = 0;
	if (argc > 1) {
		n = atoi(argv[1]);

		float *y = malloc(sizeof(float) * n);
		rand_init(n, y);
		print(n, y);
		softmax(n, y);
		print(n, y);
		free(y);
	}

	return 0;
}

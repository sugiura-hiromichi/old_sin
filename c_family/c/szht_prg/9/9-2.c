#include "impl.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
	int n = 0;
	if (argc > 1) {
		n = atoi(argv[1]);
	} else {
		return 0;
	}
	float *y = malloc(sizeof(float) * n);
	rand_init(n, y);
	sort(n, y);
	print(n, y);

	int *index = malloc(sizeof(int) * n);
	int i = 0;
	for (; i < n; i++) {
		index[i] = i;
	}
	shuffle(n, index);
	for (i = 0; i < n; i++) {
		printf("%f ", y[index[i]]);
	}

	free(y);
	free(index);
	return 0;
}

#ifndef IMPL_H
#define IMPL_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/// implementation of functions in 9-1----------------------------------
void rand_init(int n, float *y) {
	int i = 0;
	srand((unsigned)time(NULL));
	for (i = 0; i < n; i++) {
		y[i] = (float)rand() / ((float)RAND_MAX / 2.0) - 1.0;
	}
}
void print(int n, float *y) {
	int i = 0;
	for (i = 0; i < n; i++) {
		printf("%f ", y[i]);
	}
	printf("\n");
}
void sort(int n, float *y) {
	int i = 0;
	for (i = 0; i < n - 1; i++) {
		int j = i + 1;
		for (; j < n; j++) {
			if (y[i] > y[j]) {
				float tmp = y[i];
				y[i] = y[j];
				y[j] = tmp;
			}
		}
	}
}

/// implementation of functions in 9-2----------------------------------
void shuffle(int n, int *x) {
	int i = 0;
	srand((unsigned)time(NULL));

	for (i = 0; i < 1; i++) {
		int s = rand() % n;
		int tmp[n];

		int j = 0;
		while (j < s) {
			tmp[j] = x[j + n - s];
			j++;
		}
		while (j < n) {
			tmp[j] = x[j - s];
			j++;
		}

		for (j = 0; j < n; j++) {
			x[j] = tmp[j];
		}
	}
}

/// implementation of functions in 9-3----------------------------------
void relu(int n, float *y) {
	int i;
	for (i = 0; i < n; i++) {
		if (y[i] < 0) {
			y[i] = 0;
		}
	}
}

/// implementation of functions in 9-4----------------------------------
void softmax(int n, float *y) {
	int i;
	float sum = 0.0;
	for (i = 0; i < n; i++) {
		sum += exp(y[i]);
	}
	for (i = 0; i < n; i++) {
		y[i] = exp(y[i]) / sum;
	}
}

/// implementation of functions in 9-5----------------------------------
void init(int n, float x, float *o) {
	int i;
	for (i = 0; i < n; i++) {
		o[i] = x;
	}
}

void save(char *f_name, int m, int n, float *A, float *b) {
	FILE *f = fopen(f_name, "wb");
	fwrite(A, sizeof(float), n * m, f);
	fwrite(b, sizeof(float), m, f);
}

/// implementation of functions in 9-6----------------------------------
void load(char *f_name, int m, int n, float *A, float *b) {
	FILE *f = fopen(f_name, "r");
	float tmp[m * (n + 1)];
	fread(tmp, sizeof(float), m * (n + 1), f);
	int i;
	for (i = 0; i < m * n; i++) {
		A[i] = tmp[i];
	}
	for (i = 0; i < m; i++) {
		b[i] = tmp[i + m * n];
	}
}

#endif

#include "data-samplecode-v14/nn.h"
#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct Matrix {
	float for_fc1[50 * 784];
	float for_fc2[100 * 50];
	float for_fc3[10 * 100];
};

struct Vector {
	float for_fc1[50];
	float for_fc2[100];
	float for_fc3[10];
};

struct MatrixAndVector {
	struct Matrix A;
	struct Vector b;
};

struct Matrix init_matrix() {}
struct Vector init_vector() {}
struct MatrixAndVector init_matrix_and_vector() {
	struct MatrixAndVector mav;
	mav.A = init_matrix();
	mav.b = init_vector();
	return mav;
}

int get_len(float *a) { return sizeof(*a) / sizeof(a[0]); }

float get_max(float *a) {
	float max = 0.0;
	int len_a = get_len(a);
	int i = 0;
	for (; i < len_a; i++) {
		if (max < a[i]) {
			max = a[i];
		}
	}

	return max;
}

/// input---------
/// A: n*m
/// b: n
/// x: m
/// output--------
/// y: n
void fc(float *x, float *A, float *b, float *y) {
	int n = get_len(b), m = get_len(x);

	int i = 0;
	for (; i < n; i++) {
		y[i] = b[i];

		int j = 0;
		for (; j < m; j++) {
			y[i] += A[i * m + j] * x[j];
		}
	}
}

void relu(float *x, float *y) {
	int i = 0, len_x = get_len(x);
	for (; i < len_x; i++) {
		if (x[i] > 0) {
			y[i] = x[i];
		} else {
			y[i] = 0.0;
		}
	}
}

/// x: 10
void softmax(float *x, float *y) {
	float x_max = get_max(x);
	int len_x = get_len(x);

	// NOTE: performance reason;
	float exp_x[len_x];
	float sum = 0.0;

	int i = 0;
	for (; i < len_x; i++) {
		exp_x[i] = expf(x[i] - x_max);
		sum += exp_x[i];
	}

	for (i = 0; i < len_x; i++) {
		y[i] = exp_x[i] / sum;
	}
}

struct InferenceReturn {
	float rslt_of_fc1[50];
	float rslt_of_fc2[100];
	float rslt_of_fc3[10];
	float rslt_of_relu1[50];
	float rslt_of_relu2[100];
	float rslt_of_softmax[10];
};

// struct Inference_Return init_inference_return() {}

void inference(struct MatrixAndVector *mav, float *x,
					struct InferenceReturn *rslt) {
	fc(x, mav->A.for_fc1, mav->b.for_fc1, rslt->rslt_of_fc1);
	relu(rslt->rslt_of_fc1, rslt->rslt_of_relu1);
	fc(rslt->rslt_of_relu1, mav->A.for_fc2, mav->b.for_fc2, rslt->rslt_of_fc2);
	relu(rslt->rslt_of_fc2, rslt->rslt_of_relu2);
	fc(rslt->rslt_of_relu2, mav->A.for_fc3, mav->b.for_fc3, rslt->rslt_of_fc3);

	softmax(rslt->rslt_of_fc3, rslt->rslt_of_softmax);
}

float calc_loss(int *t, float *y) {
	float sum = 0.0;
	int len_y = get_len(y);
	int i = 0;

	for (; i < len_y; i++) {
		sum += (float)t[i] * logf(y[i]);
	}

	return sum;
}

void shuffle(float *img_data) {}

void sgd() {
	int batch_size = 100;
	int w, h, count;
	float *img_data = load_mnist_image(
		 "data-samplecode-v14/train-images-idx3-ubyte", &w, &h, &count);
	shuffle(img_data);
}

void back_prop(float *x, int *t, struct MatrixAndVector *mav,
					struct InferenceReturn *ir, float *div_to_fc3,
					float *div_to_relu2, float *div_of_A3, float *div_of_b3,
					float *div_to_fc2, float *div_to_relu1, float *div_of_A2,
					float *div_of_b2, float *div_to_fc1, float *div_of_A1,
					float *div_of_b1) {
	int i = 0;
	// softmax + loss -> fc3
	for (i = 0; i < 10; i++) {
		div_to_fc3[i] = ir->rslt_of_softmax[i] - t[i];
	}

	// fc3 -> relu2
	for (i = 0; i < 100; i++) {
		float sum = 0.0;
		int j = 0;
		for (j = 0; j < 10; j++) {
			sum += mav->A.for_fc3[i * 100 + j] * div_to_fc3[j];
			div_of_A3[i * 100 + j] = div_to_fc3[j] * ir->rslt_of_relu2[i];
		}
		div_to_relu2[i] = sum;
	}
	for (i = 0; i < 10; i++) {
		div_of_b3[i] = div_to_fc3[i];
	}

	// relu2 -> fc2
	for (i = 0; i < 100; i++) {
		if (ir->rslt_of_fc2[i] > 0) {
			div_to_fc2[i] = ir->rslt_of_fc2[i];
		} else {
			div_to_fc2[i] = 0;
		}
	}

	// fc2 -> relu1
	for (i = 0; i < 50; i++) {
		float sum = 0.0;
		int j = 0;
		for (j = 0; j < 100; j++) {
			sum += mav->A.for_fc2[i * 50 + j] * div_to_fc2[j];
			div_of_A2[i * 50 + j] = div_to_fc2[j] * ir->rslt_of_relu1[i];
		}
		div_to_relu1[i] = sum;
	}
	for (i = 0; i < 10; i++) {
		div_of_b2[i] = div_to_fc2[i];
	}

	// relu1 -> fc1
	for (i = 0; i < 50; i++) {
		if (ir->rslt_of_fc1[i] > 0) {
			div_to_fc1[i] = ir->rslt_of_fc1[i];
		} else {
			div_to_fc1[i] = 0;
		}
	}

	// fc1
	for (i = 0; i < 50; i++) {
		int j = 0;
		for (j = 0; j < 784; j++) {
			div_of_A1[i * 50 + j] = div_to_fc1[i] * x[j];
		}
	}

	sgd();
}

void training(struct MatrixAndVector *mav, float *x, int *t) {
	struct InferenceReturn ir;
	//= init_inference_return();
	inference(mav, x, &ir);
	float loss = calc_loss(t, ir.rslt_of_softmax);

	float div_of_A1[50 * 784], div_of_b1[784];
	float div_to_fc1[50];
	float div_to_relu1[50], div_of_A2[50 * 100], div_of_b2[100];
	float div_to_fc2[100];
	float div_to_fc3[10];
	float div_to_relu2[100], div_of_A3[100 * 10], div_of_b3[10];

	back_prop(x, t, mav, &ir, div_to_fc3, div_to_relu2, div_of_A3, div_of_b3,
				 div_to_fc2, div_to_relu1, div_of_A2, div_of_b2, div_to_fc1,
				 div_of_A1, div_of_b1);
}

int main() {
	// training();
	//  inference();
	return 0;
}

#include <stdio.h>

void minmax(int data[], int *min, int *max) {
	printf("input 1st integer : ");
	scanf("%d", &data[0]);
	printf("input 2nd integer : ");
	scanf("%d", &data[1]);
	printf("input 3rd integer : ");
	scanf("%d", &data[2]);

	if (data[0] > data[1]) {
		if (data[1] > data[2]) {
			*max = data[0];
			*min = data[2];
		} else {
			if (data[0] > data[2]) {
				*max = data[0];
			} else {
				*max = data[2];
			}
			*min = data[1];
		}
	} else { // data[0] < data[1]
		if (data[0] > data[2]) {
			*max = data[1];
			*min = data[2];
		} else {
			if (data[1] < data[2]) {
				*max = data[2];
			} else {
				*max = data[1];
			}
			*min = data[0];
		}
	}
}

int main() {
	int *min, *max, data[3];
	minmax(data, min, max);
	printf("min: %d, max: %d", *min, *max);
}

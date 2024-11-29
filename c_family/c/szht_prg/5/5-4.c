#include <stdio.h>

int input() {
	printf("n=");

	int n = 0;
	scanf("%d", &n);
	return n;
}

void print_binary_order(int n) {
	printf("%d(10)=", n);
	int b[32];
	int i = 0;

	for (i = 0; i < 32; i++) {
		b[i] = (n >> i) & 1;
	}

	for (i = 31; i >= 0; i--) {
		printf("%d", b[i]);
	}
	printf("(2)");
}

int main() {
	int n = input();
	print_binary_order(n);

	return 0;
}

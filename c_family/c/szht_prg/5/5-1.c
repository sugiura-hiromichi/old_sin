#include <stdio.h>

int c(int n, int r) {
	if (n < r) {
		return 0;
	} else if (n == r || r == 0) {
		return 1;
	} else if (r == 1) {
		return n;
	} else {
		return c(n - 1, r - 1) + c(n - 1, r);
	}
}

int main() {
	int n = 0, r = 0;
	printf("input n & r\nn: ");
	scanf("%d", &n);
	printf("r: ");
	scanf("%d", &r);
	printf("result is %d", c(n, r));
}

#include <math.h>
#include <stdio.h>

int main() {
	double a = 1.0, b = pow(10, 15), c = pow(10, 14), x = 0.0;
	x = (-b + sqrt(b * b - 4.0 * a * c)) / (2.0 * a);
	printf("%f\n", x);
	x = (2.0 * c) / (-b - sqrt(b * b - 4.0 * a * c));
	printf("%f", x);
}

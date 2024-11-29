// a1: 1.00000000000000000000
// a2: 1.99999999392252902908
// a3: 1.99999999392252902908
//
// a1が1.00...なのはfloat型の有効桁数が約6桁な為、1e-8を充分に表現できないから
// a2とa3が同じなのは、double型の時点で1e-8を表すには十分な精度を持っていたから

#include <stdio.h>

int main() {
	float a1 = 1.0;
	double a2 = 1.0;
	long double a3 = 1.0;

	int i = 0;
	for (i = 0; i < 1e+8; i++) {
		a1 = a1 + 1e-8;
		a2 = a2 + 1e-8;
		a3 = a3 + 1e-8;
	}
	float f = 1e-8;
	double d = 1e-8;
	long double d2 = 1e-8;
	printf("float:\t\t%.15f\n", f);
	printf("double:\t\t%.15f\n", d);
	printf("long double:\t%.15Lf\n\n", d2);

	printf("a1: %.20f\n", a1);
	printf("a2: %.20f\n", a2);
	printf("a3: %.20Lf", a3);
}

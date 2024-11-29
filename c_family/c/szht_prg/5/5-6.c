// プログラムにおいて、2進法で表した時に小数点以下が無限に続くような数は
// 完全に正確に表すことはできない。従って現実の値と誤差が生じる。
// ここでは1.0/3.0がそれにあたり、xが完全に0と等しくなることはない
#include <math.h>
#include <stdio.h>

int main() {
	float x = 1.0;
	int i = 0;
	for (i = 0; i < 10; i++) {
		x = x - 1.0 / 3.0;
		if (fabsf(x) < 0.000001) {
			break;
		}
		printf("%f\n", x);
	}
	return 0;
}

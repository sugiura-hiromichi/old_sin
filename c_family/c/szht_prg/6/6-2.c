/* output
---
value: S, address 0x16db6b23c
value: u, address 0x16db6b23d
value: n, address 0x16db6b23e
value: 10, address 0x16db6b258
value: 20, address 0x16db6b25c
value: 40, address 0x16db6b260
value: 1.500000, address 0x16db6b240
value: 3.500000, address 0x16db6b248
value: 7.500000, address 0x16db6b250
---
*/

#include <stdio.h>

int main() {
	char str[] = "Sun";
	int n[] = {10, 20, 40};
	double m[] = {1.5, 3.5, 7.5};

	printf("value: %c, address %p\n", str[0], &str[0]);
	printf("value: %c, address %p\n", str[1], &str[1]);
	printf("value: %c, address %p\n", str[2], &str[2]);
	printf("value: %d, address %p\n", n[0], &n[0]);
	printf("value: %d, address %p\n", n[1], &n[1]);
	printf("value: %d, address %p\n", n[2], &n[2]);
	printf("value: %f, address %p\n", m[0], &m[0]);
	printf("value: %f, address %p\n", m[1], &m[1]);
	printf("value: %f, address %p\n", m[2], &m[2]);
}

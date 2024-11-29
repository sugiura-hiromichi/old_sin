#include <stdio.h>

int main() {
	char str[10];
	printf(" In: ");
	scanf("%s", str);
	char diff = 'A' - 'a';

	int i = 0;
	while (str[i]) {
		if (str[i] >= 'A' && str[i] <= 'Z') {
			str[i] = str[i] - diff;
		} else if (str[i] >= 'a' && str[i] <= 'z') {
			str[i] = str[i] + diff;
		}
		i++;
	}

	printf("Out: %s", str);
}

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
/// INFO: This file is `test.c`

void t1() {
	char *str = malloc(20);
	sprintf(str, "+sprintf");
	assert(strcmp(str, "+sprintf") == 0);
}

void t2() {
	char *program = malloc(20);
	snprintf(program, 19, "%s", "abcde");
	assert(strcmp("abcde", program) == 0);
}

void t3() {
	char command[] = "cd ; exa";
	system(command);
}

int main() {
	t1();
	t2();
	t3();
}

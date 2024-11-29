#include <assert.h>
#include <stdio.h>
/// INFO: This file is `test.c`

void check(char *word_answers[]) {
	int problems = sizeof(word_answers) / sizeof(word_answers[0]);
	printf("-------------- problems : %d", problems);
	// assert(problems == 4);
}

int main() {
	char *word_answers[] = {"c", "physics", "program", "∆"};
	check(word_answers);
	int problems = sizeof(word_answers) / sizeof(word_answers[0]);
	printf("-------------- problems : %d", problems);
}

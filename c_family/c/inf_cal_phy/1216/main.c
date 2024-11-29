#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT_LEN 100

int check_input(char *answers[], int len) {
	char input[INPUT_LEN];
	int accuracy = 0;
	for (int i = 0; i < len; i++) {
		printf("\n\n[ %s ]\n> ", answers[i]);
		scanf("%s", input);
		if (strcmp(input, answers[i]) == 0) {
			printf("correct!");
			accuracy++;
		} else {
			printf("wrong");
			accuracy--;
		}
	}

	return accuracy;
}

void rslt(int problems, int accuracy) {
	printf("result-----------------\n");
	int passed = problems - (problems - accuracy) / 2;

	printf("all problems: %d\n", problems);
	printf("passed: %d\n", passed);
	printf("failed: %d\n", problems - passed);
	printf("accuracy: %f", (double)passed * 100 / (double)problems);
}

// typing game
int main() {
	printf("[typing game]\n");
	printf("type displayed word correctry\n-----------------------");

	// `problems` is number of problem. accuracy is used to calc accuracy
	int problems = 0, accuracy = 0;

	const char *word_answers[] = {"c", "physics", "program", "∆"};
	problems += sizeof(word_answers) / sizeof(word_answers[0]);
	accuracy += check_input(word_answers, problems);

	printf("\nfinished!\n");

	rslt(problems, accuracy);
}

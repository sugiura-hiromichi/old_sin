#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PROGRAM_LEN 100

void create_tiny_program(char content[]) {
	FILE *f;
	if ((f = fopen("tiny.c", "w")) == NULL) {
		printf("ERROR: failed to open file");
		exit(1);
	}

	fprintf(f, "%s", content);
	fclose(f);
}

int main() {
	int dbg = 1;
	char template[] = "#include <stdio.h>\nint main(){";
	char *program = malloc(MAX_PROGRAM_LEN);
	printf("input your code > ");

	if (dbg == 0) {
		snprintf(program, 20, "printf(\"aaa\");");
	} else {
		fgets(program, MAX_PROGRAM_LEN, stdin);
	}

	char *content = malloc(2 * MAX_PROGRAM_LEN);
	snprintf(content, 2 * MAX_PROGRAM_LEN, "%s%s}", template, program);

	create_tiny_program(content);
	if (system("cc tiny.c -o TINY.out ; ./TINY.out") == -1) {
		printf("Error occured when executing cc tiny.c -o TINY.out ; ./TINY.out");
	}
}

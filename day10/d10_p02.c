#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d10_short_input"
#define INPUT	    "d10_input"

void reset_stack();
int middle(int len, long* scores);

char obracks[4] = {'(', '[', '{', '<'};
char cbracks[4] = {')', ']', '}', '>'};
int  points [4] = { 1,   2,   3,   4 }; 

char *stack;
char *sp;
void push(char c);
char pop();

int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

	int lines = 1;
	while (1) {
		char c = fgetc(fp);
		if (c == '\n') lines++;
		if (c == EOF) break;
	}

	long scores[lines];
	int index = 0;

	rewind(fp);

	reset_stack();

	// go through all lines
	int corrupted;
	char line[200];
	while (fgets(line, 200, fp) != NULL) {
		corrupted = 0;

		// go through all chars in line
		int i = 0;
		while (!corrupted) {
			char c = line[i];
			if (c == '\n') break;
			
			for (int i = 0; i < 4 && !corrupted; i++) {
				if (c == obracks[i])
					push(c);

				if (c == cbracks[i]) {
					char o = pop();
					if (o != obracks[i])
						corrupted = 1;
				}
			}

			i++;
		}

		if (!corrupted) {
			long line_score = 0;

			while (1) {
				char o = pop();
				if (o == 'E') break;

				for (int i = 0; i < 4; i++) {
					if (o == obracks[i]) {
						char c_match = cbracks[i]; 
						line_score *= 5;
						line_score += points[i];
					}
				}

			}

			scores[index++] = line_score;
		}
		reset_stack();
	}

	int score = middle(index, scores);
	printf(" score = %i\n", score);

	return 0;
}


void push(char c) {
	sp++;
	*sp = c;
} 

char pop() {
	char c = *sp;
	sp--;
	return c;
}

void reset_stack() {
	if (stack != NULL) free(stack);
	stack = malloc(200 * sizeof(char));
	stack[0] = 'E';
	sp = stack + 1;
}

int middle(int len, long* scores) {
	int i, j;
	for (i = 0; i < len-1; i++) {
		for (j = 0; j < len-i-1; j++) {
			if (scores[j] > scores[j+1]) {
				long temp = scores[j];
				scores[j] = scores[j+1];
				scores[j+1] = temp;
			}
		}
	}

	return scores[len/2];
}

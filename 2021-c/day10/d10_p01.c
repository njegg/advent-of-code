#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d10_short_input"
#define INPUT	    "d10_input"

void reset_stack();

char obracks[4] = {'(', '[', '{', '<'};
char cbracks[4] = {')', ']', '}', '>'};
int  points [4] = {3, 57, 1197, 25137}; 

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
	
	reset_stack();

	// go through all lines
	int score = 0, corrupted;
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
					
					if (o != obracks[i]) {
						corrupted = 1;
						score += points[i];
					}
				}
			}

			i++;
		}
		reset_stack();
	}

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

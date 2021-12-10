#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d10_short_input"
#define INPUT	    "d10_input"

void reset_counter();
void print_counter();
void reset_stack();

char obracks[4] = {'(', '[', '{', '<'};
char cbracks[4] = {')', ']', '}', '>'};
int  counter[4] = { 0 ,  0 ,  0 ,  0 };
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
			printf("%c", c);
			if (c == '\n') break;
			
			for (int i = 0; i < 4 && !corrupted; i++) {
				if (c == obracks[i]) counter[i]++;
				if (c == cbracks[i]) {
					counter[i]--;
					if (counter[i] < 0) {
						// first illegal
						score += points[i];
						corrupted = 1;
					}
				}
			}

			i++;
		}
		print_counter();
		printf("\n");
		reset_counter();


		/* break; */

		
		/* printf("%s\n", line); */
			
	}
	printf("score = %i\n", score);

	return 0;
}


void reset_counter() {
	for (int i = 0; i < 4; i++)
		counter[i] = 0;
}


void print_counter() {
	for (int i = 0; i < 4; i++)
		printf("%i ", counter[i]);
	printf("\n");
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


#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>

#define INPUT       "d15_input"
#define SHORT_INPUT "d15_short_input"
#define MAX_32 		~(1<<31)

/* --- Day 15: Chiton --- */

int ** m;
int x = 0, y = 0;

int main(int ac, char** args) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

	char c;
	while (1) {
		if ((c = fgetc(fp)) == EOF) break;
		if (c == '\n') y++;
		if (y == 0) x++;
	}
		
	rewind(fp);

	m = (int**) malloc(y * sizeof(int*));
	for (int i = 0; i < y; i++) {
		m[i] = (int*) malloc(x * sizeof(int));
	}

	// dynamic brogramin
	int** dm = (int**) calloc(y, sizeof(int*));
	for (int i = 0; i < y; i++) {
		dm[i] = (int*) calloc(x, sizeof(int));
	}

	for (int i = 0; i < y; i++) {
		for (int j = 0; j < x; j++) {
			m[i][j] = fgetc(fp) - '0';
		}
		fgetc(fp);
	}
	

	for (int i = 0; i < y; i++) {
		for (int j = 0; j < x; j++) { 
        	int here = dm[i][j];

			if (i + 1 < y) {
				int down = here + m[i + 1][j];
				if (dm[i + 1][j] == 0 || down < dm[i + 1][j])
					dm[i + 1][j] = down;
			}

			if (j + 1 < x) {
				int right = here + m[i][j + 1];
				if (dm[i][j + 1] == 0 || right < dm[i][j + 1])
					dm[i][j + 1] = right;
			}
		}
	}

	printf("minimal risk = %i\n", dm[y-1][x-1]);

	return 0;
}


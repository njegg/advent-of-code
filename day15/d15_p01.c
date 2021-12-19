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

int min_risk = MAX_32;


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
				int up   = here + m[i][j + 1];
				if (dm[i][j + 1] == 0 || up < dm[i][j + 1])
					dm[i][j + 1] = up;
			}
		}
	}

	printf("%i\n", dm[y-1][x-1]);

	/* for (int i = 0; i < y; i++) { */
	/* 	for (int j = 0; j < x; j++) { */
	/* 		printf("%i", m[i][j]); */
	/* 	} */
	/* 	printf("\n"); */
	/* } */

	/* printf("%ix%i\n", x, y); */
	
	return 0;
}


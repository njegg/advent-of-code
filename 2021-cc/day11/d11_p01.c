#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d11_short_input"
#define INPUT	    "d11_input"

int x, y;
int** m;
int n_step = 1;
int ni[8] = {-1, -1, -1,  0,  0,  1,  1,  1 };
int nj[8] = {-1,  0,  1, -1,  1, -1,  0,  1 };


void increase_neighbours(int i, int j) {
	for (int d = 0; d < 8; d++) {
		int di = i + ni[d];
		int dj = j + nj[d];

		int out_of_bounds = 0;
		if (di < 0 || di >= y) out_of_bounds = 1;
		if (dj < 0 || dj >= x) out_of_bounds = 1;

		if (!out_of_bounds) {
			m[di][dj]++;

			if (m[di][dj] == 10) {
				increase_neighbours(di, dj);
			}
		}
	}
}

int step() {
	int flashes = 0;

	// increase all octopuses
	for (int i = 0; i < y; i++) {
		for (int j = 0; j < x; j++) {
			m[i][j]++;

			if (m[i][j] == 10) {
				increase_neighbours(i, j);			
			}
		}
	}

	// check for flashes
	printf("\nAfter step %i:\n", n_step++);
	for (int i = 0; i < y; i++) {
		for (int j = 0; j < x; j++) {
			if (m[i][j] > 9) {
				m[i][j] = 0;
				flashes++;
			} 

			printf("%i", m[i][j]);
		}
		printf("\n");
	}

	return flashes;
}


int main(int ac, char** args) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

	y = 0;
	x = 0;
	int x_done = 0;

	// calc x and y of matrix
	while (1) {
		char c = fgetc(fp);

		if (c == '\n') {
			y++;
			x_done = 1;
			continue;
		} else if (c == EOF) break;

		if (!x_done) x++;
	}

	rewind(fp);

	m = (int**)malloc(x * y * sizeof(int));
	for (int i = 0; i < y; i++)
		m[i] = (int*)malloc(x * sizeof(int));
	
	// read file
	for (int i = 0; i < y; i++) {
		for (int j = 0; j < x; j++) {
			m[i][j] = fgetc(fp) - '0';
		}
		fgetc(fp); // read '/n'
	}

	int flashes = 0;
	int steps = 100;
	while (n_step <= 100) {
		flashes += step();
	}

	printf("\nflashes after %i steps = %i\n", steps, flashes);

	for (int i = 0; i < y; i++)
		free(m[i]);
	free(m);

	return 0;
}


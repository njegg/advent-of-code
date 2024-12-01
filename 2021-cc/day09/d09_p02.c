#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d09_short_input"
#define INPUT	    "d09_input"

int traverse(int x, int y, int (*m)[x], int i, int j);
void put_if_max(int size);

int y_size = 1;
int x_size = 0;
int max[3] = {0};

int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

	int x_done = 0;

	while (1) {
		char c = fgetc(fp);

		if (c == '\n') {
			y_size++;
			x_done = 1;
			continue;
		} else if (c == EOF) break;

		if (!x_done) x_size++;
	}

	rewind(fp);

	int m[y_size][x_size];

	int i = 0;
	while (1) {
		for (int j = 0; j < x_size; j++)
			fscanf(fp, "%1d", &m[i][j]);

		i++;

		if (fgetc(fp) == EOF) break;
	}

    int size = -1, found = 1;
    int start_i = 0, start_j = 0;

    while (found) {
        found = 0;
        start_i = 0;
        start_j = 0;

        for (int i = 0; i < y_size && !found; i++) {
            for (int j = 0; j < x_size && !found; j++) {
                int n = m[i][j];

                if (n != -1 && n != 9) {
                    start_j = j;
                    start_i = i;
                    found = 1;
                }
            }
        }

        if (found) {
            size = traverse(x_size, y_size, m, start_i, start_j);
            put_if_max(size);
        }

    }

    printf("result = %i\n", (max[0] * max[1] * max[2]));
    return 0;
}


int traverse(int x, int y, int (*m)[x], int i, int j) {
    if (i < 0 || i >= y_size) return 0;
    if (j < 0 || j >= x_size) return 0;
    if (m[i][j] == 9)         return 0;
    if (m[i][j] == -1)        return 0;

    m[i][j] = -1;

    int sum = 0;

    sum += traverse(x, y, m, i+1, j);
    sum += traverse(x, y, m, i-1, j);
    sum += traverse(x, y, m, i, j+1);
    sum += traverse(x, y, m, i, j-1);

    return sum + 1;
}

void put_if_max(int size) {
    int index = -1;

    for (int i = 0; i < 3; i++)
        if (size > max[i]) index = i;
    
    
    if (index > -1) {
        for (int i = 0; i < index; i++)
            max[i] = max[i+1];

        max[index] = size;
        
    }
}

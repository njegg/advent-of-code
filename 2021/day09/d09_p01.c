#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d09_short_input"
#define INPUT	    "d09_input"


int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

	int y_size = 1;
	int x_size = 0;
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
	printf("x=%i, y=%i\n", x_size, y_size);

	rewind(fp);

	int m[y_size][x_size];

	int i = 0;
	while (1) {
		for (int j = 0; j < x_size; j++) {
			fscanf(fp, "%1d", &m[i][j]);
			printf("%i", m[i][j]);
		}
		i++;
		printf("\n");

		if (fgetc(fp) == EOF) break;
	}
	printf("\n");

	
	int di[4] = {1, 0, -1,  0};
	int dj[4] = {0, 1,  0, -1};

	int n, good, sum = 0;
	for (int i = 0; i < y_size; i++) {
		for (int j = 0; j < x_size; j++) {
			good = 1;

			for (int k = 0; k < 4; k++) {
				if (i + di[k] >= 0 && i + di[k] < y_size && j + dj[k] >= 0 && j + dj[k] < x_size) {
					if (m[i][j] >= m[i + di[k]][j + dj[k]]) {
						good = 0;
					}
				} 
			}
            printf("%i", good);
			
			if (good) sum += m[i][j]+1;
		}
		printf("\n");
	}

	printf("\nsum = %i\n", sum);

}





// int di[4] = {1, 0, -1,  0};
	// int dj[4] = {0, 1,  0, -1};

			// for (int k = 0; k < 4; k++) {
			// 	if (i + di[k] >= 0 && i + di[k] < y_size && j + dj[k] >= 0 && j + dj[k] < x_size) {
			// 		if (n > m[i + di[k]][j + dj[k]]) {
			// 			good = 0;
			// 			break;
			// 		}
			// 	}
			// }
			
			// if (good == 1) sum += n+1;

	// for (int i = 0; i < y_size; i++) {
	// 	for (int j = 0; j < x_size; j++) {
	// 		int n = m[i][j];
	// 		good = 1;

	// 		if (i+1 < y_size && n > m[i+1][j]) { good = 0; } 
	// 		if (j+1 < x_size && n > m[i][j+1]) { good = 0; }
	// 		if (j-1 >= 0     && n > m[i][j-1]) { good = 0; }
	// 		if (i-1 >= 0     && n > m[i-1][j]) { good = 0; }

	// 		if (good) sum += n+1;
	// 	}
	// }
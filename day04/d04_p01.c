#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d04_short_input"
#define INPUT	    "d04_input"
#define ZERO		1<<31


int count_boards(FILE* fp);
void print_boards_and_draws(int* d, int ds, int (*b)[5][5], int bc);
int board_sum(int (*b)[5][5], int i);


int main(void) {

	FILE *fp = fopen(SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
		return 1;
	}

	int board_count = count_boards(fp);

	// read first line
	int draws[200];
	int draw_size = 0;
	while (1) {
		fscanf(fp, "%i", &draws[draw_size++]);
		char c = fgetc(fp);
		if (c == '\n') break;
	}
	fgetc(fp); 

	// read boards

	int boards[board_count][5][5];

	int i = 0;
	while (1) {

		for (int j = 0; j < 5; j++) {
			for (int k = 0; k < 5; k++) {
				fscanf(fp, "%i", &boards[i][j][k]);
				fgetc(fp);
			}
		}

		if (fgetc(fp) == EOF) break;
		i++;
	}


	/*
		notes
		every match is flaged with negative value
		of that number
		
		for every drawn number, flag
		them on the boards, while doing that
		calculate the sum of a row (0, 5),
		at the same time add the curr column
		to the array of size 5 that keeps
		the sum of every column (0, 5)

		sum is increased by 1 if number is negative
		after going through a row, check if the sum
		is 5 (5 negative flags)

		checks columns after the whole board
		(goes through col_sum array)

		problem: 0 cant be negative
		solution: flag it with a min integer
	*/

	for (int d = 0; d < draw_size; d++) {
		
		int draw = draws[d];
		for (int  i = 0; i < board_count; i++) {

			int* col_sum = calloc(5, sizeof(int));
			for (int j = 0; j < 5; j++) {

				int row_sum = 0;
				for (int k = 0; k < 5; k++) {
					
					int n = boards[i][j][k];
					
					if (n == draw)
						n = n == 0 ? ZERO : -n; 

					int sum = n < 0 ? 1 : 0;
					row_sum += sum;
					col_sum[k] += sum;

					boards[i][j][k] = n;
				}

				if (row_sum == 5) {
					int win_sum = 0;

					int sum = board_sum(boards, i);
					printf("win number = %i\n", draw);
					printf("sum = %i\n", sum);
					printf("resault = %i\n", (sum * draw));
					return 0;
				}

			}

			for (int b = 0; b < 5; b++) {
				if (col_sum[b] == 5) {
					int sum = board_sum(boards, i);
					printf("resault = %i\n", (sum * draw));
					printf("sum = %i\n", sum);
					printf("win number = %i\n", draw);

					return 0;
				}
			}

			free(col_sum);
		}
	}

	// print_boards_and_draws(draws, draw_size, boards, board_count);
	fclose(fp);
}


int board_sum(int (*b)[5][5], int i) {

	int resAult = 0;
	for (int j = 0; j < 5; j++) {
		for (int k = 0; k < 5; k++) {
			int n = b[i][j][k];
			if (n > 0) resAult += n;
		}
	}
	return resAult; 
}


int count_boards(FILE* fp) {
	int ans = 0;
	char cur, prev;
	
	cur = fgetc(fp);
	while(1) {
		prev = cur;
		cur = fgetc(fp);
		if (cur == EOF) break;
		if (prev == '\n' && cur == '\n') ans++;
	}

	rewind(fp);
	return ans;
}


void print_boards_and_draws(int* draws, int draw_size, int (*boards)[5][5], int board_count) {

	for (int i = 0; i < draw_size; i++) {
		printf("%i ", draws[i]);
	}
	printf("\n\n");
	

	for (int  i = 0; i < board_count; i++) {
		for (int j = 0; j < 5; j++) {
			for (int k = 0; k < 5; k++) {
				printf("%i ", boards[i][j][k]);
			}
			printf("\n");
		}
		printf("\n");
	}

}


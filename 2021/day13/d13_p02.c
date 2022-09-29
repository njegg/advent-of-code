#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>

#define INPUT       "d13_input"
#define SHORT_INPUT "d13_short_input"

/* --- Day 13: Transparent Origami --- */

int** paper;
int folds[20][2];
int fold_count = 0;
int paper_x = -1;
int paper_y = -1;

int fold(char axis, int where) {
	int new_y = axis == 'y' ? where : paper_y;
	int new_x = axis == 'x' ? where : paper_x;

	printf("paper folded on %c at %i, new size = %ix%i\n", axis, where, new_x, new_y);

	int visible = 0;

	for (int y = 0; y < new_y; y++) {
		for (int x = 0; x < new_x; x++) {

			if (axis == 'y') {
				int opposite = y + 2 * (new_y - y);
				if (opposite < paper_y && paper[opposite][x] == 1) {
					paper[y][x] = 1;
				}
			} else {
				int opposite = x + 2 * (new_x - x);
				if (opposite < paper_x && paper[y][opposite] == 1) {
					paper[y][x] = 1;
				}
			}

			printf("%c", paper[y][x] ? '#' : '.');

			if (paper[y][x] == 1) {
				visible++;
			}
		}
		printf("\n");
	}

	if (axis == 'y') paper_y = where;
	else 			 paper_x = where;

	return visible;
}


int main(int ac, char** args) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

	char line[20];
	int x, y;
	while (fscanf(fp, "%s" , line) > 0) {
		if (line[0] == '\n') break;
		fgetc(fp);

		sscanf(line, "%i,%i", &x, &y);
		
		if (x > paper_x) {
			paper_x = x;
		}

		if (y > paper_y) {
			paper_y = y;
		}

		char c = fgetc(fp);
		if (c == '\n') break;
		else ungetc(c, fp);
	}
	paper_x++;
	paper_y++;

	rewind(fp);

	paper = (int**) calloc(paper_y, sizeof(int*));
	for (int i = 0; i < paper_y; ++i)
		paper[i] = (int*) calloc(paper_x, sizeof(int));

	while (fscanf(fp, "%s" , line) > 0) {
		if (line[0] == '\n') break;
		fgetc(fp);

		sscanf(line, "%i,%i", &x, &y);
		paper[y][x] = 1;

		char c = fgetc(fp);
		if (c == '\n') break;
		else ungetc(c, fp);
	}

	while (fscanf(fp, "%[ foldalong]s" , line) > 0) {
		char axis = fgetc(fp);
		fgetc(fp);
		int value;
		fscanf(fp, "%d", &value);

		folds[fold_count][0] = axis;
		folds[fold_count++][1] = value;

		fgetc(fp);
	}

	printf("\n");

	for (int i = 0; i < fold_count; i++) {
		int visible = fold(folds[i][0], folds[i][1]);
		printf("\nvisible dots = %i\n", visible);
	}

	for (int i = 0; i < paper_y; ++i)
		free(paper[i]);
	free(paper);

	return 0;
}


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define INPUT       "d14_input"
#define SHORT_INPUT "d14_short_input"

char template[30];

int pairs_size;
char** pairs;
long* pair_count;


int find_pair(char a, char b) {
	for (int i = 0; i < pairs_size; i++) {
		if (pairs[i][0] == a && pairs[i][1] == b) {
			return i;
		}
	}

	return -1;
}

void inc_pair(char a, char b) {
	for (int i = 0; i < pairs_size; i++) {
		if (pairs[i][0] == a && pairs[i][1] == b) {
			pair_count[i]++;
			return;
		}
	}
}

int main(int ac, char** args) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

	fscanf(fp, "%s", template);
	fgetc(fp); // '\n'
	fgetc(fp);

	int init_size = strlen(template);

	char c;
	while ((c = fgetc(fp)) != EOF)  {
		if (c == '\n') pairs_size++;
	}
	rewind(fp);
	
	pairs = (char**) malloc(pairs_size * sizeof(char*));
	for (int i = 0; i < pairs_size; i++) {
		pairs[i] = malloc(3 * sizeof(char));
	}

	char line[30];
	fscanf(fp, "%s\n", line);

	int pair_index = 0;
	while (fscanf(fp, "%c%c -> %c\n",
				&pairs[pair_index][0],
				&pairs[pair_index][1],
				&pairs[pair_index][2]) > 0) {

		pair_index++;
	}
 
	pair_count = (long*) calloc(pairs_size, sizeof(long));

	int template_size = strlen(template);
	for (int i = 0; i < template_size - 1; i++) {
		inc_pair(template[i], template[i + 1]);
	}

	int steps = 10;

	for (int s = 1; s <= steps; s++) {
		long* temp = calloc(pairs_size, sizeof(long));

		for (int i = 0; i < pairs_size; i++) {
			int count = pair_count[i];
			char ins = pairs[i][2];

			int pair1 = find_pair(pairs[i][0], ins);
			int pair2 = find_pair(ins, pairs[i][1]);

			temp[pair1] += count;
			temp[pair2] += count;

			if (count > 0) {
				temp[i] -= count;
			}
		}

		for (int i = 0; i < pairs_size; i++) {
			pair_count[i] += temp[i];
		}

		free(temp);
	}
	
	long element_count[30] = {0};
	for (int i = 0; i < pairs_size; i++) {
		element_count[pairs[i][0] - 'A'] += pair_count[i];
	}

	element_count[template[template_size - 1] - 'A']++;

	long min = LONG_MAX;
	long max = LONG_MIN;
	
	for (int i = 0; i < 30; i++) {
		if (element_count[i] < min && element_count[i] > 0) {
			min = element_count[i];
		} 

		if (element_count[i] > max) {
			max = element_count[i];
		}
	}

	printf("resault = %li\n", (max - min));

	return 0;
}


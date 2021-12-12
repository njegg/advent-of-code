#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT       "d12_input"
#define SHORT_INPUT "d12_short_input"

int* sp;
int stack[100];
int stack_size = 0;

int pop();
void push(int cave);
void reset_stack();

char** caves;
int cave_count;
int max_caves = 50;

void free_caves();
int cave_index(char* cave);

int **graph;
void free_map(int size);

int paths = 0;

void find(int cave) {
	// if end, print path, go bacc
	if (strcmp(caves[cave], "end") == 0) {
		push(cave);

		for (int i = 0; i < stack_size; i++)
			printf("%s,", caves[stack[i]]);
		printf("\n");

		pop();
		paths++;
		return;
	}

	// if a small cave is already in path, skip
	char* cur_cave = caves[cave];
	if (cur_cave[0] > 'Z') {
		for (int i = 0; i < stack_size; i++) {
			if (strcmp(cur_cave, caves[stack[i]]) == 0) return;
		}
	}

	// add to path
	push(cave);

	// call find for all caves that connect to this
	for (int i = 0; i < cave_count; i++) {
		if (graph[i][cave]) find(i);
        if (graph[cave][i]) find(i);
	}

	pop();
	return;
}


int main(int ac, char** args) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

	caves = malloc(max_caves * 6 * sizeof(int));
	for (int i = 0; i < max_caves; i++)
		caves[i] = malloc(6 * sizeof(int));

	// read a set of caves
	char line[20];
	while (fscanf(fp, "%s" , line) > 0) {
		char* cave_a = strtok(line, "-");	
		char* cave_b = strtok(NULL, "-");	

		if (cave_index(cave_a) == -1)
			strcpy(caves[cave_count++], cave_a);

		if (cave_index(cave_b) == -1)
			strcpy(caves[cave_count++], cave_b);
	}

	rewind(fp);

	// adjacency matrix
	graph = (int **)calloc(cave_count * cave_count, sizeof(int));
	for (int i = 0; i < cave_count; i++)
	  graph[i] = calloc(cave_count, sizeof(int));

	// mark connected caves
	while (fscanf(fp, "%s" , line) > 0) {
		char* cave_a = strtok(line, "-");	
		char* cave_b = strtok(NULL, "-");	
		
		int i_a = cave_index(cave_a);
		int i_b = cave_index(cave_b);

		graph[i_a][i_b] = 1;
	}

	for (int i = 0; i < cave_count; i++) {
		for (int j = 0; j < cave_count; j++)
			printf("%i", graph[i][j]);
		printf("\n");
	}
	printf("\n");

	reset_stack();

	int start = -1;
	for (int i = 0; i < cave_count; i++) {
		if (strcmp(caves[i], "start") == 0) {
			start = i;
			break;
		} 
	}

	if (start == -1) printf("brokey\n");

	find(start);

	free_caves();
	free_map(cave_count);

	printf("\npaths found: %i\n", paths);
	return 0;
}


void push(int cave) {
	*sp = cave;
	sp++;
	stack_size++;
} 


int pop() {
	sp--;
	int cave = *sp;
	stack_size--;
	return cave;
}


void reset_stack() {
	stack_size = 0;
	sp = stack;
}


int cave_index(char* cave) {
	for (int i = 0; i < cave_count; i++)
		if (strcmp(caves[i], cave) == 0)
			return i;

	return -1;
}


void free_caves() {
	for (int i = 0; i < max_caves; i++)
		free(caves[i]);
	free(caves);
}


void free_map(int size) {
	for (int i = 0; i < size; i++)
          free(graph[i]);
        free(graph);
}


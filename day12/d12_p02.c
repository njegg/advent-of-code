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

int cave_index(char* cave);
void free_caves();

int **graph;
void free_map(int size);

int paths = 0;

int small_visited_twice = 0;

void find(int cave) {

	// if end, print path, go back
	if (strcmp(caves[cave], "end") == 0) {
		push(cave);
		for (int i = 0; i < stack_size; i++)
			printf("%s,", caves[stack[i]]);
		printf("\n");
		pop();
		paths++;
		return;
	}

	char* cur_cave = caves[cave];

	if (strcmp(cur_cave, "start") == 0 && stack_size > 0) return;

	int curr_visited_twice = 0;
	if (cur_cave[0] > 'Z') {
		for (int i = 0; i < stack_size; i++) {
			if (strcmp(cur_cave, caves[stack[i]]) == 0)	{
				if (small_visited_twice) return;
				small_visited_twice = 1;
				curr_visited_twice = 1;
			}
		}
	}

	// add to path
	push(cave);

	// call find for all caves that connect to this
	for (int i = 0; i < cave_count; i++) {
		if (graph[i][cave]) find(i);
		if (graph[cave][i]) find(i);
	}

	// notes: all paths where this cave is visited 2 times found
	// set the flag to 0 so other paths with 2 small caves
	// can be found
	if (curr_visited_twice) 
		small_visited_twice = 0;

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

	for (int i = 0; i < cave_count; i++)
		printf("%s\n", caves[i]);

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
	for (int i = 0; i < cave_count; i++) {
		if (strcmp(caves[i], cave) == 0) {
			return i;
		}
	}
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


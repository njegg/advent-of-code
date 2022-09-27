#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define INPUT       "d15_input"
#define SHORT_INPUT "d15_short_input"

/* --- Day 15: Chiton --- */

typedef struct Node
{
	int dst;
	int local;
	int global;
	struct Node *friends[4];
	struct Node *parent;
	int x;
	int y;
} Node;

float heuristic(int x, int y);


// Heap Priority Queue
Node **pq;
const int PQ_MAX = 1024;
int pq_size = 0;

void pq_insert(Node *);
Node * pq_pop();


// Matrix with nodes, grid
Node **m;
int w = 0, h = 0;

int main(int argc, char** args) {
    int input = argc > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		perror("fopen");
        return 1;
	}

	// Read input size
	char c;
	while (1) {
		if ((c = fgetc(fp)) == EOF) break;
		if (c == '\n') h++;
		if (h == 0) w++;
	}
		
	rewind(fp);

	// Alloc node grid
	m = (Node **) malloc(h * sizeof(Node *));
	for (int i = 0; i < h; i++)
		m[i] = (Node *) malloc(w * sizeof(Node));


	// Read input into nodes and setup nodes data
	for (int i = 0; i < h; i++) {
		for (int j = 0; j < w; j++) {
			Node *n = &m[i][j];
			n->dst = fgetc(fp) - '0';

			n->local = INT_MAX;
			n->global = INT_MAX;

			n->friends[0] = i > 0 ? 	 &m[i - 1][j] : NULL;
			n->friends[1] = j < w - 1 ? &m[i][j + 1] : NULL;
			n->friends[2] = i < h - 1 ? &m[i + 1][j] : NULL;
			n->friends[3] = j > 0 ?  	 &m[i][j - 1] : NULL;

			n->parent  = NULL;
			n->x = j;
			n->y = i;
		}
		fgetc(fp);
	}
	m[0][0].local = 0;
	
	pq = (Node **) malloc(PQ_MAX * sizeof(Node *));

	int **map = (int **) malloc(w * h * sizeof(int*));
	for (int i = 0; i < h; i++)
		map[i] = (int *) malloc(w * h * sizeof(int));

	
	// A*
	pq_insert(&m[0][0]);
	map[0][0] = 1;

	while (pq_size) {
		Node *cur = pq_pop();

		if (cur->global == 0) {
			break;
		}

		for (int i = 0; i < 4; i++) {
			Node *friend = cur->friends[i];
			if (!friend) continue;

			int to_friend = cur->local + friend->dst;
			if (to_friend < friend->local) {
				friend->parent = cur;
				friend->local = to_friend;
				friend->global = heuristic(friend->x, friend->y) + to_friend;
				if (!map[friend->y][friend->x]) {
					pq_insert(friend);
				}
			}
		}
	}

	printf("%i\n", m[h - 1][w - 1].global); 
	if (pq_size != 0) {
		printf("WARNING: More nodes in queue\n");
	}

	// Cleanup
	free(pq);

	for (int i = 0; i < h; i++)
		free(m[i]);
	free(m);

	for (int i = 0; i < h; i++)
		free(map[i]);
	free(map);

	return 0;
}


// Heap Priority Queue Impl

#define C1_INDEX (index * 2 + 1)
#define C2_INDEX (index * 2 + 2)
#define PA_INDEX ((index - 1) / 2)

void pq_insert(Node *node)
{
	int index = pq_size++;
	if (pq_size == PQ_MAX) {
		printf("ERROR: PQ Overload\n");
		return;
	}

	pq[index] = node;
	if (index == 0) return;

	Node *tmp;
	int pindex = (index - 1) / 2;
	while (index > 0 && pq[pindex]->global > pq[index]->global) {
		tmp = pq[index];
		pq[index] = pq[pindex];
		pq[pindex] = tmp;

		index = pindex;
		pindex = (index - 1) / 2;
	}
}

Node * pq_pop()
{
	if (pq_size == 0) return NULL;
	int end = --pq_size;

	Node *del = pq[0];
	if (!end) return del; 	// empty

	pq[0] = pq[end];		// swap first with last
	pq[end] = del;

	int index = 0;
	int c1 = C1_INDEX;
	int c2 = C2_INDEX;

	int min_index;
	Node *tmp;
	
	if (c1 < pq_size && c2 < pq_size) {
		min_index = pq[c1]->global < pq[c2]->global ? c1: c2;
	} else if (c1 < pq_size) {
		min_index = c1;			// only first child
	} else {
		return del;				// no children
	}

	while (pq[index]->global > pq[min_index]->global) { // restore heap
		tmp = pq[index];
		pq[index] = pq[min_index];
		pq[min_index] = tmp;

		index = min_index;

		c1 = C1_INDEX;
		c2 = C2_INDEX;
		if (c1 < pq_size && c2 < pq_size) {
			min_index = pq[c1]->global < pq[c2]->global ? c1: c2;
		} else if (c1 < pq_size) {
			min_index = c1;			// only first child
		} else {
			break;					// no children
		}
	}

	return del;
}

// The rule is to not overestimate the distance
// So min distance is steps to end node
float heuristic(int x, int y)
{
	return w - x + h - y - 2;
}


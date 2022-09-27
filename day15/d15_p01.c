#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define INPUT       "d15_input"
#define SHORT_INPUT "d15_short_input"

/* --- Day 15: Chiton --- */

typedef struct Node
{
	int dst;
	int to_here;
	struct Node *friends[4];
	struct Node *parent;
	int x;
	int y;
	int in_pq;
} Node;


// Heap Priority Queue
const int PQ_MAX = 2048;
typedef struct pq
{
	Node **data;
	int size;
} pq;

void pq_insert(pq *, Node *);
Node * pq_pop(pq *);
void pq_destroy(pq *);


int main(int argc, char** args) {
    int input_file = argc > 1;
	
	FILE *fp = fopen(input_file ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		perror("fopen");
        return 1;
	}

	// Read input size
	int w = 0, h = 0;
	char c;
	while (1) {
		if ((c = fgetc(fp)) == EOF) break;
		if (c == '\n') h++;
		if (h == 0) w++;
	}
		
	rewind(fp);


	// Node Graph
	Node **g = (Node **) malloc (h * sizeof(Node *));
	for (int i = 0; i < h; i++)
		g[i] = (Node *) malloc (w * sizeof(Node));

	// Reading & Filling Nodes data

	for (int i = 0; i < h; i++) {
		for (int j = 0; j < w; j++) {
			Node *n = &g[i][j];
			n->dst = fgetc(fp) - '0';

			n->to_here = INT_MAX;

			n->friends[0] = i > 0 		? &g[i - 1][j] : NULL;
			n->friends[1] = (j < w - 1) ? &g[i][j + 1] : NULL;
			n->friends[2] = (i < h - 1) ? &g[i + 1][j] : NULL;
			n->friends[3] = j > 0 		? &g[i][j - 1] : NULL;

			n->parent = NULL;
			n->x = j;
			n->y = i;
		}
		fgetc(fp);
	}


	g[0][0].to_here = 0;


	// Priority queue
	pq *q = (pq *) malloc(sizeof(q));
	q->data = (Node **) malloc(PQ_MAX * sizeof(Node *));

	pq_insert(q, &g[0][0]);

	// Dijkstra
	while (q->size) {
		Node *cur = pq_pop(q);
		cur->in_pq = 0;

		for (int i = 0; i < 4; i++) {
			Node *friend = cur->friends[i];
			if (!friend) continue;

			int to_friend = cur->to_here + friend->dst;
			if (to_friend < friend->to_here) {
				friend->parent = cur;
				friend->to_here = to_friend;
				if (!friend->in_pq) {
					pq_insert(q, friend);
					friend->in_pq = 1;
				}
			}
		}
	}

	printf("%i\n", g[h - 1][w - 1].to_here); 

	if (q->size != 0)
		printf("WARNING: More nodes in queue\n");
	

	// Cleanup
	pq_destroy(q);

	for (int i = 0; i < h; i++)
		free(g[i]);
	free(g);

	return 0;
}


// Heap Priority Queue Impl

#define C1_INDEX (index * 2 + 1)
#define C2_INDEX (index * 2 + 2)
#define PA_INDEX ((index - 1) / 2)

void pq_insert(pq * q, Node *node)
{
	int index = q->size++;
	if (q->size == PQ_MAX) {
		printf("ERROR: PQ Overload\n");
		return;
	}

	q->data[index] = node;
	if (index == 0) return;

	Node *tmp;
	int pindex = PA_INDEX;
	while (index > 0 && q->data[pindex]->to_here > q->data[index]->to_here) {
		tmp = q->data[index];
		q->data[index] = q->data[pindex];
		q->data[pindex] = tmp;

		index = pindex;
		pindex = PA_INDEX;
	}
}

Node * pq_pop(pq * q)
{
	if (q->size == 0) return NULL;
	int end = --q->size;

	Node *del = q->data[0];
	if (!end) return del; 	// empty

	q->data[0] = q->data[end];		// swap first with last
	q->data[end] = del;

	int index = 0;
	int c1 = C1_INDEX;
	int c2 = C2_INDEX;

	int min_index;
	Node *tmp;
	
	if (c1 < q->size && c2 < q->size) {
		min_index = q->data[c1]->to_here < q->data[c2]->to_here ? c1: c2;
	} else if (c1 < q->size) {
		min_index = c1;			// only first child
	} else {
		return del;				// no children
	}

	while (q->data[index]->to_here > q->data[min_index]->to_here) { // restore heap
		tmp = q->data[index];
		q->data[index] = q->data[min_index];
		q->data[min_index] = tmp;

		index = min_index;

		c1 = C1_INDEX;
		c2 = C2_INDEX;
		if (c1 < q->size && c2 < q->size) {
			min_index = q->data[c1]->to_here < q->data[c2]->to_here ? c1: c2;
		} else if (c1 < q->size) {
			min_index = c1;			// only first child
		} else {
			break;					// no children
		}
	}

	return del;
}

void pq_destroy(pq *q)
{
	free(q->data);
	free(q);
}


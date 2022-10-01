#include <bits/pthreadtypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "../../util/util.h"

#define INPUT  "d18_input"
#define SAMPLE "d18_sample"

/* --- Day 17: Trick Shot --- */

#define L 1
#define R 0

typedef struct number {
    struct number *l;
    struct number *r;
    struct number *p;

    int is_leaf;
    int val;
} number;


number *create_number_root()
{
    number *n = (number *) calloc(1, sizeof(number));
    return n;
}   

number *create_number(int val)
{
    number *n = (number *) calloc(1, sizeof(number));

    n->is_leaf = 1;
    n->val = val;

    return n;
}

void free_number(number *n)
{
    if (!n) return;

    free_number(n->l);
    free_number(n->r);
    free(n);
}

void print_number_helper(number *n)
{
    if (!n) return;
    if (n->is_leaf) {;
        dlog("%i", n->val);
        return;
    }

    dlog("[");
    print_number_helper(n->l);
    dlog(",");
    print_number_helper(n->r);
    dlog("]");
}

void print_number(number *n)
{
    print_number_helper(n);
    dlog("\n");
}


void explode(number *n);
void split(number *n);

int main(int argc, char **argv)
{
    set_debug_info(1);

	FILE *fp = fopen(argc > 1 ? INPUT : SAMPLE, "r");

	if (fp == NULL) {
		perror("fopen");
        return EXIT_FAILURE;
	}

    number *root = create_number_root();

    char c = fgetc(fp);
    char prev;
    number *cur = root;
    int side = L;
    while (cur && (c = fgetc(fp)) != EOF) {
        if (c == '[') {
            number *n = create_number_root();
            n->p = cur;
            if (side == L) {
                cur->l = n;
                cur = cur->l;
            } else {
                cur->r = n;
                cur = cur->r;
            }
            side=L;
        } else if (c == ',') {
            side = R;
        } else if (c == ']') {
            cur = cur->p;
        } else {
            number *leaf = create_number(c - '0');
            leaf->p = cur;
            if (side == L) cur->l = leaf;
            else           cur->r = leaf;
        }

        prev = c;
    }

    print_number(root);
    free_number(root);

    return EXIT_SUCCESS;
}


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

number ** number_from_file(FILE *fp, int *count)
{
    char c;
    int number_count = 0;
    while ((c = fgetc(fp)) != EOF)
        if (c == '\n') number_count++;
    rewind(fp);

    number **numbers = (number **) malloc(number_count * sizeof(number *));
    for (int i = 0; i < number_count; i++)
        numbers[i] = (number *) malloc(sizeof(number));

    fgetc(fp);
    number *root = create_number_root();
    int add_index = 0;

    char prev;
    number *cur = root;
    int side = L;
    while (1) {
        c = fgetc(fp);

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
        } else if (c == '\n' || c == EOF) {
            numbers[add_index++] = root;
            dlog("READ: ");
            print_number(root);

            root = create_number_root();
            cur = root;
            side = L;

            if (c == EOF || add_index == number_count) break;
            if (c == '\n') fgetc(fp);
        } else {
            number *leaf = create_number(c - '0');
            leaf->p = cur;
            if (side == L) cur->l = leaf;
            else           cur->r = leaf;
        }

        prev = c;
    }

    *count = number_count;
    return numbers;
}

void free_number(number *n)
{
    if (!n) return;

    free_number(n->l);
    free_number(n->r);
    free(n);
}


void find_explode_helper(number *n, number **found, int deep)
{
    if (!n || *found) return;

    find_explode_helper(n->l, found, deep + 1);

    dlog("[%c] %i at %i, found: %i, has parent: %i\n", n->is_leaf ? 'L' : 'N',  n->val, deep, *found != NULL, n->p != NULL);

    if (n->is_leaf && !(*found) && deep > 4) {
        *found = n->p;
        dlog("EPIK\n");
        print_number(*found);
        return;
    } 

    find_explode_helper(n->r, found, deep + 1);
}

number * find_explode(number *n) {
    number *found = NULL;
    find_explode_helper(n, &found, 0);
    return found;
}

void find_left(number *n, number **left)
{
    number *cur = n->p;
    if (cur->r == n) {
        *left = cur->l;
        return;
    }

    while (cur->p && cur == cur->p->l) {
        cur = cur->p;
    }

    if (!cur->p) {
        // root
        return;
    }

    // go to left branch and find rightmost number
    cur = cur->p->l;

    while (cur && !cur->is_leaf) {
        // go left
        cur = cur->r;
    }

    *left = cur;
}

void find_right(number *n, number **right)
{
    number *cur = n->p;
    if (cur->l == n) {
        *right = cur->r;
        return;
    }

    while (cur->p && cur == cur->p->r) {
        cur = cur->p;
    }

    if (!cur->p) {
        // root
        return;
    }

    // go to right branch and find leftmost number
    cur = cur->p->r;

    while (cur && !cur->is_leaf) {
        // go left
        cur = cur->l;
    }

    *right = cur;
}

void explode(number *n)
{
    int l_val = n->l->val;
    int r_val = n->r->val;

    number *left = NULL;
    find_left(n, &left);

    if (left) {
        left->val += l_val;
    }

    number *right = NULL;
    find_right(n, &right);

    if (right) {
        right->val += r_val;
    }

    free_number(n->l);
    free_number(n->r);

    n->l = NULL;
    n->r = NULL;

    n->is_leaf = 1;
    n->val = 0;
}

void find_and_split(number *n, int *found)
{
    if (!n || *found) return;

    find_and_split(n->l, found);

    if (n->val > 9 && !*found) {
        int l_val = n->val / 2;
        int r_val = n->val / 2 + n->val % 2;

        n->l = create_number(l_val);
        n->r = create_number(r_val);

        n->l->p = n;
        n->r->p = n;

        n->is_leaf = 0;
        n->val = 0;

        *found = 1;
        return;
    }

    find_and_split(n->r, found);
}

number * add(number *x, number *y)
{
    number *res = create_number_root();

    x->p = res;
    y->p = res;

    res->l = x;
    res->r = y;

    return res;
}

int main(int argc, char **argv)
{
    set_debug_info(1);

	FILE *fp = fopen(argc > 1 ? INPUT : SAMPLE, "r");

	if (fp == NULL) {
		perror("fopen");
        return EXIT_FAILURE;
	}

    int number_count;
    number **numbers = number_from_file(fp, &number_count);

    number *root = add(numbers[0], numbers[1]);
    /* number *root = numbers[0]; */

    dlog("\n");
    print_number(root);

    int found_split = 0;
    number *ex;

    while (1) {
        ex = find_explode(root);
        print_number(ex);
        if (ex) {
            explode(ex);
            dlog("EXPLODED\n");
            print_number(root);
            continue;
        }

        found_split = 0;
        find_and_split(root, &found_split);
        if (found_split) {
            dlog("SPLITED\n");
            print_number(root);
        } else {
            break;
        }
    }

    ex = find_explode(root);
    ex = root;
    print_number(ex);

    free_number(root);

    /* for (int i = 0; i < number_count; i++) */
    /*     free(numbers[i]); */
    /* free(numbers); */


    return EXIT_SUCCESS;
}

/*
[[[[0,7],4],[[7,8],[6,0]]],[8,1]]
[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]


*/

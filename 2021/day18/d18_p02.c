#include <bits/pthreadtypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "../../util/util.h"

#define INPUT  "d18_input"
#define SAMPLE "d18_sample"

/* --- Day 18: Snailfish --- */

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

    /* dlog("[%c] %i at %i, found: %i, has parent: %i\n", n->is_leaf ? 'L' : 'N',  n->val, deep, *found != NULL, n->p != NULL); */

    if (n->is_leaf && !(*found) && deep > 4)
    {
        *found = n->p;
        /* dlog("EPIK\n"); */
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
        cur = cur->l;
        while (!cur->is_leaf) {
            cur = cur->r;
        }

        *left = cur;
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
        cur = cur->r;
        while (!cur->is_leaf) {
            cur = cur->l;
        }

        *right = cur;
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
        print_number(left);
        left->val += l_val;
    }

    number *right = NULL;
    find_right(n, &right);

    if (right) {
        print_number(right);
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
        *found = n->val;
        int l_val = n->val / 2;
        int r_val = n->val / 2 + n->val % 2;

        n->l = create_number(l_val);
        n->r = create_number(r_val);

        n->l->p = n;
        n->r->p = n;

        n->is_leaf = 0;
        n->val = 0;

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

    dlog("ADD ");
    print_number(y);
    print_number(res); dlog("\n");

    int found_split = 0;
    number *ex;

    while (1) {
        ex = find_explode(res);
        /* print_number(ex); */
        if (ex) {
            explode(ex);
            dlog("  ^-- EXPLODE\n");
            print_number(res); dlog("\n");
            continue;
        }

        found_split = 0;
        find_and_split(res, &found_split);
        if (found_split) {
            dlog("SPLIT %i\n", found_split);
            print_number(res); dlog("\n");
        } else {
            break;
        }
    }

    return res;
}

int magnitude(number *n)
{
    if (!n) return 0;
    if (n->is_leaf) return n->val;
    return 3 * magnitude(n->l) + 2 * magnitude(n->r);
}

void number_copy_helper(number *n, number *copy, number *parent)
{
    if (!n) return;
    if (!copy) {
        dlog("WTF\n");
        return;
    }

    dlog("OK\n");

    *copy = *n;
    copy->p = parent;
    copy->p = parent;

    if (!n->is_leaf) {
        n->l = (number *) malloc(sizeof(number));
        n->r = (number *) malloc(sizeof(number));

        number_copy_helper(n->l, copy->l, copy);
        number_copy_helper(n->r, copy->r, copy);
    }
}

void number_copy(number *n, number *copy)
{
    copy = (number *) malloc(sizeof(number));
    number_copy_helper(n, copy, NULL);
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
    /* dlog("\n"); */

    number *res;
    int max = -1;

    number *cur;
    for (int i = 0; i < number_count; i++) {
        for (int j = 0; j < number_count; j++) {
            if (i == j) continue;

            cur = add(numbers[i], numbers[j]);
            int mag = magnitude(res);
            if (mag > max) {
                max = mag;
                res = cur;
            }
        }
    }

    /* print_number(res); */
    /* dlog("\n"); */

    /* printf("%i\n", magnitude(res)); */

    
    number *n = numbers[0];
    number *copy;
    number_copy(n, copy);
    print_number(copy);




    free_number(n); // all numbers are in res

    dlog("Freeing the copy\n");
    free_number(copy);
    dlog("OK\n");
    
    free(numbers);    // free the array of pointers

    return EXIT_SUCCESS;
}


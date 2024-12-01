#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d07_short_input"
#define INPUT	    "d07_input"

int fuel(int* crabs, int pos, int prev, int dir);
void print_crabs(int* crabs);

int crab_size = 0;


int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

    // sum, and crab count
    int n, sum = 0;
    while (1) {
		fscanf(fp, "%i", &n);
        sum += n;
        crab_size++;
		if (fgetc(fp) == EOF) break;
	}
    rewind(fp);

    int avg = sum/crab_size; // average value

    // load crabs
    int crabs[crab_size], i = 0;
    while (1) {
		fscanf(fp, "%i", &crabs[i++]);
		if (fgetc(fp) == EOF) break;
	}

    /*
        idea
        calc avg and fuel to get to avg
        if avg + 1 better, search that way while better
        if avg - 1 better, same
        smallest = best
    */

    int fuel_avg = 0, diff;
    for (int i = 0; i < crab_size; i++) {
        diff = abs(crabs[i] - avg);
        fuel_avg += diff * (diff + 1 ) / 2;
    }

    int left = fuel(crabs, avg - 1, fuel_avg, -1);
    int right = fuel(crabs, avg + 1, fuel_avg, +1);

    int ans = fuel_avg;
    if (left < ans) ans = left;
    if (right < ans) ans = right;

    printf("optimal fuel = %i\n", ans);
}


int fuel(int* crabs, int pos, int prev, int dir) {
    int ans = 0;

    for (int i = 0; i < crab_size; i++) {
        int dif = abs(crabs[i] - pos);
        ans += dif * (dif + 1) / 2;
    }

    if (prev < ans) return prev;

    return fuel(crabs, pos + dir, ans, dir);
}


void print_crabs(int* crabs) {
    for (int i = 0; i < crab_size; i++)
        printf("%i ", crabs[i]);
    printf("\n");
}

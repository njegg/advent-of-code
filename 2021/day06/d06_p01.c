#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d06_short_input"
#define INPUT	    "d06_input"

void print_timers(int* timers);
void table();
int sum(int* timers);

int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

    /*
        idea:
        have a 10 element array that keeps the number of
        fish for each timer level
        10nth is for the timer 8 to wait a day
    */  

    int timers[10] = {0};

    int n;
    while (1) {
        fscanf(fp, "%i", &n);
        timers[n]++;
        if (fgetc(fp) == EOF) break;
    }

    // table();
        
    int days = 80;

    for (int day = 1; day <= days; day++) {

        for (int i = 0; i < 10; i++) {
            if (timers[i] == 0) continue;

            if (i == 0) {
                timers[6+1] += timers[0];
                timers[8+1] += timers[0];
                timers[0] = 0;
            } else {
                timers[i-1] += timers[i];;
                timers[i] = 0;
            }
        }
        // printf("after %i days: ", day);
        // print_timers(timers);
    }

    int ans = sum(timers);
    printf("answer = %i\n", ans);

}

int sum(int* timers) {
    int sum = 0;
    for (int i = 0; i < 9; i++) {
        sum += timers[i];
    }
    return sum;
}

void print_timers(int* timers) {
    for (int i = 0; i < 9; i++) {
        printf("%3i ", timers[i]);
    }
    printf("\n");
}

void table(){
    printf("                0   1   2   3   4   5   6   7   8\n");
    printf("-------------------------------------------------\n");
}
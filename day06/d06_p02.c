#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d06_short_input"
#define INPUT	    "d06_input"

long sum(long* timers);

int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

    long timers[10] = {0};

    int n;
    while (1) {
        fscanf(fp, "%i", &n);
        timers[n]++;
        if (fgetc(fp) == EOF) break;
    }

    int days = 1929; // limit before long overflow for my input

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
    }

    long ans = sum(timers);
    printf("days = %i, fish = %ld\n", days, ans);
}

long sum(long* timers) {
    long sum = 0;
    for (int i = 0; i < 9; i++) {
        sum += timers[i];
    }
    return sum;
}

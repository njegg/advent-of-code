#include <stdio.h>
#include <stdlib.h>

#define SHORT_INPUT "d03_short_input"
#define INPUT	    "d03_input"


int main(void) {
	
	FILE *fp = fopen(INPUT, "r");

	if (fp == NULL) {
		printf("cant read file");
		return 1;
	}


	int bin_len = 12;

	char *sum = calloc(bin_len, sizeof(int)); // allocates and zeros
	int digit;

	while(!feof(fp)) {
		for (int i = 0; i < bin_len; i++) {
			fscanf(fp, "%1i", &digit);	
			sum[i] += digit == 1 ? 1 : -1;
		}
	}


	int gamma = 0, epsilon = 0;

	int base = 1;
	for (int i = bin_len - 1; i >= 0; i--) {
		gamma   += (sum[i] >= 0) * base;
		epsilon += (sum[i] <  0) * base;

		base *= 2;
	}
	
	printf("gamma = %i, epsilon = %i\n", gamma, epsilon);
	printf("power consumption = %i", (gamma * epsilon));

	fclose(fp);
	free(sum);
}

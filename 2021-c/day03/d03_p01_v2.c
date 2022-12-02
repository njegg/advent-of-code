#include <stdio.h>
#include <stdlib.h>

#define INPUT "d03_input"

int main(void) {

	FILE *fp = fopen(INPUT, "r");

	if (fp == NULL) {
		printf("cant read file");
		return 1;
	}

	int bin_len = 12;

	int *sum = calloc(bin_len, sizeof(int)); // allocates and zeros
	int digit;

	while(!feof(fp)) {
		for (int i = 0; i < bin_len; i++) {
			fscanf(fp, "%1i", &digit);	
			sum[i] += digit == 1 ? 1 : -1;
		}
	}


	int gamma = 0, epsilon = 0;

	for (int i = 0; i < bin_len; i++)
		gamma = (gamma << 1) ^ (sum[i] >= 0);
	epsilon = ~gamma << (32-bin_len) >> (32-bin_len);
	

	printf("gamma = %i, epsilon = %i\n", gamma, epsilon);
	printf("power consumption = %i", (gamma * epsilon));

	fclose(fp);
	free(sum);
}

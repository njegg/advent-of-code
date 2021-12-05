#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d03_short_input"
#define INPUT	    "d03_input"

// god forgive me for this disgusting peace of code

int main(void) {
    clock_t begin = clock();

    int input = 0;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file");
		return 1;
	}


    int bits = input ? 12 : 5;
    int rows = input ? 1000 : 12;
    int nums_size = bits * rows;

    int* nums = calloc(nums_size, sizeof(int));
    int i_nums = 0;


    while (!feof(fp)) {

        int n = 0;

        for (int i = 0; i < bits; i++) {
            int bit = fgetc(fp) == '1';

            n <<= 1;
            n += bit;            

            nums[i_nums] = n;

            i_nums++;
        }
            
        fgetc(fp); // read new line
    }

    
    int patt_oxy = 0, patt_co2 = 0;
    int oxy = 0, co2 = 0;
    int match_co2 = -1;

    for (int i = 0; i < bits; i++) {

        for (int j = 0; j < rows; j++) {
            int num = nums[j * bits + i];

            int original_to_sum = num % 2 == 0 ? -1 : 1;

            oxy += (num >> 1) == patt_oxy ? original_to_sum : 0;

            if (num >> 1 == patt_co2) {
                match_co2 = match_co2 == -1 ? num : -2;
                co2 += original_to_sum;
            }
        }

        patt_oxy <<= 1;
        patt_oxy += oxy >= 0;

        patt_co2 <<= 1;
        patt_co2 += match_co2 < 0 ? co2 < 0 : match_co2 % 2;

        co2 = 0;
        oxy = 0;
        match_co2 = -1;
    }
    
    printf("oxygen rating = %i, co2 rating = %i\n", patt_co2, patt_oxy);
    printf("resault = %i\n", patt_oxy * patt_co2);

    free(nums);
    fclose(fp);

clock_t end = clock();
double time_spent = (double)(end - begin) / 1000;

printf("\n%f\n", time_spent);
}

/*
        this line translated to human readable lmao

        oxy += (num >> 2) & patt ? (num % 2 ? -1 : 1) : 0;
                                          ^
                    odd if was 1 originaly, even if was 0
                    
        if (num / 2 == patt) {
            if (num % 2 == 0) {
                oxy -= 1;
            } else {
                oxy += 1;
            }
        } 
*/

/*
    notes: 

    every bit will represent a number that coresponds
    with the pattern so far

    0007     0  0  1  2
    1110 ->  1  3  7  15
    1010     1  2  5  11
    ...

    this is used to check if a row matches the pattern from previous
    something

    steps:
    0. use the pattern (initialy is *) and calc the oxy
    1. add the oxy >= 0 to pattern

*/

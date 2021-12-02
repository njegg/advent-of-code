#include <stdio.h>

#define WIN_SIZE 3


int sum_and_shift(int* window)
{
    int sum = 0;
    for (int i = 0; i < WIN_SIZE; i++)
    {
        sum += *(window + i);

        if (i < WIN_SIZE - 1) {
            *(window + i) = *(window + i + 1); // shift back
        }
    }

    return sum;
}


void main(void)
{
    FILE *fp = fopen("d01_input", "r");

	int incs = 0;	
    int sum_a = 0, sum_b = 0;
    int window[3];

    // fill window
    for (int i = 0; i < WIN_SIZE; i++)
    {
        if (fscanf(fp, "%i", window + i) < 0) return;
    }
    
    sum_a = sum_and_shift(window);

    // tries to add to last slot
    while (fscanf(fp, "%i", window + WIN_SIZE - 1) > 0)
    {
        sum_b = sum_and_shift(window);

        if (sum_b > sum_a)
            incs++;

        sum_a = sum_b;
    }

	printf("Increases = %i\n", incs);

    fclose(fp);
}
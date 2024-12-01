#include <stdio.h>

int main(void) {

	FILE *fp = fopen("d01_input", "r");

	int incs = 0;	

	int measure, tmp_measure;
	fscanf(fp, "%i", &measure); // first measure

	while (fscanf(fp, "%i", &tmp_measure) == 1)
	{
		if (tmp_measure > measure)
			incs++;

		measure = tmp_measure;
	}
	
    fclose(fp);

	printf("Increases = %i\n", incs);
	return incs;
}

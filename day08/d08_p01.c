#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d08_short_input"
#define INPUT	    "d08_input"


int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

    char word[8];
    int count = 0;
    while (1) {
        while (fgetc(fp) != '|') {}
        fgetc(fp); // ' '

        for (int i = 0; i < 4; i++) {
            fscanf(fp, "%s", word);
            int l = strlen(word);
            printf("%i ", l);
            if (l == 2 || l == 3 || l == 4 || l == 7)
                count++;
        }

        if (fgetc(fp) == EOF) break; // '\n'
    }

    printf("simples = %i\n", count);
    
}

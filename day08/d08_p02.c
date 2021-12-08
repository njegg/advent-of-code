#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d08_short_input"
#define INPUT	    "d08_input"

int decode(char (*patterns)[8], char (*numbers)[8]);
char diff(char* longer, int l, char* shorter);

int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}


    char patterns[10][8];
    char numbers[4][8];
    int count = 0;
    while (1) {
        for (int i = 0; i < 10; i++)
            fscanf(fp,"%s", patterns[i]);
        fgetc(fp);
        fgetc(fp);

        for (int i = 0; i < 4; i++)
            fscanf(fp, "%s", numbers[i]);

        int sum = decode(patterns, numbers);

        // if (fgetc(fp) == EOF) break; // '\n'
        break;
    }
    
}

int decode(char (*patterns)[8], char (*numbers)[8]) {
    char decoded[10][8];
    char symbols[7];

    // difference between 7 and 1 is *a*
    char seven[4];
    char one[3];
    int len;
    for (int i = 0; i < 10;i++) {
        len = strlen(patterns[i]);

        if      (len == 3) strcpy(seven, patterns[i]);
        else if (len == 2) strcpy(one, patterns[i]);
    }

    symbols[0] = diff(seven, 3, one); // a
    printf("%c\n", symbols[0]);

    from 6 segment numbers only 6 has exactly one common
    with one and thats *f*, the remaining unknown in 1 is *c* 

    for (int i = 0; i < 10; i++) {
        len = strlen(patterns[i]);
        if (len == 6 && num_commons(patterns[i], one) == 1)
            symbols[5] = diff(one, 2, patterns[i]);
    }

    symbols[5] = one[0]; // *c*
    
    return 0;
}

char diff(char* longer, int l, char* shorter) {
    for (int i = 0; i < l; i++) {
        if (!strchr(shorter, longer[i]))
            return longer[i];
    }
    return '!';
}

int num_commons(char* shorter, int s, char* longer) {
    int cnt = 0;
    for (int i = 0; i < s; i++) {
        if (strchr())
    }
    
}
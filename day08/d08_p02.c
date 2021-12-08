#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d08_short_input"
#define INPUT	    "d08_input"

int decode(char (*patterns)[8], char (*numbers)[8]);
char diff(char* longer, int l, char* shorter);
int num_commons(char* shorter, int s, char* longer);
void diff_str(char* longer, int l, char* shorter, char* diffs);

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
    char seven[4], one[3], four[5], eight[8];
    int len;
    for (int i = 0; i < 10;i++) {
        len = strlen(patterns[i]);

        if      (len == 3) strcpy(seven, patterns[i]);
        else if (len == 2) strcpy(one, patterns[i]);
        else if (len == 4) strcpy(four, patterns[i]);
        else if (len == 7) strcpy(eight, patterns[i]);
    }

    symbols[0] = diff(seven, 3, one); // a


    for (int i = 0; i < 10; i++) {
        len = strlen(patterns[i]);

        // from 6 segment numbers only 6 has exactly one common
        // with one and thats *f*, the remaining unknown in 1 is *c* 
        if (len == 6 && num_commons(one, 2, patterns[i]) == 1) {
            symbols[2] = diff(one, 2, patterns[i]); // c
            symbols[5] = symbols[2] == one[0] ? one[1] : one [0]; // f
            break;
        }

        // 3 is the only 5 segment number with two commons with 1    
        else if (len == 5 && num_commons(one, 2, patterns[i]) == 2) {
            char* three = patterns[i];

            // diff between 7 and 3 is d and g
            // 4 has only *d* so the other one is *g*
            char dg[8];
            diff_str(three, 5, seven, dg);

            symbols[3] =  strchr(four, dg[0]) ? dg[0] : dg[1];
            symbols[6] = !strchr(four, dg[0]) ? dg[0] : dg[1];

            // 8-3 = b, e; 4 has only *b*, other is *e*
            char be[8];
            diff_str(eight, 7, three, be);

            symbols[1] =  strchr(four, be[0]) ? be[0] : be[1];
            symbols[4] = !strchr(four, be[0]) ? be[0] : be[1];
        }
    }

    // all sybols decoded, decode the 4 numbers
    for (int i = 0; i < 4; i++) {

    }
    

    
    return 0;
}

void diff_str(char* longer, int l, char* shorter, char* diffs) {
    int dif_len = 0; 

    for (int  i = 0; i < l; i++) {
        if (!strchr(shorter, longer[i])) {
            diffs[dif_len++] = longer[i];
        }
    }
    diffs[dif_len] = '\0';
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
        if (strchr(longer, shorter[i]))
            cnt++;
    }
    return cnt;
}
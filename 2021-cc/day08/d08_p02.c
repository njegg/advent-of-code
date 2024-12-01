#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d08_short_input"
#define INPUT	    "d08_input"

int decode(char (*patterns)[8], char (*numbers)[8]);
char diff(char* longer, int l, char* shorter);
int num_commons(char* shorter, int s, char* longer);
void diff_str(char* longer, int l, char* shorter, char* diffs);

// update: i was dumb and blind so i overengeered a bit


// map every segment from 7-segment display to one bit
// so when it decodes 'f' to 'a', 'a' is a the first bit
// so a=1, b=2, c=4... now you can just add them together
// to get the number
void init_dispaly_values(int* display);
int display[128];
int values[7] = {1,2,4,8,16,32,64};

int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");
    int lines = input ? 200 : 10; // lazy to count the lines

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

    init_dispaly_values(display);

    char patterns[10][8];
    char numbers[4][8];
    int count = 0;
    int sum = 0;
    for (int i = 0; i < lines; i++) {
        for (int i = 0; i < 10; i++)
            fscanf(fp,"%s", patterns[i]);

        fgetc(fp);
        fgetc(fp);

        for (int i = 0; i < 4; i++)
            fscanf(fp, "%s", numbers[i]);

        sum += decode(patterns, numbers);
    }
    
    printf("result = %i\n", sum);
}

int decode(char (*patterns)[8], char (*numbers)[8]) {
    /*
        symbols is used like a 'map' to store which character
        a coded character represents when decoded
        but i a fucking confusing way because 
        i thought that iterating through array is lame

        'g' maps to 'a' and 'g' = 5 ('g'-'a')
        so symbols[5] = 'a' so symbols['g'-'a'] = 'a'
    */
    char symbols[8];

    // difference between 7 and 1 is *a*
    char seven[4], one[3], four[5], eight[8], three[6];
    int len;
    for (int i = 0; i < 10;i++) {
        len = strlen(patterns[i]);

        if      (len == 3) strcpy(seven, patterns[i]);
        else if (len == 2) strcpy(one, patterns[i]);
        else if (len == 4) strcpy(four, patterns[i]);
        else if (len == 7) strcpy(eight, patterns[i]);
    }

    char a = diff(seven, 3, one); // a
    symbols[a -'a'] = 'a';

    for (int i = 0; i < 10; i++) {
        len = strlen(patterns[i]);

        // from 6 segment numbers only 6 has exactly one common
        // with one and thats *f*, the remaining unknown in 1 is *c* 
        if (len == 6 && num_commons(one, 2, patterns[i]) == 1) {
            char c = diff(one, 2, patterns[i]); // c
            symbols[c-'a'] = 'c';

            char f = c == one[0] ? one[1] : one [0]; // f
            symbols[f-'a'] = 'f';
        }

        // 3 is the only 5 segment number with two commons with 1    
        else if (len == 5 && num_commons(one, 2, patterns[i]) == 2 ) {
            strcpy(three, patterns[i]);

            // diff between 7 and 3 is d and g
            // 4 has only *d* so the other one is *g*
            char dg[8];
            diff_str(three, 5, seven, dg);

            char d =  strchr(four, dg[0]) ? dg[0] : dg[1];
            symbols[d-'a'] = 'd'; 
            char g = !strchr(four, dg[0]) ? dg[0] : dg[1];
            symbols[g-'a'] = 'g';

            // 8-3 = b, e; 4 has only *b*, other is *e*
            char be[8];
            diff_str(eight, 7, three, be);

            char b =  strchr(four, be[0]) ? be[0] : be[1];
            symbols[b-'a'] = 'b';

            char e = !strchr(four, be[0]) ? be[0] : be[1];
            symbols[e-'a'] = 'e';
        }
    }

    // symbols
    // for (int i = 0; i < 7; i++)
    //     printf("%c\n", symbols[i]);
    // printf("\n");
    
    int result = 0;
    // decode the 4 numbers using symbols 'map'
    for (int i = 0; i < 4; i++) {
        int len = strlen(numbers[i]);

        // converts coded word to a number whose
        // bits represent what segments are on
        int to_display = 0; 
        for (int j = 0; j < len; j++) {
            char c = numbers[i][j];
            
            int bit = values[symbols[c-'a']-'a'];

            to_display += bit;
        }

        result *= 10;
        result += display[to_display]; 
    }
    
    return result;
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

void init_dispaly_values(int* display) {
    display[127-8]      = 0;
    display[4+32]       = 1;
    display[127-2-32]   = 2;
    display[127-2-16]   = 3;
    display[2+4+8+32]   = 4;
    display[127-4-16]   = 5;
    display[127-4]      = 6;
    display[1+4+32]     = 7;
    display[127]        = 8;
    display[127-16]     = 9;
}

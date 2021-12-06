#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SHORT_INPUT "d05_short_input"
#define INPUT	    "d05_input"

int map_size;

void process_input(int x1, int y1, int x2, int y2, int (*map)[map_size]) {
    int diagonal = abs(x1 - x2) == abs(y1 - y2); // they make a square

    if (!diagonal && (x1 != x2 && y1 != y2)) return;

    if (diagonal) {
        int x_inc = x1 < x2 ? 1 : -1;
        int y_inc = y1 < y2 ? 1 : -1;

        int diff = abs(x1 - x2);
        for (int i = 0; i <= diff; i++) {
            map[y1][x1]++;
            x1 += x_inc;
            y1 += y_inc;
        }

    } else {
        int fixed_x = x1 == x2;

        int start, end;
        if (fixed_x) {
            start = y1 < y2 ? y1 : y2;
            end   = y1 < y2 ? y2 : y1;
        } else {
            start = x1 < x2 ? x1 : x2;
            end   = x1 < x2 ? x2 : x1;
        }

        for (int i = start; i <= end; i++) {
            if (fixed_x) map[i][x1]++;
            else         map[y1][i]++;
        }
    }

}


void print_map (int (*map)[map_size]) {
    for (int i = 0; i < map_size; i++) {
        for (int j = 0; j < map_size; j++) {
            int n = map[i][j];
            if (n == 0) printf("%c", '.');
            else        printf("%i", n);
        }
        printf("\n");
    }
}


int check_danger(int (*map)[map_size]) {
    int dangers = 0;

    for (int i = 0; i < map_size; i++) {
        for (int j = 0; j < map_size; j++) {
            if (map[i][j] > 1) dangers++;
        }
    }

    return dangers;
}


int main(int ac) {
    int input = ac > 1;
	
	FILE *fp = fopen(input ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		printf("cant read file\n");
        return 1;
	}

    map_size = input ? 1000 : 10;

    int map[map_size][map_size];
    memset(map, 0, map_size*map_size*sizeof(int));

    char trash[4];
    int x1, x2, y1, y2;
    while (1) {
        fscanf(fp, "%i", &x1);
        fgetc(fp); // ','

        fscanf(fp, "%i", &y1);
        fscanf(fp, "%4c", trash); // ' -> '
        
        fscanf(fp, "%i", &x2);
        fgetc(fp); // ','
        fscanf(fp, "%i", &y2);

        process_input(x1, y1, x2, y2, map);

        char c = fgetc(fp);
        if (c == EOF) break; // else its \n
    }

    // print_map(map);
    int danger_zones = check_danger(map);

    printf("danger zones: %i\n", danger_zones);
}
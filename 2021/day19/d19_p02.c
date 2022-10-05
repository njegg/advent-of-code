#include <bits/pthreadtypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "../../util/util.h"

#define INPUT  "d19_input"
#define SAMPLE "d19_sample"

/* --- Day 19: Beacon Scanner --- */

typedef struct point
{
    int end;
    int x, y, z;
} point;

int main(int argc, char **argv)
{
    set_debug_info(1);

	FILE *fp = fopen(argc > 1 ? INPUT : SAMPLE, "r");

	if (fp == NULL) {
		perror("fopen");
        return EXIT_FAILURE;
	}

    int scanner_count = 0;

    int line_max = 32;
    char line[line_max];
    while (fgets(line, line_max, fp))
        if (line[1] == '-') scanner_count++; // line[1] - could be a negative number
    rewind(fp);

    if (!scanner_count) {
        dlog("ERROR: no scanners read\n");
        return EXIT_FAILURE;
    }
    
    const int max_beacons = 40;
    
    point **scanners = (point **) malloc(sizeof(point *) * scanner_count);
    for (int i = 0; i < scanner_count; i++)
        scanners[i] = (point *) calloc(max_beacons, sizeof(point));

    int scaner_index = 0;
    int beacon_index = 0;

    fgets(line, line_max, fp);
    while (fgets(line, line_max, fp)) {
        if (line[0] == '\n') continue;
        
        if (line[1] == '-') {
            scanners[scaner_index][beacon_index].end = 1;

            beacon_index = 0;
            scaner_index++;
            continue;
        }

        sscanf(line, "%i,%i,%i\n",
            &scanners[scaner_index][beacon_index].x,
            &scanners[scaner_index][beacon_index].y,
            &scanners[scaner_index][beacon_index].z
        );
        
        beacon_index++;
    }
    scanners[scaner_index][beacon_index].end = 1;

    for (int i = 0; i < scanner_count; i++) {
        for (int j = 0; j < max_beacons; j++) {
            point *cur = &scanners[i][j];

            if (cur->end) break;
            
            dlog("%i,%i,%i\n", cur->x, cur->y, cur->z);
        }
    }

    for (int i = 0; i < scanner_count; i++)
        free(scanners[i]);
    free(scanners);
    

    return EXIT_SUCCESS;
}


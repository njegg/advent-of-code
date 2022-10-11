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
        if (line[1] == '-') scanner_count++; // line[0] could be a negative number
    rewind(fp);

    if (!scanner_count) {
        dlog("ERROR: no scanners read\n");
        return EXIT_FAILURE;
    }
    
    const int max_beacons = 40; // if it reads more than this, error
    
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
        if (beacon_index == 39) {
            for (int i = 0; i < scanner_count; i++)
                free(scanners[i]);
            free(scanners);
            dlog("ERROR: Too manny samples!\n");
        }
    }
    scanners[scaner_index][beacon_index].end = 1;

    for (int i = 0; i < scanner_count; i++) {
        dlog("\nscanner %i\n", i);
        for (int j = 0; j < max_beacons; j++) {
            point *cur = &scanners[i][j];

            if (cur->end) break;
            
            dlog("%i,%i,%i\n", cur->x, cur->y, cur->z);
        }
        dlog("\n");
    }

    // Main alg
    // For every beacon in scanner A, fix the beacon to 0,0,0 and calcucate
    // a copy of beacons with coordinates relative to that point
    // With that data go through beacons of scanner B and fix every point of B
    // to 0,0,0 and check others if they exist in A (check rotations)
    
    dlog("----------------\n\n");

    point *sample = (point *) malloc(sizeof(point) * max_beacons);

    for (int i = 0; i < scanner_count; i++) {
        for (int j = 0; j < max_beacons; j++) {
            point *cur = &scanners[i][j];
            if (cur->end) break;
            
            sample[j].x = 0;
            sample[j].y = 0;
            sample[j].z = 0;
            
            // Fill sample with points relative to cur
            for (int k = 0; !scanners[i][k].end; k++) {
                if (k != j) {
                    sample[k].x = scanners[i][k].x - cur->x;
                    sample[k].y = scanners[i][k].y - cur->y;
                    sample[k].z = scanners[i][k].z - cur->z;
                }

                dlog("%i,%i,%i\n", sample[k].x, sample[k].y, sample[k].z);
            }
            dlog("\n");
        }
        dlog("\n");
    }

    for (int i = 0; i < scanner_count; i++)
        free(scanners[i]);
    free(scanners);
    

    return EXIT_SUCCESS;
}

/*
 *
 *
        --- scanner 0 ---
        0,2  0,0            0, -2
        4,1  4,-1
        3,3  0,1

        --- scanner 1 ---
        -1,-1 0,0           1,1
        -5,0  -4,0
        -2,1  -1,2
 *
 */

#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "../../util/util.h"

#define INPUT  "d19_input"
#define SAMPLE "d19_sample"

/* --- Day 19: Beacon Scanner --- */

const int max_beacons = 40; // if it reads more than this, error

typedef struct point
{
    int x, y, z;
    int end;
} point;

float dst(point *p, point *q);
void print_point(point *p);
int feq(float x, float y);
int check_scanner_pair(point *, point *);

int main(int argc, char **argv)
{
    set_debug_info(1);

	FILE *fp = fopen(argc > 1 ? INPUT : SAMPLE, "r");

	if (fp == NULL) {
		perror("fopen");
        return EXIT_FAILURE;
	}

    int scanner_count = 0;

    // Count scanners
    int line_max = 32;
    char line[line_max];
    while (fgets(line, line_max, fp))
        if (line[1] == '-') scanner_count++; // line[0] could be a negative number
    rewind(fp);

    if (!scanner_count) {
        dlog("ERROR: no scanners read\n");
        return EXIT_FAILURE;
    }
    

    point **scanners = (point **) malloc(sizeof(point *) * scanner_count);
    for (int i = 0; i < scanner_count; i++)
        scanners[i] = (point *) calloc(max_beacons, sizeof(point));

    int scaner_index = 0;
    int beacon_index = 0;

    // Read input
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

    // Print scanners
    for (int i = 0; i < scanner_count; i++) {
        dlog("\nscanner %i\n", i);
        for (int j = 0; j < max_beacons; j++) {
            point *cur = &scanners[i][j];

            if (cur->end) break;
            
            dlog("%i,%i,%i\n", cur->x, cur->y, cur->z);
        }
        dlog("\n");
    }
    dlog("----------------\n\n");

    // Pairing alg
    // scanner A, calc all distances relative to A[0]
    // do the same for each in B and check if distances are the same
    // if 12 not found, calculate again based on A[1]
    
    for (int i = 0; i < scanner_count - 1; i++) {
        for (int j = i + 1; j < scanner_count; j++) {
            int pair = check_scanner_pair(scanners[i], scanners[j]);
            if (pair) {
                dlog("Scanner %i and Scanner %i match!\n", i, j);
            }
        }
    }


    for (int i = 0; i < scanner_count; i++)
        free(scanners[i]);
    free(scanners);

    return EXIT_SUCCESS;
}

int check_scanner_pair(point *A, point *B)
{
    if (!A || !B) {
        dlog("ERROR: One scanner is NULL\n");
        return 0;
    }

    float *dists = (float *) malloc(sizeof(float) * max_beacons);

    for (int i = 0; !A[i].end; i++)
    {
        point *a_beacon = &A[i];

        int a;
        for (a = 0; !A[a].end; a++)
            dists[a] = dst(a_beacon, &A[a]); // Relative distances to A[i]
        dists[a] = -1.0;

        // Find 12 beacons that have distacnes that are in A
        // Starting with distances relative to B[0] and so on
        for (int j = 0; !B[j].end; j++)
        {
            int matches = 0;

            point *b_beacon = &B[j]; // B[i]

            // For each in B calc distances relative to B[i]
            for (int k = 0; !B[k].end; k++) {
                float distb = dst(b_beacon, &B[k]);

                // Check if the same distance is in A
                for (int d = 0; dists[d] >= 0; d++) {
                    if (feq(dists[d], distb) && distb != 0) {
                        matches++;
                        break;
                    }
                }
            }
            
            if (matches >= 11) {
                // Calculate the position of B based on A
                return 1;
            }
        }
    }
    
    return 0;
}

float dst(point *p, point *q)
{
    return sqrtf(
        (p->x - q->x) * (p->x - q->x) +
        (p->y - q->y) * (p->y - q->y) +
        (p->z - q->z) * (p->z - q->z)
    );
}

void print_point(point *p)
{
    dlog("%i,%i,%i\n", p->x, p->y, p->z);
}


int feq(float x, float y)
{
    return fabs(x - y) < 0.9; // max distance is 1 because integer points
}

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#define INPUT       "d17_input"
#define SHORT_INPUT "d17_short_input"

/* --- Day 17: Trick Shot --- */

short debug = 0;
void dlog(const char *format, ...);

int main(int argc, char **argv)
{
	FILE *fp = fopen(argc > 1 ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		perror("fopen");
        return EXIT_FAILURE;
	}

    int tx1, tx2, ty1, ty2;
    fscanf(fp, "target area: x=%i..%i, y=%i..%i", &tx1, &tx2, &ty1, &ty2);
    dlog("x=%i..%i, y=%i..%i\n", tx1, tx2, ty1, ty2);

    int res = 0;

    int init_vx = 0, init_vy = -200;
    int vx = init_vx, vy = init_vy;

    int tries = 69420;
    while (tries--) {
        int x = 0, y = 0;
        int vx = init_vx;
        int vy = init_vy;

        while (x <= tx2 && y >= ty1) {
            x += vx;
            y += vy;

            if (x >= tx1 && x <= tx2 && y >= ty1 && y <= ty2) {
                dlog("%i,%i\n", init_vx, init_vy);
                res++;
                break;
            }

            if      (vx > 0) vx--;
            else if (vx < 0) vx++;
            
            vy--;
        }

        init_vx++;

        if (init_vx > tx2) {
            init_vx = 1;
            init_vy++;
        }

        if (init_vy > 2000) {
            break;
        }
    }

    printf("%i\n", res);

    return EXIT_SUCCESS;
}


void dlog(const char *format, ...)
{
    if (!debug) return;
    va_list arg;

    va_start(arg, format);
    vfprintf(stdout, format, arg);
    va_end(arg);
}


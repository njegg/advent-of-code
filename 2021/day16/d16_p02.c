#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define INPUT       "d16_input"
#define SHORT_INPUT "d16_short_input"

/* --- Day 16: Packet Decoder --- */

#define BITS_MAX 6000
#define BITS_MAX_SHORT 600

int read_bits(int n);
int read_packet();

char *bits;
int bi = 0;

int bits_len;

// Toggle info output
short debug = 1;
void dlog(const char *format, ...)
{
    if (!debug) return;
    va_list arg;

    va_start(arg, format);
    vfprintf(stdout, format, arg);
    va_end(arg);
}

int main(int argc, char **argv)
{
	FILE *fp = fopen(argc > 1 ? INPUT : SHORT_INPUT, "r");

	if (fp == NULL) {
		perror("fopen");
        return EXIT_FAILURE;
	}

    const int bits_max = argc > 1 ? 5600 : 560;
    bits = (char *) calloc(bits_max, sizeof(char));

    int n;
    while (fscanf(fp, "%1x", &n) != -1) {
        for (int i = 0; i < 4; i++) {
            int bit = 1 << (3 - i);
            bits[i + bits_len] = (n & bit) == 0 ? 0 : 1;
        }

        bits_len += 4;
    }

    for (int i = 0; i < bits_len; i++) {
        dlog("%i", bits[i]);
    } dlog("\n");

    
    int res = read_packet();
    printf("%i\n", res);


    free(bits);
    return EXIT_SUCCESS;
}

int read_bits(int n)
{
    int val = 0;
    for (int i = 0; i < n; i++) {
        val <<= 1;
        val += bits[bi++];
    }
    return val;
}

int read_packet() {
    dlog("\n");

    int version = read_bits(3);
    dlog("V: %i\n", version);

    int ID = read_bits(3);
    dlog("ID: %i\n", ID);

    int packet_value = 0;

    if (ID == 4) {
        int has_more;
        do {
            has_more = bits[bi++];
            dlog("MORE: %i\n", has_more);

            int add = read_bits(4);
            dlog("%i ", add);
            packet_value <<= 4;
            packet_value += add;
        } while (has_more);

        /* if (bi % 4 != 0) bi += 4 - bi % 4; */
        dlog("\nVAL: %i\n", packet_value);
    } else {
        int len_type = bits[bi++];

        if (ID == 1) {
            if (len_type) { // n packets     (1)
                dlog("LT: 1\n");
                int packets = read_bits(11);
                dlog("L: %i\n", packets);

                for (int i = 0; i < packets; i++) {
                    packet_value *= read_packet();
                }
            } else {        // n packet bits (0)
                dlog("LT: 0\n");
                int packet_bits = read_bits(15);           
                dlog("L: %i\n", packet_bits);

                int start = bi;
                while (bi - start <= packet_bits && bi < bits_len) {
                    packet_value *= read_packet();
                    dlog("read %i bits so far\n", bi - start);
                }
            }
        }
    }

    return packet_value;
}


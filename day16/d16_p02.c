#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define INPUT       "d16_input"
#define SHORT_INPUT "d16_short_input"

/* --- Day 16: Packet Decoder --- */

#define BITS_MAX 6000
#define BITS_MAX_SHORT 600

int read_bits(int n);
char *bits;
int bits_len;
int bi = 0;     // index

long read_packet(int depth);
void do_operation(long *total, int ID, long read, short is_first);

// Toggle info output
short debug = 0;
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

    bits_len -=4;

    for (int i = 0; i < bits_len; i++) {
        dlog("%i", bits[i]);
    } dlog("\n");

    long res = read_packet(0);
    dlog("\n");
    printf("%li\n", res);

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

short END = 0;
long read_packet(int depth) {
    if (bi + 6 >= bits_len) {
        END = 1;
        return 0;
    }
    dlog("\n");

    char padding[depth + 1];
    for (int i = 0; i < depth; i++) {
        padding[i] = ' ';
    }
    padding[depth] = '\0';

    int version = read_bits(3);
    int ID = read_bits(3);
    dlog("%s", padding);
    dlog("ID: %i\n", ID);

    long packet_value = 0;

    if (ID == 4) {
        int has_more;
        do {
            has_more = bits[bi++];

            long add = read_bits(4);
            packet_value <<= 4;
            packet_value += add;
        } while (has_more);
    } else {
        int len_type = bits[bi++];

        short is_first = 1;

        if (len_type) {                            
            // n packets (1)
            int packets = read_bits(11);
            dlog("%s", padding);
            dlog("PN: %i\n", packets);

            for (int i = 0; i < packets; i++) {
                do_operation(&packet_value, ID, read_packet(depth+1), is_first);
                is_first = 0;
            }
        } else {
            // n packet bits (0)
            int packet_bits = read_bits(15);
            dlog("%s", padding);
            dlog("BN: %i\n", packet_bits);

            int start = bi;

            while (bi - start < packet_bits && !END) {
                long val = read_packet(depth+1);
                if (!END)
                    do_operation(&packet_value, ID, val, is_first);

                is_first = 0;
            }
        }

        dlog("\n");
    }

    dlog("%s", padding);
    dlog("VAL: %li\n", packet_value);
    return packet_value;
}

void do_operation(long *total, int ID, long read, short is_first)
{
    if (END) return;

    if (is_first) {
        *total = read;
    } else {
        if      (ID == 0)                  *total += read;
        else if (ID == 1)                  *total *= read;
        else if (ID == 2 && read < *total) *total = read; 
        else if (ID == 3 && read > *total) *total = read; 
        else if (ID == 5)                  *total = read <  *total ? 1 : 0;
        else if (ID == 6)                  *total = read >  *total ? 1 : 0;
        else if (ID == 7)                  *total = read == *total ? 1 : 0;
    }

}


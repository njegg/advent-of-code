#include <stdio.h>

#define SHORT_INPUT "d02_short_input"
#define INPUT	    "d02_input"


void main(void) {

	FILE *fp = fopen(INPUT, "r");

	if (fp == NULL) {
		printf("cant read file");
		return;
	}

}
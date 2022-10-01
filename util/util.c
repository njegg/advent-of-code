#include "util.h"

short DEBUG_INFO = 1;

void dlog(const char *format, ...)
{
    if (!DEBUG_INFO) return;
    va_list arg;

    va_start(arg, format);
    vfprintf(stdout, format, arg);
    va_end(arg);
}

void set_debug_info(short on)
{
    DEBUG_INFO = on;
}

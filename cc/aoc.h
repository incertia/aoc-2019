#ifndef __AOC_H__
#define __AOC_H__

#include <stdint.h>

int usage(int argc, char **argv);

#ifndef __MACH__
void cstart();
void cend();
uint64_t elapsed();
#endif

#endif

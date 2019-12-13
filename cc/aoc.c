#include "aoc.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct timespec start;
struct timespec end;

int
usage(int argc, char **argv)
{
  if (argc < 2) {
    printf("usage: %s [A/B]\n", argv[0]);
    exit(1);
  }

  if (*argv[1] == 'A') {
    return 0;
  } else if (*argv[1] == 'B') {
    return 1;
  } else {
    printf("usage: %s [A/B]\n", argv[0]);
    exit(1);
  }
}

#ifndef __MACH__
void cstart()
{
  clock_gettime(CLOCK_MONOTONIC_RAW, &start);
}

void cend()
{
  clock_gettime(CLOCK_MONOTONIC_RAW, &end);
}

uint64_t elapsed()
{
  time_t s = end.tv_sec - start.tv_sec;
  time_t n = end.tv_nsec - start.tv_nsec;
  return s * 1000 + n / 1000000;
}
#endif

#include <stdio.h>

#include "aoc.h"
#include "solver.h"

int
main(int argc, char **argv)
{
  int z;
  z = usage(argc, argv);
#ifndef __MACH__
  cstart();
#endif
  solve(z);
#ifndef __MACH__
  cend();
  printf("solution took %lums\n", elapsed());
#endif
  return 0;
}

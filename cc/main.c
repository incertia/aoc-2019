#include <stdio.h>

#include "aoc.h"
#include "solver.h"

int
main(int argc, char **argv)
{
  int z;
  z = usage(argc, argv);
  cstart();
  solve(z);
  cend();
  printf("solution took %lums\n", elapsed());
  return 0;
}

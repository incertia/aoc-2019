#define MAX(a, b) ((a) < (b) ? (b) : (a))

#include <stdio.h>

#include "solver.h"

void
solve(int z)
{
  int w;
  int s = 0;
  while (scanf("%d", &w) != EOF) {
    w = w / 3 - 2;
    if (z) {
      while (w > 0) {
        s += MAX(w, 0);
        w = w / 3 - 2;
      }
    } else {
      s += MAX(w, 0);
    }
  }
  printf("%d\n", s);
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "solver.h"

#define N 10000

typedef struct {
  char n[16];
  char p[16];
} tree_t;
tree_t planets[N];

int cmp_tree(const void *a, const void *b)
{
  return strcmp(((tree_t *)a)->n, ((tree_t *)b)->n);
}

// this is basically as fast as the Haskell version
size_t depth(tree_t t, char *r, size_t sz)
{
  if (strcmp(t.p, r) == 0)
  {
    return 1;
  }
  else
  {
    size_t lo = 0;
    size_t hi = sz;
    while (lo != hi)
    {
      size_t m = (lo + hi) / 2;
      int cmp = strcmp(planets[m].n, t.p);

      if (cmp == 0)
      {
        return 1 + depth(planets[m], r, sz);
      }
      else if (cmp < 0)
      {
        lo = m + 1;
      }
      else
      {
        hi = m;
      }
    }
    printf("could not find %s\n", t.p);
    return 0;
  }
}

void
solve(int z)
{
  char l[16];
  size_t sz = 0;

  if (!z)
  {
    size_t s = 0;
    while (scanf("%sl", l) != EOF)
    {
      l[3] = 0;
      strcpy(planets[sz].n, l + 4);
      strcpy(planets[sz].p, l);
      sz++;
    }
    qsort(planets, sz, sizeof(tree_t), cmp_tree);
    for (size_t i = 0; i < sz; i++)
    {
      s += depth(planets[i], "COM", sz);
    }
    printf("%lu\n", s);
  }
  else
  {
  }
}

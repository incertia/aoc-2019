#include <stdio.h>
#include <stdlib.h>

#include "solver.h"
#include "intcode.h"

void
solve(int z)
{
  char *l = NULL;
  char *x;
  size_t sz = 0;
  size_t c = 0;
  int64_t *p;
  intcode_t *m;

  getline(&l, &sz, stdin);
  for (size_t i = 0; i < sz; i++)
  {
    if (l[i] == ',' || l[i] == '\n') c++;
  }
  p = malloc((c + 2) * sizeof(int64_t));
  x = l; c = 0;
  for (size_t i = 0; i < sz; i++)
  {
    if (l[i] == ',' || l[i] == '\n')
    {
      l[i] = 0;
      sscanf(x, "%ld", p + c++);
      x = l + i + 1;
    }
  }
  free(l);

  m = intcode_new(p, c);

  if (!z)
  {
    intcode_mem_set(m, 1, 12);
    intcode_mem_set(m, 2, 2);
    intcode_run(m, NULL);
    printf("%ld\n", intcode_mem_get(m, 0));
  }
  else
  {
    int found = 0;
    for (int a = 0; a < 100 && !found; a++)
    {
      for (int b = 0; b < 100 && !found; b++)
      {
        // without using the copy constructor, rebuilting the avl from p tree
        // takes our runtime up to 2s
        intcode_t *mm = intcode_cpy(m);
        intcode_mem_set(mm, 1, a);
        intcode_mem_set(mm, 2, b);
        intcode_run(mm, NULL);
        if (intcode_mem_get(mm, 0) == 19690720)
        {
          found = 1;
          printf("%d\n", 100 * a + b);
        }
        intcode_free(mm);
      }
    }
  }

  intcode_free(m);
  free(p);
}

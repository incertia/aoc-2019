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
  vec_t *in, *out;

  getline(&l, &sz, stdin);
  for (size_t i = 0; i < sz; i++)
  {
    if (l[i] == ',' || l[i] == '\n') c++;
  }
  p = malloc(c * sizeof(int64_t));
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
  in = intcode_in(m);
  out = intcode_out(m);
  vec_append(in, 4 * z + 1);
  intcode_run(m, 0);
  printf("%ld\n", vec_get(out, vec_size(out) - 1));

  intcode_free(m);
  free(p);
}

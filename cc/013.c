#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "intcode.h"

#define AIR 0
#define WALL 1
#define BLOCK 2
#define PADDLE 3
#define BALL 4

int64_t
key(int64_t x, int64_t y)
{
  return (int64_t)(((uint64_t)((uint32_t)x) << 32) | ((uint64_t)((uint32_t)y) << 0));
}

void
solve(int _)
{
  char *l = NULL, *x;
  size_t sz = 0;
  int64_t *p;
  intcode_t *m;
  int res;
  size_t c;
  int64_t score = 0;
  int64_t bx, px;
  bx = px = 0;

  int64_t minX, minY, maxX, maxY;

  bt_t *t;
  vec_t *in, *out;

  minX = minY = maxX = maxY = 0;

  getline(&l, &sz, stdin);
  c = 0;
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
      sscanf(x, "%" SCNd64, p + c++);
      x = l + i + 1;
    }
  }
  free(l);

  m = intcode_new(p, c);
  in = intcode_in(m);
  out = intcode_out(m);
  t = bt_new();

  free(p);


  if (!_)
  {
    c = 0;
    intcode_run(m, 0);
    for (size_t i = 0; i < vec_size(out); i += 3)
    {
      if (vec_get(out, i + 2) == BLOCK) c++;
    }
    printf("%lu\n", c);
  }
  else
  {
    intcode_mem_set(m, 0, 2);
    c = 0;
    while ((res = intcode_run(m, 1)) != HALTED)
    {
      if (res == GAVE_OUTPUT)
      {
        // calculate from output
        int64_t a, b, tt;

        c++;
        if (c % 3 != 0) continue;

        c = 0;
        a = vec_get(out, 0);
        b = vec_get(out, 1);
        tt = vec_get(out, 2);
        bt_insert(t, key(a, b), tt);

        if (a == -1 && b == 0)
        {
          score = tt;
        }
        else
        {
          if (a < minX) minX = a;
          if (a > maxX) maxX = a;
          if (b < minY) minY = b;
          if (b > maxY) maxY = b;

          if (tt == PADDLE)
          {
            px = a;
          }
          else if (tt == BALL)
          {
            bx = a;
          }
        }
        vec_reset(out);
      }
      else if (res == NEEDS_INPUT)
      {
        // calculate and input
        int dir = 0;
        if (bx < px)
        {
          dir = -1;
        }
        if (bx > px)
        {
          dir = 1;
        }
        vec_append(in, dir);
      }
      else
      {
        printf("bad machine state\n");
        exit(5);
      }
    }

    printf("%" PRId64 "\n", score);
  }

  bt_free(t);
  intcode_free(m);
}

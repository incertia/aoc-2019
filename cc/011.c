#include <stdio.h>
#include <stdlib.h>

#include "solver.h"
#include "intcode.h"

int64_t
key(int64_t x, int64_t y)
{
  return (int64_t)(((uint64_t)((uint32_t)x) << 32) | ((uint64_t)((uint32_t)y) << 0));
}

int64_t
kx(int64_t k)
{
  return (int64_t)((int32_t)((uint64_t)k >> 32));
}

int64_t
ky(int64_t k)
{
  return (int64_t)((int32_t)((uint64_t)k >> 0));
}

void
solve(int z)
{
  char *l = NULL;
  char *x;
  size_t sz = 0;
  size_t c = 0;
  int64_t *p;
  intcode_t *m;
  bt_t *colors;
  vec_t *in, *out;
  size_t outpos = 0;
  int res;
  int64_t px = 0, py = 0; // origin
  int dx = 0, dy = 1; // up

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
      sscanf(x, "%lld", p + c++);
      x = l + i + 1;
    }
  }
  free(l);

  colors = bt_new();
  m = intcode_new(p, c);
  in = intcode_in(m);
  out = intcode_out(m);
  bt_insert(colors, key(px, py), z);
  vec_append(in, bt_get(colors, key(px, py)));
  c = 0;
  while ((res = intcode_run(m, 1)) != HALTED)
  {
    if (res == GAVE_OUTPUT)
    {
      // calculate from output
    }
    else if (res == NEEDS_INPUT)
    {
      // calculate and input
      int64_t c = vec_get(out, outpos++);
      int64_t d = vec_get(out, outpos++);
      int tmp;
      bt_insert(colors, key(px, py), c);
      if (d == 0)
      {
        // left
        tmp = dy;
        dy = dx;
        dx = -tmp;
      }
      else if (d == 1)
      {
        // right
        tmp = dx;
        dx = dy;
        dy = -tmp;
      }
      else
      {
        printf("bad dir: %lld\n", d);
        exit(6);
      }
      // move forward
      px += dx;
      py += dy;
      vec_append(in, bt_get(colors, key(px, py)));
    }
    else
    {
      printf("bad machine state\n");
      exit(5);
    }
  }

  if (!z)
  {
    printf("%lu\n", bt_size(colors));
  }
  else
  {
    vec_t *keys = bt_keys(colors);
    int64_t minx = INT64_MAX, maxx = INT64_MIN, miny = INT64_MAX, maxy = INT64_MIN;
    for (c = 0; c < vec_size(keys); c++)
    {
      int64_t xk = kx(vec_get(keys, c)), yk = ky(vec_get(keys, c));
      if (xk < minx) minx = xk;
      if (xk > maxx) maxx = xk;
      if (yk < miny) miny = yk;
      if (yk > maxy) maxy = yk;
    }
    vec_free(keys);
    for (int64_t yy = maxy; yy >= miny; yy--)
    {
      for (int64_t xx = minx; xx <= maxx; xx++)
      {
        int c = bt_get(colors, key(xx, yy));
        if (c == 1)
        {
          printf("#");
        }
        else
        {
          printf(" ");
        }
      }
      printf("\n");
    }
  }

  bt_free(colors);
  intcode_free(m);
  free(p);
}

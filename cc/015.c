#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "intcode.h"

#define WALL 0
#define MOVED 1
#define OXYGEN 2

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

void dfs(bt_t *visited, bt_t *map, int64_t x, int64_t y, intcode_t *m, vec_t *in, vec_t *out, int64_t *tx, int64_t *ty)
{
  if (bt_get(visited, key(x, y)) == 1)
  {
    return;
  }
  bt_insert(visited, key(x, y), 1);

  for (int i = 1; i <= 4; i++)
  {
    int dx, dy;
    int64_t type;
    switch (i)
    {
    case 1:
      dx = 0;
      dy = 1;
      break;
    case 2:
      dx = 0;
      dy = -1;
      break;
    case 3:
      dx = -1;
      dy = 0;
      break;
    case 4:
      dx = 1;
      dy = 0;
      break;
    }
    vec_append(in, i);
    intcode_run(m, 1);
    type = vec_get(out, vec_size(out) - 1);
    if (type != 0)
    {
      bt_insert(map, key(x + dx, y + dy), type);
      dfs(visited, map, x + dx, y + dy, m, in, out, tx, ty);
      // go backward
      vec_append(in, (i % 2 + 1) + 2 * ((i - 1) / 2));
      intcode_run(m, 1);
      if (type == OXYGEN)
      {
        *tx = x + dx;
        *ty = y + dy;
      }
    }
  }
}

int64_t find(bt_t *visited, bt_t *map, int64_t x, int64_t y, int64_t tx, int64_t ty)
{
  int64_t min = INT64_MAX;
  int found = 0;
  if (bt_get(visited, key(x, y)) == 1)
  {
    return -1;
  }
  bt_insert(visited, key(x, y), 1);

  if (x == tx && y == ty)
  {
    return 0;
  }

  for (int i = 1; i <= 4; i++)
  {
    int dx, dy;
    switch (i)
    {
    case 1:
      dx = 0;
      dy = 1;
      break;
    case 2:
      dx = 0;
      dy = -1;
      break;
    case 3:
      dx = -1;
      dy = 0;
      break;
    case 4:
      dx = 1;
      dy = 0;
      break;
    }
    if (bt_get(map, key(x + dx, y + dy)))
    {
      int64_t res = find(visited, map, x + dx, y + dy, tx, ty);
      if (res != -1 && res < min) found = 1, min = res;
    }
  }
  if (found) return 1 + min;
  else return -1;
}

int64_t depth(bt_t *visited, bt_t *map, int x, int y)
{
  int64_t max = INT64_MIN;
  int found = 0;

  if (bt_get(visited, key(x, y)) == 1)
  {
    return -1;
  }
  bt_insert(visited, key(x, y), 1);

  for (int i = 1; i <= 4; i++)
  {
    int dx, dy;
    switch (i)
    {
    case 1:
      dx = 0;
      dy = 1;
      break;
    case 2:
      dx = 0;
      dy = -1;
      break;
    case 3:
      dx = -1;
      dy = 0;
      break;
    case 4:
      dx = 1;
      dy = 0;
      break;
    }
    if (bt_get(map, key(x + dx, y + dy)))
    {
      int64_t res = depth(visited, map, x + dx, y + dy);
      if (res != -1 && res > max) found = 1, max = res;
    }
  }

  if (!found) return 0;
  else return 1 + max;
}

void
solve(int _)
{
  char *l = NULL, *x;
  size_t sz = 0;
  int64_t *p;
  intcode_t *m;
  size_t c;
  int64_t tx, ty;

  bt_t *visited, *map;
  vec_t *in, *out;

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

  free(p);

  visited = bt_new();
  map = bt_new();
  dfs(visited, map, 0, 0, m, in, out, &tx, &ty);
  bt_insert(map, key(0, 0), 1);
  bt_free(visited);
  visited = bt_new();

  if (!_)
  {
    printf("%" PRId64 "\n", find(visited, map, 0, 0, tx, ty));
  }
  else
  {
    printf("%" PRIu64 "\n", depth(visited, map, tx, ty));
  }

  bt_free(map);
  bt_free(visited);
  intcode_free(m);
}

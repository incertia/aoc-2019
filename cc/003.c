#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) < (b) ? (b) : (a))
#define ABS(x) ((x) >= 0 ? (x) : (-(x)))

#include <stdio.h>
#include <stdlib.h>

#include "solver.h"

#define H 0
#define V 1

typedef struct {
  int x;
  int y;
  int dir;
  int len;
  int dist;
} seg_t;

seg_t *mksegs(char *s, size_t len, size_t *sz);

void
solve(int z)
{
  char *a = NULL, *b = NULL;
  size_t sa = 0, sb = 0;
  size_t na, nb;
  seg_t *va, *vb;
  size_t md = -1;

  getline(&a, &sa, stdin);
  getline(&b, &sb, stdin);

  va = mksegs(a, sa, &na);
  vb = mksegs(b, sb, &nb);

  for (size_t i = 0; i < na; i++)
  {
    for (size_t j = 0; j < nb; j++)
    {
      seg_t *h = NULL, *v = NULL;
      if (va[i].dir == H && vb[j].dir == V)
      {
        h = va + i; v = vb + j;
      }
      else if (va[i].dir == V && vb[j].dir == H)
      {
        v = va + i; h = vb + j;
      }
      if (h != NULL && v != NULL)
      {
        int l = MIN(h->x, h->x + h->len);
        int r = MAX(h->x, h->x + h->len);
        int d = MIN(v->y, v->y + v->len);
        int u = MAX(v->y, v->y + v->len);
        if (h->y >= d && h->y <= u && v->x >= l && v->x <= r)
        {
          size_t d;
          if (z)
          {
            d = h->dist + v->dist + ABS(v->x - h->x) + ABS(h->y - v->y);
          }
          else
          {
            d = ABS(v->x) + ABS(h->y);
          }
          if (d < md)
          {
            md = d;
          }
        }
      }
    }
  }

  free(vb);
  free(va);
  free(b);
  free(a);

  printf("%lu\n", md);
}

seg_t *
mksegs(char *s, size_t len, size_t *sz)
{
  seg_t *r;
  char *og = s, *c = s;
  size_t n = 0;
  size_t j = 0;
  int x = 0, y = 0, dist = 0;

  for (size_t i = 0; i < len; i++)
  {
    if (s[i] == ',' || s[i] == '\n')
    {
      n++;
    }
  }

  r = malloc(n * sizeof(seg_t));

  for (size_t i = 0; i < len; i++)
  {
    if (og[i] == ',' || og[i] == '\n')
    {
      int len;
      og[i] = 0;
      r[j].x = x;
      r[j].y = y;
      sscanf(c + 1, "%d", &len);
      r[j].len = len;
      r[j].dist = dist;
      dist += len;
      switch (*c)
      {
      case 'U':
        r[j].dir = V;
        y += len;
        break;
      case 'D':
        r[j].dir = V;
        r[j].len *= -1;
        y -= len;
        break;
      case 'L':
        r[j].dir = H;
        r[j].len *= -1;
        x -= len;
        break;
      case 'R':
        r[j].dir = H;
        x += len;
        break;
      default:
        printf("bad dir: %c\n", *c);
        exit(2);
        break;
      }
      j++;
      c = og + i + 1;
    }
  }

  *sz = j;
  return r;
}

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "intcode.h"

#define N 50

typedef struct slist_t
{
  int64_t d;
  struct slist_t *n;
} slist_t;

typedef struct queue_t
{
  slist_t *start;
  slist_t *end;
} queue_t;

slist_t *
slist_new()
{
  slist_t *sl = malloc(sizeof(slist_t));
  sl->n = NULL;
  return sl;
}

void
slist_free(slist_t *sl, int r)
{
  if (!sl) return;
  if (r) slist_free(sl->n, r);
  free(sl);
}

queue_t *
queue_new()
{
  queue_t *q = malloc(sizeof(queue_t));
  q->start = q->end = slist_new();
  return q;
}

int
queue_empty(queue_t *q)
{
  return q->start->n == NULL;
}

int64_t
queue_pop(queue_t *q)
{
  if (!q->start->n)
  {
    return -1;
  }
  else
  {
    slist_t *tmp = q->start;
    int64_t r = tmp->d;
    q->start = q->start->n;
    slist_free(tmp, 0);
    return r;
  }
}

void
queue_push(queue_t *q, int64_t d)
{
  q->end->n = slist_new();
  q->end->d = d;
  q->end = q->end->n;
}

void
queue_free(queue_t *q)
{
  if (!q) return;
  slist_free(q->start, 1);
  free(q);
}

void
solve(int _)
{
  char *l = NULL, *x;
  size_t sz = 0;
  int64_t *p;
  intcode_t *master;
  intcode_t *m[N];
  vec_t *in[N], *out[N];
  queue_t *bus[N];
  int halted[N];
  size_t c;
  size_t idle = 0;
  int nat_activated = 0;
  int64_t natx = 0, naty = 0;
  int64_t lastx = 0, lasty = 0;

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

  master = intcode_new(p, c);
  free(p);

  for (size_t i = 0; i < N; i++)
  {
    m[i] = intcode_cpy(master);
    in[i] = intcode_in(m[i]);
    out[i] = intcode_out(m[i]);
    bus[i] = queue_new();
    halted[i] = 0;
    vec_append(in[i], i);
  }

  c = 0;
  while (1)
  {
    int r;
    if (!halted[c])
    {
      r = intcode_run(m[c], 1);
      if (r == GAVE_OUTPUT)
      {
        if (vec_size(out[c]) == 3) {
          int64_t dst = vec_get(out[c], 0);
          int64_t x, y;
          x = vec_get(out[c], 1);
          y = vec_get(out[c], 2);
          if (dst == 255)
          {
            if (!_)
            {
              printf("%" PRId64 "\n", y);
              break;
            }
            natx = x;
            naty = y;
            nat_activated = 1;
          }
          else
          {
            queue_push(bus[dst], x);
            queue_push(bus[dst], y);
          }
          vec_reset(out[c]);
        }
      }
      else if (r == NEEDS_INPUT)
      {
        if (queue_empty(bus[c]))
        {
          vec_append(in[c], -1);
          idle++;
        }
        else
        {
          int64_t x, y;
          x = queue_pop(bus[c]);
          y = queue_pop(bus[c]);
          // printf("%zu <- %ld, %ld\n", c, x, y);
          vec_append(in[c], x);
          vec_append(in[c], y);
        }
      }
      else if (r == HALTED)
      {
        halted[c] = 1;
      }
      else
      {
        printf("wat\n");
        exit(5);
      }
    }
    c += 1;
    c %= N;
    if (c == 0)
    {
      if (_ && idle == N && nat_activated)
      {
        if (naty == lasty)
        {
          printf("%" PRId64 "\n", naty);
          break;
        }
        queue_push(bus[0], natx);
        queue_push(bus[0], naty);
        lastx = natx;
        lasty = naty;
      }
      idle = 0;
    }
  }

  for (size_t i = 0; i < N; i++)
  {
    queue_free(bus[i]);
    intcode_free(m[i]);
  }
  intcode_free(master);
}

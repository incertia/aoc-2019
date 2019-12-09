#include "intcode.h"

#include <stdio.h>
#include <stdlib.h>

struct vec_t {
  size_t sz;
  size_t cap;
  int64_t *v;
};

vec_t *
vec_new()
{
  vec_t *v = malloc(sizeof(vec_t));
  v->cap = 1024;
  v->v = malloc(v->cap * sizeof(int64_t));
  v->sz = 0;
  return v;
}

size_t
vec_size(vec_t *v)
{
  return v->sz;
}

int64_t
vec_get(vec_t *v, size_t n)
{
  return v->v[n];
}

void
vec_set(vec_t *v, size_t n, int64_t d)
{
  v->v[n] = d;
}

void
vec_append(vec_t *v, int64_t d)
{
  if (v->sz == v->cap)
  {
    v->cap *= 2;
    v->v = realloc(v, v->cap);
  }
  v->v[v->sz++] = d;
}

void
vec_free(vec_t *v)
{
  if (!v) return;
  free(v->v);
  free(v);
}

typedef struct node_t
{
  int64_t k, v;
  struct node_t *l, *r;
} node_t;

node_t *
node_new()
{
  node_t *node = malloc(sizeof(node_t));
  node->l = node->r = NULL;
  return node;
}

void
node_insert(node_t **pn, int64_t k, int64_t v)
{
  node_t *n;
  if (*pn)
  {
    n = *pn;
    if (k == n->k)
    {
      n->v = v;
    }
    else if (k < n->k)
    {
      node_insert(&n->l, k, v);
    }
    else
    {
      node_insert(&n->r, k, v);
    }
  }
  else
  {
    n = *pn = node_new();
    n->k = k;
    n->v = v;
  }
}

int64_t node_get(node_t *node, int64_t k)
{
  if (!node)
  {
    return 0;
  }
  else if (k == node->k)
  {
    return node->v;
  }
  else if (k < node->k) {
    return node_get(node->l, k);
  }
  else
  {
    return node_get(node->r, k);
  }
}

void
node_free(node_t *node)
{
  if (!node) return;
  node_free(node->l);
  node_free(node->r);
  free(node);
}

typedef struct
{
  node_t *r;
} bt_t;

bt_t *
bt_new()
{
  bt_t *bt = malloc(sizeof(bt_t));
  bt->r = NULL;
  return bt;
}

void
bt_insert(bt_t *bt, int64_t k, int64_t v)
{
  node_insert(&bt->r, k, v);
}

int64_t bt_get(bt_t *bt, int64_t k)
{
  return node_get(bt->r, k);
}

void
bt_free(bt_t *bt)
{
  node_free(bt->r);
  free(bt);
}

vec_t *
intcode_run(int64_t *prog, size_t sz, int64_t *in, size_t insz)
{
  vec_t *out = vec_new();
  bt_t *m = bt_new();
  int64_t ip = 0;
  int64_t base = 0;
  size_t inpos = 0;

  for (size_t i = 0; i < sz; i++)
  {
    bt_insert(m, i, prog[i]);
  }

  while (1)
  {
    int64_t pa, pb, pc, ma, mb, mc, a, b;
    int64_t opc, op, modes;
    int halt = 0;

    opc = bt_get(m, ip);
    op = opc % 100;
    modes = opc / 100;
    halt = 0;
    pa = bt_get(m, ip + 1);
    pb = bt_get(m, ip + 2);
    pc = bt_get(m, ip + 3);
    ma = modes % 10; modes /= 10;
    mb = modes % 10; modes /= 10;
    mc = modes % 10; modes /= 10;
    if (ma == 2) pa += base;
    if (mb == 2) pb += base;
    if (mc == 2) pc += base;
    a = (ma == 1 ? pa : bt_get(m, pa));
    b = (mb == 1 ? pb : bt_get(m, pb));

    // printf("ip: %ld, opc: %ld, pa: %ld, pb: %ld, ma: %ld, mb: %ld, a: %ld, b: %ld\n", ip, opc, pa, pb, ma, mb, a, b);

    switch (op)
    {
    case 1:
      bt_insert(m, pc, a + b);
      ip += 4;
      break;
    case 2:
      bt_insert(m, pc, a * b);
      ip += 4;
      break;
    case 3:
      bt_insert(m, pa, in[inpos++]);
      ip += 2;
      break;
    case 4:
      vec_append(out, a);
      ip += 2;
      break;
    case 5:
      ip += 3;
      if (a) ip = b;
      break;
    case 6:
      ip += 3;
      if (!a) ip = b;
      break;
    case 7:
      bt_insert(m, pc, a < b);
      ip += 4;
      break;
    case 8:
      bt_insert(m, pc, a == b);
      ip += 4;
      break;
    case 9:
      base += a;
      ip += 2;
      break;
    case 99:
      halt = 1;
      break;
    default:
      printf("unhandled opcode: %ld\n", op);
      break;
    }
    if (halt)
    {
      break;
    }
  }

  bt_free(m);
  return out;
}

#include "intcode.h"

#include <inttypes.h>
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

vec_t *
vec_cpy(vec_t *v)
{
  if (!v) return NULL;
  vec_t *cpy = malloc(sizeof(vec_t));
  cpy->sz = v->sz;
  cpy->cap = v->sz;
  cpy->v = malloc(cpy->cap * sizeof(int64_t));
  for (size_t i = 0; i < cpy->sz; i++)
  {
    cpy->v[i] = v->v[i];
  }
  return cpy;
}

void
vec_reset(vec_t *v)
{
  v->sz = 0;
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
  if (n + 1 > v->cap)
  {
    v->cap *= 2;
    v->cap = v->cap > n + 1 ? v->cap : n + 1;
    v->v = realloc(v->v, v->cap * sizeof(int64_t));
  }
  v->v[n] = d;
  v->sz = v->sz > n + 1 ? v->sz : n + 1;
}

void
vec_append(vec_t *v, int64_t d)
{
  if (v->sz == v->cap)
  {
    v->cap *= 2;
    v->v = realloc(v->v, v->cap * sizeof(int64_t));
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

node_t *
node_cpy(node_t *node)
{
  if (!node) return NULL;
  node_t *cpy = malloc(sizeof(node_t));
  cpy->l = cpy->r = NULL;
  cpy->k = node->k; cpy->v = node->v;
  cpy->l = node_cpy(node->l);
  cpy->r = node_cpy(node->r);
  return cpy;
}

int
node_height(node_t *node)
{
  int l, r;
  if (!node) return 0;
  l = node_height(node->l);
  r = node_height(node->r);
  return 1 + (l < r ? r : l);
}

node_t *
node_rebalance(node_t *x)
{
  int b, zb, l, r;
  node_t *z;

  if (!x) return NULL;

  x->l = node_rebalance(x->l);
  x->r = node_rebalance(x->r);

  l = node_height(x->l); r = node_height(x->r);
  b = l - r;

  if (b == -2)
  {
    z = x->r;
    zb = node_height(z->l) - node_height(z->r);
    if (zb == 0 || zb == -1)
    {
      x->r = z->l;
      z->l = x;

      return z;
    }
    else if (zb == 1)
    {
      node_t *y = z->l;
      x->r = y->l;
      z->l = y->r;
      y->l = x; y->r = z;

      return y;
    }
    else
    {
      printf("|balance| > 1!!\n");
      exit(3);
      return NULL;
    }
  }
  else if (b == 2)
  {
    z = x->l;
    zb = node_height(z->l) - node_height(z->r);
    if (zb == 0 || zb == 1)
    {
      x->l = z->r;
      z->r = x;

      return z;
    }
    else if (zb == -1)
    {
      node_t *y = z->r;
      x->l = y->r;
      z->r = y->l;
      y->r = x; y->l = z;

      return y;
    }
    else
    {
      printf("|balance| > 1!!\n");
      exit(3);
      return NULL;
    }
    return NULL;
  }
  else if (b >= -1 && b <= 1)
  {
    return x;
  }
  else
  {
    printf("oops\n");
    exit(4);
    return NULL;
  }
}

int
node_insert(node_t **pn, int64_t k, int64_t v)
{
  node_t *n;
  if (*pn)
  {
    n = *pn;
    if (k == n->k)
    {
      n->v = v;
      return 0;
    }
    else if (k < n->k)
    {
      return node_insert(&n->l, k, v);
    }
    else
    {
      return node_insert(&n->r, k, v);
    }
  }
  else
  {
    n = *pn = node_new();
    n->k = k; n->v = v;
    return 1;
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

size_t
node_size(node_t *node)
{
  if (!node) return 0;
  return 1 + node_size(node->l) + node_size(node->r);
}

void
node_keys(node_t *node, vec_t *v)
{
  if (!node) return;
  node_keys(node->l, v);
  vec_append(v, node->k);
  node_keys(node->r, v);
}

void
node_free(node_t *node)
{
  if (!node) return;
  node_free(node->l);
  node_free(node->r);
  free(node);
}

struct bt_t
{
  node_t *r;
};

bt_t *
bt_new()
{
  bt_t *bt = malloc(sizeof(bt_t));
  bt->r = NULL;
  return bt;
}

bt_t *
bt_cpy(bt_t *bt)
{
  if (!bt) return NULL;
  bt_t *cpy = malloc(sizeof(bt_t));
  cpy->r = node_cpy(bt->r);
  return cpy;
}

void
bt_insert(bt_t *bt, int64_t k, int64_t v)
{
  if (node_insert(&bt->r, k, v))
  {
    bt->r = node_rebalance(bt->r);
  }
}

int64_t
bt_get(bt_t *bt, int64_t k)
{
  return node_get(bt->r, k);
}

size_t
bt_size(bt_t *bt)
{
  return node_size(bt->r);
}

vec_t *
bt_keys(bt_t *bt)
{
  vec_t *k = vec_new();
  node_keys(bt->r, k);
  return k;
}

void
bt_free(bt_t *bt)
{
  node_free(bt->r);
  free(bt);
}

struct intcode_t
{
  bt_t *m;
  vec_t *i;
  vec_t *o;
  int64_t ip;
  size_t inpos;
  int64_t base;
};

intcode_t *
intcode_new(int64_t *prog, size_t sz)
{
  intcode_t *m = malloc(sizeof(intcode_t));
  m->m = bt_new();
  m->i = vec_new();
  m->o = vec_new();
  m->ip = 0;
  m->inpos = 0;
  m->base = 0;
  for (size_t i = 0; i < sz; i++)
  {
    bt_insert(m->m, i, prog[i]);
  }
  return m;
}

intcode_t *
intcode_cpy(intcode_t *m)
{
  if (!m) return NULL;
  intcode_t *cpy = malloc(sizeof(intcode_t));
  cpy->m = bt_cpy(m->m);
  cpy->i = vec_cpy(m->i);
  cpy->o = vec_cpy(m->o);
  cpy->ip = m->ip;
  cpy->inpos = m->inpos;
  cpy->base = m->base;
  return cpy;
}

vec_t *
intcode_in(intcode_t *m)
{
  return m->i;
}

vec_t *
intcode_out(intcode_t *m)
{
  return m->o;
}

int64_t
intcode_mem_get(intcode_t *m, int64_t k)
{
  return bt_get(m->m, k);
}

void
intcode_mem_set(intcode_t *m, int64_t k, int64_t v)
{
  bt_insert(m->m, k, v);
}

void
intcode_free(intcode_t *m)
{
  vec_free(m->o);
  vec_free(m->i);
  bt_free(m->m);
  free(m);
}

int
intcode_run(intcode_t *m, int io)
{
  while (1)
  {
    int64_t pa, pb, pc, ma, mb, mc, a, b;
    int64_t opc, op, modes;
    int halt;

    opc = bt_get(m->m, m->ip);
    op = opc % 100;
    modes = opc / 100;
    halt = 0;
    pa = bt_get(m->m, m->ip + 1);
    pb = bt_get(m->m, m->ip + 2);
    pc = bt_get(m->m, m->ip + 3);
    ma = modes % 10; modes /= 10;
    mb = modes % 10; modes /= 10;
    mc = modes % 10; modes /= 10;
    if (ma == 2) pa += m->base;
    if (mb == 2) pb += m->base;
    if (mc == 2) pc += m->base;
    a = (ma == 1 ? pa : bt_get(m->m, pa));
    b = (mb == 1 ? pb : bt_get(m->m, pb));

    // printf("ip: %ld, opc: %ld, pa: %ld, pb: %ld, ma: %ld, mb: %ld, a: %ld, b: %ld\n", m->ip, opc, pa, pb, ma, mb, a, b);

    switch (op)
    {
    case 1:
      bt_insert(m->m, pc, a + b);
      m->ip += 4;
      break;
    case 2:
      bt_insert(m->m, pc, a * b);
      m->ip += 4;
      break;
    case 3:
      if (m->inpos == m->i->sz) {
        return NEEDS_INPUT;
      }
      bt_insert(m->m, pa, vec_get(m->i, m->inpos++));
      m->ip += 2;
      break;
    case 4:
      vec_append(m->o, a);
      m->ip += 2;
      if (io) {
        return GAVE_OUTPUT;
      }
      break;
    case 5:
      m->ip += 3;
      if (a) m->ip = b;
      break;
    case 6:
      m->ip += 3;
      if (!a) m->ip = b;
      break;
    case 7:
      bt_insert(m->m, pc, a < b);
      m->ip += 4;
      break;
    case 8:
      bt_insert(m->m, pc, a == b);
      m->ip += 4;
      break;
    case 9:
      m->base += a;
      m->ip += 2;
      break;
    case 99:
      halt = 1;
      break;
    default:
      printf("unhandled opcode at position %" PRId64 ": %" PRId64 "\n", m->ip, op);
      exit(2);
      break;
    }
    if (halt)
    {
      break;
    }
  }
  return HALTED;
}

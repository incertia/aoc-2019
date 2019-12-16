#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#define N 10000
#define EXPONENT 100

int
egcd(int a, int b, int *x, int *y)
{
  int x1, y1;
  int g;

  if (a == 0)
  {
      *x = 0;
      *y = 1;
      return b;
  }

  g = egcd(b % a, a, &x1, &y1);

  *x = y1 - (b / a) * x1;
  *y = x1;

  return g;
}

int
binp(int n, int k, int p)
{
  int res;
  if (k == 0)
  {
    res = 1;
  }
  else if (n < p && k < p)
  {
    int kinv, t;
    egcd(k, p, &kinv, &t);
    res = (n * (binp(n - 1, k - 1, p) % p) * kinv) % p;
  }
  else
  {
    res = (binp(n / p, k / p, p) * binp(n % p, k % p, p)) % p;
  }
  if (res < 0) res += p;
  return res;
}

int
bin10(int n, int k)
{
  // the iso between Z/2 x Z/5 <-> Z/10
  return (5 * binp(n, k, 2) + 6 * binp(n, k, 5)) % 10;
}

void
solve(int _)
{
  char *l = NULL;
  size_t sz = 0;
  int *data;

  getline(&l, &sz, stdin);
  sz -= 2;
  if (!_)
  {
    data = malloc(sz * sizeof(int));
    for (size_t j = 0; j < sz; j++)
    {
      data[j] = l[j] - '0';
    }
  }
  else
  {
    size_t offset = 0;
    int *bins;
    int result = 0;

    data = malloc(N * sz * sizeof(int));
    bins = malloc(N * sz * sizeof(int));
    for (int i = 0; i < N; i++)
    {
      for (size_t j = 0; j < sz; j++)
      {
        data[sz * i + j] = l[j] - '0';
      }
    }
    l[7] = 0;

    sscanf(l, "%zu", &offset);

    for (size_t i = 1; i <= N * sz - offset; i++)
    {
      // bins[offset + i - 1] = bin10(n - 1 + k - 1, k - 1)
      bins[offset + i - 1] = bin10(i - 1 + EXPONENT - 1, EXPONENT - 1);
      // printf("%d\n", bins[offset + i - 1]);
    }

    for (size_t i = 0; i < 8; i++)
    {
      int s = 0;
      for (size_t j = offset; j < N * sz - i; j++)
      {
        s += bins[j] * data[i + j];
        s %= 10;
      }
      result *= 10;
      result += s;
    }
    printf("%d\n", result);
    free(bins);
  }

  free(data);
  free(l);
}

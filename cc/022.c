#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gmp.h>

void
solve(int _)
{
  char *l = NULL;
  size_t sz = 0;
  char out[64];
  mpz_t pos, inc, n, phi, times, target, tmp1, tmp2;

  mpz_init_set_str(pos, "0", 10);
  mpz_init_set_str(inc, "1", 10);
  mpz_init(n);
  mpz_init(phi);
  mpz_init(times);
  mpz_init(target);
  mpz_init(tmp1);
  mpz_init(tmp2);

  if (!_)
  {
    mpz_set_str(n, "10007", 10);
    mpz_set_str(phi, "10006", 10);
    mpz_set_str(times, "1", 10);
    mpz_set_str(target, "2019", 10);
  }
  else
  {
    mpz_set_str(n, "119315717514047", 10);
    mpz_set_str(phi, "19315717514046", 10);
    mpz_set_str(times, "101741582076661", 10);
    mpz_set_str(target, "2020", 10);
  }

  while (getline(&l, &sz, stdin) != -1)
  {
    char w[64];
    sscanf(l, "%s", w);
    if (strcmp(w, "deal") == 0)
    {
      sscanf(l, "%*s %s", w);
      // reverse the deck
      if (strcmp(w, "into") == 0) {
        mpz_sub(pos, pos, inc);
        mpz_mod(pos, pos, n);
        mpz_sub(inc, n, inc);
      }
      else if (strcmp(w, "with") == 0)
      {
        mpz_t i;
        sscanf(l, "%*s %*s %*s %s", w);
        mpz_init_set_str(i, w, 10);
        mpz_invert(i, i, n);
        mpz_mul(inc, inc, i);
        mpz_mod(inc, inc, n);
        mpz_clear(i);
      }
    }
    else if (strcmp(w, "cut") == 0)
    {
      mpz_t i;
      sscanf(l, "%*s %s", w);
      mpz_init_set_str(i, w, 10);
      mpz_addmul(pos, i, inc);
      mpz_mod(pos, pos, n);
      mpz_clear(i);
    }
    free(l);
    l = NULL;
    sz = 0;
  }

  free(l);

  mpz_sub_ui(tmp1, inc, 1);     // tmp1 = inc - 1
  mpz_invert(tmp1, tmp1, n);    // tmp1 = (inc - 1)^-1
  mpz_powm(inc, inc, times, n); // inc = inc^times
  mpz_set(tmp2, inc);           // tmp2 = inc^times
  mpz_sub_ui(tmp2, tmp2, 1);    // tmp2 = inc^times - 1
  mpz_mul(pos, pos, tmp1);
  mpz_mul(pos, pos, tmp2);      // pos = pos * (inc^times - 1) / (inc - 1)
  mpz_mod(pos, pos, n);
  if (!_)
  {
    mpz_sub(tmp1, target, pos);
    mpz_mod(tmp1, tmp1, n);
    mpz_invert(tmp2, inc, n);
    mpz_mul(tmp1, tmp1, tmp2);
    mpz_mod(tmp1, tmp1, n);
  }
  else
  {
    mpz_mul(tmp1, target, inc);
    mpz_add(tmp1, tmp1, pos);
    mpz_mod(tmp1, tmp1, n);
  }
  mpz_get_str(out, 10, tmp1);
  printf("%s\n", out);

  mpz_clear(tmp2);
  mpz_clear(tmp1);
  mpz_clear(target);
  mpz_clear(times);
  mpz_clear(phi);
  mpz_clear(n);
  mpz_clear(inc);
  mpz_clear(pos);
}

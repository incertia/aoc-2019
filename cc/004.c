#include <stdio.h>
#include <string.h>

#include "solver.h"

void
solve(int z)
{
  int a, b, c = 0;
  scanf("%d-%d", &a, &b);

  for (int i = a; i <= b; i++) {
    char s[16];
    char m = 0;
    int f = 0;
    int rl = 0;
    sprintf(s, "%d\x7f", i);
    for (int j = 0; j < strlen(s); j++)
    {
      if (s[j] == m)
      {
        rl++;
      }
      else if (s[j] > m)
      {
        if ((!z && rl >= 2) || (z && rl == 2))
        {
          f = 1;
        }
        rl = 1;
        m = s[j];
      }
      else
      {
        f = 0;
        break;
      }
    }
    if (f)
    {
      c++;
    }
  }

  printf("%d\n", c);
}

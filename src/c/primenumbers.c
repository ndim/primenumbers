/* $Id: primenumbers.c,v 1.2 2003/09/23 21:50:24 ndim Exp $
 *
 * C version of prime number tests
 * Copyright (C) 2002 Hans Ulrich Niedermann <primes@n-dimensional.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <stdlib.h>

#ifdef FAST_OUTPUT
#include <unistd.h>

inline static ssize_t write_data(unsigned int t1, unsigned int t2)
{
  static char buf[] = "0000000000 0000000000\n";
  int n;

  for (n=9; n>=0; n--) {
    /* FIXME: shouldn't we use div(3) or ldiv(3) here? */
    buf[n] = '0' + t1 % 10;
    t1 /= 10;
    buf[11+n] = '0' + t2 % 10;
    t2 /= 10;
  }
  return write(STDOUT_FILENO, buf, 10+10+2);
}
#else
inline static int write_data(unsigned int i, unsigned int p)
{
    printf("%u %u\n", i, p);
}
#endif /* FAST_OUTPUT */

#define MAX (1<<16)

inline static unsigned int xqrt(unsigned int x)
{
  unsigned int i,l;
  if (x <= 0) return 0;
  i = x/2;
  while (1) {
    l = i;
    i = (l + x/l)/2;
    if (abs((int)l-i) <= 1) {
      return (i+x/i)/2;
    }
  }
}


unsigned int *primelist(unsigned int max)
{
  unsigned int i;
  unsigned int *p = malloc(max*sizeof(*p));
  p[0] = 2;
  p[1] = 3;
  for (i=2; i<max; ++i) {
    unsigned int k;
    unsigned int s;
    p[i] = p[i-1] + 2;
    k = 1;
    s = xqrt(p[i]);
    while ( (k<i) && (p[k] <= s) ) {
      if ((p[i] % p[k]) == 0) {
        p[i] += 2;
        s = xqrt(p[i]);
        k = 1;
      } else {
        ++k;
      }
    }
  }
  return p;
}


void print_primes(unsigned int *primes, unsigned int max)
{
  unsigned int i;
  for (i=0; i<max; ++i) {
    write_data(i, primes[i]);
  }
}


int main(int argc, char *argv[])
{
  unsigned int max = MAX;
  unsigned int *p = NULL;

  if (argc == 2) {
    long int tmp = strtol(argv[1], NULL, 10);
    max = (unsigned int) tmp;
  }

  p = primelist(max);
  print_primes(p, max);

  free(p);p=NULL;
  return 0;
}

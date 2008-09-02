/* $Id: glue.c,v 1.2 2003/09/23 21:50:24 ndim Exp $
 *
 * C helper for assembler version of prime number tests
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

#include <stdlib.h>
#include <stdio.h>
#include "primenumbers.h"

extern unsigned int *primetable;
extern unsigned int max;
extern void mainp();

int main(int argc, char *argv[])
{
  unsigned int i;
  if (argc == 2) {
    max = atoi(argv[1]);
  } else {
    max = MAX;
  }
  primetable = malloc(sizeof(*primetable)*max);
  mainp();
  for (i=0; i<max; ++i) {
    printf("%u %u\n",i,primetable[i]);
  }
  free(primetable);
  return 0;
}

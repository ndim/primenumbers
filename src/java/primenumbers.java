/* $Id: primenumbers.java,v 1.2 2003/09/23 21:50:24 ndim Exp $
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

public class primenumbers {

    final static int MAX = (1<<16);
    
    static int p[] = new int[MAX];

    static int sqrt(int x)
    {
        int i,l;
        if (x <= 0) return 0;
        i = x/2;
        while (true) {
            int t;
            l = i;
            i = (l + x/l)/2;
            t = l - i;
            if ((t >= -1) && (t <= 1)) {
                return (i+x/i)/2;
            }
        }
    }

    public static void main(String args[])
    {
        int i;
        p[0] = 2;
        p[1] = 3;
        for (i=2; i<MAX; ++i) {
            int k;
            int s;
            p[i] = p[i-1] + 2;
            k = 1;
            s = sqrt(p[i]);
            while ( (k<i) && (p[k] <= s) ) {
                if ((p[i] % p[k]) == 0) {
                    p[i] += 2;
                    s = sqrt(p[i]);
                    k = 1;
                } else {
                    ++k;
                }
            }
        }
        
        for (i=0; i<MAX; ++i) {
            System.out.print(i);
            System.out.print(" ");
            System.out.println(p[i]);
        }
    }

}

-- $Id: primenumbers.adb,v 1.2 2003/09/23 21:50:24 ndim Exp $
--
-- Ada version of prime number tests
-- Copyright (C) 2002 Hans Ulrich Niedermann <primes@n-dimensional.de>
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

with Ada.Text_IO, Ada.Integer_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO;

procedure PrimeNumbers is

   function Sqrt (X: in NATURAL) Return NATURAL is
      I: NATURAL := X/2;
      L: NATURAL;
   begin
      if X = 0 then
         return 0;
      end if;
      loop
         L := I;
         I := (I + X/I)/2;
         if abs(L-I) <= 1 then
            return (I+X/I)/2;
         end if;
      end loop;
   end Sqrt;

   P: array (0..2**16-1) of NATURAL;
   K: NATURAL range P'Range;
   S: NATURAL;

begin

   P(P'first) := 2;
   P(P'First+1) := 3;

   for I in  P'First+2..P'Last loop

      P(I) := P(I-1) + 2;
      K := P'First;
      S := Sqrt(P(I));

      while K < I and then P(K) <= S
      loop
         if P(I) rem P(K) = 0 then

            P(I) := P(I) + 2;
            K := P'First;
            S := Sqrt(P(I));

         else
            K := K + 1;
         end if;
      end loop;
   end loop;

   for I in P'Range loop
      Put(I,0);
      Put(' ');
      Put(P(I),0);
      New_Line;
   end loop;
   
end PrimeNumbers;

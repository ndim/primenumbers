/*
 * Attention: (for the uninitiated)
 * In REXX...
 * % is integer divide
 * // is integer remainder
 * & is boolean and
 * && is boolean xor
 * Apparently, REXX really is a very fine language.
 */

/* number of prime numbers to determine and print */
MAXi = 2**16 - 1

p.0 = 2;
p.1 = 3;

do i=2 to MAXi
  iminusone = i - 1
  p.i = p.iminusone + 2
  k = 1
  s = sqrt(p.i)
  do while ((k<i) & (p.k <= s))
    if ((p.i // p.k) == 0) then do
      p.i = p.i + 2
      s = sqrt(p.i)
      k = 1
      end
    else do
      k = k + 1;
      end
    end
  end

do i=0 to MAXi
  say i p.i
  end

exit

sqrt: procedure
arg x
if x <= 0 then return 0
i = x % 2
do while (0 == 0)
  l = i
  i = (l + x % l) % 2
  if (abs(l-i) <= 1) then return ((i + x % i) % 2)
  end


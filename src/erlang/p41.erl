%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Straight Erlang version of prime number tests in misc languages
%  Copyright (C) 2005 Hans Ulrich Niedermann <primes@n-dimensional.de>
%  Copyright (C) 2009 Hans Ulrich Niedermann <hun@n-dimensional.de>
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Please note that some functions have nonsensical names ("bork",
% "chalk").
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(p41).
-export([start/0, start/1]).
-export([primelist/1]).
-export([print_list/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% xqrt - fast, approximate integer square root
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xqrti(Xl,X) ->
    Xi = (Xl + X div Xl) div 2,
    case abs(Xl-Xi) =< 1 of
	true ->
	    (Xi+X div Xi) div 2;
	    %Xl;
	false ->
	    xqrti(Xi,X)
    end.

xqrt(0) ->
    0;
xqrt(X) when X > 0 ->
    Xi = X div 2,
    xqrti(Xi,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check whether Test is prime according to the list of primes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_prime(Test, Sqrt, [Head|Tail]) when Head > Sqrt ->
    %% skip testing primes larger than square root
    %% io:format("is_prime ~w ~w ~w Head > Sqrt~n", [Test,Sqrt,[Head|Tail]]),
    is_prime(Test, Sqrt, Tail);
is_prime(Test, _Sqrt, [Head|_Tail]) when (Test rem Head) == 0 ->
    %% Test can be divided by Head, and thus is non-prime
    %% io:format("is_prime ~w ~w ~w Test rem Head == 0~n",
    %%           [Test,_Sqrt,[Head|_Tail]]),
    false;
is_prime(Test, Sqrt, [_Head|Tail]) ->
    %% Test cannot be divided by Head, continue checking against Tail
    %% io:format("is_prime ~w ~w ~w otherwise ~n", [Test,Sqrt,[_Head|Tail]]),
    is_prime(Test, Sqrt, Tail);
is_prime(_Test, _Sqrt, []) ->
    %% Test is prime.
    %% io:format("is_prime ~w ~w ~w PRIME ~n", [_Test,_Sqrt,[]]),
    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check next odd number until we have the number of primes we want
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chalk(Test, List, ListLen, MaxNum) when ListLen < MaxNum ->
    Sqrt = xqrt(Test),
    %io:format("chalk ~w ~w ~w~n",[Test,Sqrt,List]),
    case is_prime(Test, Sqrt, List) of
	false ->
	    chalk(Test+2, List, ListLen, MaxNum);
	true ->
	    chalk(Test+2, [Test|List], (ListLen+1), MaxNum)
    end;
chalk(_Test, List, _ListLen, _MaxNum) ->
    List.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Print list of integers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_list(Index,[Head|Tail]) ->
    io:format("~w ~w~n",[Index,Head]),
    print_list(Index+1,Tail);
print_list(_Index,[]) ->
    ok.

print_list(List) ->
    print_list(0,List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Return list of prime numbers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

primelist(0) ->
    [];
primelist(1) ->
    [2];
primelist(MaxPrimeCount) when MaxPrimeCount > 1 ->
    chalk(5, [3,2], 2, MaxPrimeCount).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start([MaxPrimeCount]) when is_atom(MaxPrimeCount) ->
    start(list_to_integer(atom_to_list(MaxPrimeCount)));
start(MaxPrimeCount) when is_integer(MaxPrimeCount) ->
    PrimeList = primelist(MaxPrimeCount),
    ReversedList = lists:reverse(PrimeList),
    print_list(ReversedList),
    ok.

start() ->
    start(65536).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Local Variables:
% End:

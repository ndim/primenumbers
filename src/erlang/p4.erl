%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Straight Erlang version of prime number tests in misc languages
%  Copyright (C) 2005 Hans Ulrich Niedermann <primes@n-dimensional.de>
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


-module(p4).
-export([main/0]).


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

xqrt(X) when X =< 0 ->
    X;
xqrt(X) when X > 0 ->
    Xi = X div 2,
    xqrti(Xi,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check whether Test is prime according to the list or notprime
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bork(Test, Sqrt, [Head|Tail]) when Head > Sqrt ->
    List = [Head|Tail],
    {prime,[Test|List]};
bork(Test, _Sqrt, [Head|_Tail]) when (Test rem Head) == 0 ->
    {notprime,Head};
bork(Test, _Sqrt, [Head|Tail]) ->
    List = [Head|Tail],
    {prime,[Test|List]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check next odd number until we have the number of primes we want
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chalk(Test, [Head|Tail], MaxNum) when length([Head|Tail]) =< MaxNum ->
    Sqrt = xqrt(Test),
    %io:format("chalk ~w ~w ~w~n",[Test,Sqrt,[Head|Tail]]),
    case bork(Test, Sqrt, [Head|Tail]) of
	{notprime,_Foo} ->
	    chalk(Test+2, [Head|Tail], MaxNum);
	{prime,List} ->
	    chalk(Test+2, List, MaxNum)
    end;
chalk(_Test, List, _MaxNum) ->
    List.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Print list of integers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_list(Index,[Head|Tail]) ->
    io:format("~w ~w~n",[Index,Head]),
    print_list(Index+1,Tail);
print_list(_Index,[]) ->
    oerks.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Return list of prime numbers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

primelist() ->
    chalk(5, [3,2], 65536).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
    PrimeList = primelist(),
    ReversedList = lists:reverse(PrimeList),
    print_list(0,ReversedList),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Local Variables:
% compile-command:"./p4.sh"
% End:

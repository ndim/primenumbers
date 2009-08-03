%% Copyright (C) 2009 Matt Havener
%%
%% Originally found on
%% http://stackoverflow.com/questions/146622/sieve-of-eratosthenes-in-erlang/599002#599002
%%
%% Licensed as GPLv2+

-module(matthavener).
-export([start/0, start/1]).

primes(Prime, Max, Primes, Integers) when Prime > Max ->
    lists:reverse([Prime|Primes]) ++ Integers;
primes(Prime, Max, Primes, Integers) ->
    [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
    primes(NewPrime, Max, [Prime|Primes], NewIntegers).

primes(Max) ->
    primes(2, round(math:sqrt(Max)), [], lists:seq(3,Max,2)). % skip odds

print_list(Index,[Head|Tail]) ->
    io:format("~w ~w~n",[Index,Head]),
    print_list(Index+1,Tail);
print_list(_Index,[]) ->
    ok.

start(Max) ->
    L = primes(Max),
    print_list(0, L).

start() ->
    start(821641).

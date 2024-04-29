%% Copyright (C) 2009 Matt Havener
%%
%% Originally found on
%% http://stackoverflow.com/questions/146622/sieve-of-eratosthenes-in-erlang/599002#599002
%%
%% Licensed as GPLv2+

-module(matthavener).
-export([start/0, start/1]).

primes(Prime, MaxPrimeValue, Primes, Integers) when Prime > MaxPrimeValue ->
    lists:reverse([Prime|Primes]) ++ Integers;
primes(Prime, MaxPrimeValue, Primes, Integers) ->
    [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
    primes(NewPrime, MaxPrimeValue, [Prime|Primes], NewIntegers).

primes(MaxPrimeValue) ->
    primes(2, round(math:sqrt(MaxPrimeValue)), [], lists:seq(3,MaxPrimeValue,2)). % skip odds

print_list(Index,[Head|Tail]) ->
    io:format("~w ~w~n",[Index,Head]),
    print_list(Index+1,Tail);
print_list(_Index,[]) ->
    ok.

prime_count_to_value(Count) when Count == 100 ->
    542;
prime_count_to_value(Count) when Count == 1000 ->
    7920;
prime_count_to_value(Count) when Count == 65536 ->
    821642;
prime_count_to_value(Count) when Count == 131072 ->
    1742548;
prime_count_to_value(Count) when Count == 262144 ->
    3681132.

start([MaxPrimeCount]) when is_atom(MaxPrimeCount) ->
    start(list_to_integer(atom_to_list(MaxPrimeCount)));
start(MaxPrimeCount) when is_integer(MaxPrimeCount) ->
    MaxPrimeValue = prime_count_to_value(MaxPrimeCount),
    L = primes(MaxPrimeValue),
    print_list(0, L).

start() ->
    start(65536).

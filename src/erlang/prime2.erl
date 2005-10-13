%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Yet another try at hacking a prime number program in Erlang.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(prime2).
-export([oerks/0]).
-export([is_prime/1]).
-export([xqrt/1]).
-export([xt/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OK, just hacking doesn't work here - so let's try designing:
%
% - one square root function with sufficient efficiency for our purposes
% - one test for whether a number N is a multiple of any of the numbers
%   in a list L
% - one function which adds to the list of prime numbers "until it is full"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
% test whether a number N is a multiple of one of the list members
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_divides(N, [H|_T]) when (N rem H) == 0 ->
    true;
list_divides(N, [_|T]) ->
    list_divides(N,T);
list_divides(_N, []) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% also broken
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prime_loop(LargestPrime, [LargestPrime|PrimeList]) ->
    {true, [LargestPrime|PrimeList]};
prime_loop(N, [LargestPrime|PrimeList]) ->
    io:format("prime_loop(~w,~w)~n", [N,[LargestPrime|PrimeList]]),
    case list_divides(N,[3,2]) of
	true ->
	    prime_loop(LargestPrime+2, [LargestPrime|PrimeList]);
	false ->
	    XXL = [LargestPrime|PrimeList],
	    {true, [N|XXL]}
    end.    

is_prime(N) ->
    io:format("is_prime(~w)~n", [N]),
    StartList = [3,2],
    {IsPrime, PrimeList} = prime_loop(N, StartList),
    io:format("Prime List: ~w~n", [PrimeList]),
    IsPrime.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Let's oerks!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oerks(First, Last) when First > Last ->
    io:format("Bye.~n");
oerks(First, Last) ->
    io:format("   ~w ~w~n", [First, is_prime(First)]),
    oerks(First+1, Last).

oerks() ->
    oerks(2,30).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% xqrt test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xt() ->
    xt(2, 50).

xt(N, Max) ->
    io:format("    ~w ~w~n", [N, xqrt(N)]),
    if
	N < Max ->
	    xt(N+1, Max);
	true ->
	    io:format("Done.~n")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The End.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

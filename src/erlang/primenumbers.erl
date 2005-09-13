-module(primenumbers).
-export([main/0]).
-export([main/1]).


foreachr(F, [H|T]) ->
    foreachr(F, T),
    F(H);
foreachr(_, []) ->
    ok.


main() ->
    main(65536).


main(N) ->
    %io:format("Calculating...~n", []),
    Prime_List = calc_list(N),
    %Prime_Count = length(Prime_List),
    %io:format("Calculation done: count=~w requested=~w.~n", [Prime_Count, N]),

    %io:format("Prime numbers:~n", []),
    print_list(Prime_List).
    %io:format("End of prime number list.~n", []).


xqrt(X) when X =< 0 ->
    X;
xqrt(X) when X > 0 ->
    Xi = X/2,
    xqrti(Xi,X).

xqrti(Xl,X) ->
    Xi = (Xl + X/Xl)/2,
    case abs(Xl-Xi) =< 1 of
	true ->
	    (Xi+X/Xi)/2;	    
	false ->
	    xqrti(Xi,X)
    end.


print_list(Prime_List) ->
    foreachr(fun(Prime) ->
		    io:format("~w~n", [Prime])
	    end, Prime_List).


calc_list(N) ->
    calc(5, [3,2], N).


calc(Pi, Prime_List, N) when ((N >= 2) and (length(Prime_List) < N)) ->
    case is_prime(Pi, Prime_List) of
	true ->
	    calc(Pi+2, [Pi | Prime_List ], N);
	false ->
	    calc(Pi+2, Prime_List, N)
    end;
calc(_, Prime_List, N) when N >= 2 ->
    Prime_List.


is_prime(Pi, Prime_List) ->
    Pis = xqrt(Pi),
    (length([X || X <- Prime_List, 
		  (Pis >= X) and ((Pi rem X) == 0) ]) == 0).

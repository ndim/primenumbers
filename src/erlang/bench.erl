-module(bench).
-export([main/0, main/1, run_bench/1]).


run_bench(Count) ->
    Modules = [p4, p5, p6, p7,
	       prime_sieve_ack_flex,
	       prime_sieve_circle],
    lists:map(fun(M) ->
        io:format("FUN with ~p~n", [M]),
        statistics(runtime),
        statistics(wall_clock),
        Primes = apply(M, primelist, [Count]),
        {_, Runtime} = statistics(runtime),
        {_, WallClockTime} = statistics(wall_clock),
        {M, Runtime, WallClockTime, Primes}
    end, Modules).


main(Count) ->
    B = run_bench(Count),
    io:format("~p~n", [lists:map(fun({M,R,W,P}) ->
        {M,R,W,length(P),lists:sublist(P, 5)}
    end, B)]).


main() ->
    main(1000).

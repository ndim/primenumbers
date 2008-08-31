-module(bench).
-export([main/0, main/1, run_bench/1]).
-export([write_report/1]).


%%
%% Ideas:
%%   * Instrument the code in the tested modules such that a running
%%     filter process can report certain milestones to the benchmark.
%%     This should save at least half the compute time.
%%


run_bench(Count) ->
    Modules = [p4, p5, %p6, p7, % behave almost like p5
	       prime_sieve_ack_flex,
	       prime_sieve_circle,
	       %prime_sieve_circle2 % behaves almost like prime_sieve_circle3
	       prime_sieve_circle3
	      ],
    lists:map(fun(M) ->
		      io:format("~p ~p~n", [Count, M]),
		      statistics(runtime),
		      statistics(wall_clock),
		      Primes = apply(M, primelist, [Count]),
		      {_, Runtime} = statistics(runtime),
		      {_, WallClockTime} = statistics(wall_clock),
		      io:format("    ~p~n", [{M, Runtime, WallClockTime}]),
		      {M, Runtime, WallClockTime, Primes}
	      end,
	      Modules).


main(Count) ->
    Results = lists:map(
		fun({M,R,W,P}) ->
			{M,R,W,length(P),lists:sublist(P, 5)}
		end,
		run_bench(Count)),
    %% io:format("~p~n", [Results]),
    Results.


test_points(N, Scale) ->
    [round(Scale*math:exp(math:log(10)*float(X)/float(N))) || X <- lists:seq(0, N-1)].


main() ->
    %% {5000,prime_sieve_ack} takes 60 seconds on my Intel Core Duo T2500
    TestCounts = lists:concat([test_points(6, 100),
    %%                           test_points(3, 1000), [10000]]),
                               test_points(3, 1000),
                               test_points(3, 10000), [100000]]),
    Results = lists:map(fun(N) -> {N, main(N)} end, TestCounts),
    io:format("~p~n", [Results]),
    Results.


write_report([FileName]) ->
    io:format("Writing report to ~p.~n", [FileName]),
    {ok,S} = file:open(FileName, write),
    Results = main(),
    [{_Count, ZeroResults}|_] = Results,
    ModStrings = lists:map(fun({M,_,_,_,_}) -> atom_to_list(M) end,
			   ZeroResults),
    Headings = ["count"|ModStrings],
    io:format(S, "~s~n", [string:join(Headings, "\t")]),
    lists:foreach(fun({Count, CountResults}) ->
			  io:format(S, "~p", [Count]),
			  lists:foreach(fun({_M,Runtime,_,_,_}) ->
						io:format(S, "\t~p", [Runtime])
					end, CountResults),
			  io:format(S, "~n", [])
		  end, Results),
    file:close(S),
    io:format("Report written to ~p.~n", [FileName]).

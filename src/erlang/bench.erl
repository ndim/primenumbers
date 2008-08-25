-module(bench).
-export([main/0, main/1, run_bench/1]).
-export([write_report/1]).


run_bench(Count) ->
    Modules = [p4, p5, p6, p7,
	       prime_sieve_ack_flex,
	       prime_sieve_circle],
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


main() ->
    %% {5000,prime_sieve_ack} takes 60 seconds on my Intel Core Duo T2500
    TestCounts = [
		  10, 20, 30, 50, 70,
		  100, 150, 200, 300, 500, 700,
		  1000, 2000, 5000
		  %% 10000
		 ],
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

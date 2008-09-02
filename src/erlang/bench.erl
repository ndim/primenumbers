-module(bench).
-export([main/0]).
-export([write_report/1]).
-export([write_output_files/1]).


-define(MAX_RUNTIME, 21000).


%%
%% Ideas:
%%   * Instrument the code in the tested modules such that a running
%%     filter process can report certain milestones to the benchmark.
%%     This should save at least half the compute time.
%%


run_bench(Count, Module) ->
      io:format("~p ~p~n", [Count, Module]),
      statistics(runtime),
      statistics(wall_clock),
      Primes = apply(Module, primelist, [Count]),
      {_, Runtime} = statistics(runtime),
      {_, WallClockTime} = statistics(wall_clock),
      io:format("    ~p~n", [{Module, Runtime, WallClockTime}]),
      {Module, Runtime, WallClockTime, Primes}.


run_bench_for_counts([], _Mod) ->
    [];
run_bench_for_counts([Count|TestCounts], Mod) ->
    case run_bench(Count, Mod) of
        {_M,R,_W,_P} when R > ?MAX_RUNTIME ->
            [ {Count, R} ];
        {_M,R,_W,_P} ->
            [ {Count, R} | run_bench_for_counts(TestCounts, Mod)]
    end.


run_bench_for_all(_TestCounts, []) ->
    [];
run_bench_for_all(TestCounts, [Mod|Modules]) ->
    [ {Mod, run_bench_for_counts(TestCounts, Mod)} | run_bench_for_all(TestCounts, Modules) ].


test_points(N, Scale) ->
    [round(Scale*math:exp(math:log(10)*float(X)/float(N))) || X <- lists:seq(0, N-1)].


module_list() ->
    [p4,
     p5,
     % p6, p7, % behave almost like p5
     prime_sieve_ack_flex,
     prime_sieve_circle,
     % prime_sieve_circle2 % behaves almost like prime_sieve_circle3
     prime_sieve_circle3
    ].


main() ->
    Modules = module_list(),
    %% {5000,prime_sieve_ack} takes 60 seconds on my Intel Core Duo T2500
    TestCounts = lists:concat([test_points(6, 100),
                               %%test_points(3, 1000), [10000]]),
                               test_points(3, 1000),
                               test_points(3, 10000), [100000]]),
    Results = run_bench_for_all(TestCounts, Modules),
    io:format("~p~n", [Results]),
    Results.


write_report([BaseName]) ->
    Results = main(),
    write_report(BaseName, Results).


to_filename([H|T]) when is_atom(H) ->
    [atom_to_list(H)|to_filename(T)];
to_filename([H|T]) when is_list(H) ->
    [H|to_filename(T)];
to_filename([]) ->
    [].


to_filename(Base, Mod, Ext) ->
    to_filename([Base, "-", Mod, Ext]).
to_filename(Base, Mod) ->
    to_filename(Base, Mod, ".txt").


write_report(_BaseName, []) ->
    ok;
write_report(BaseName, [{Module, Timings} | Tail]) ->
    FileName = to_filename(BaseName, Module, ".txt"),
    TmpFileName = to_filename(BaseName, Module, ".new"),
    io:format("Writing report to ~s.~n", [TmpFileName]),
    {ok,S} = file:open(TmpFileName, write),
    write_timings(S, Timings),
    file:close(S),
    io:format("Report written to ~s.~n", [TmpFileName]),
    file:rename(TmpFileName, FileName),
    io:format("Report renamed to ~s.~n", [FileName]),
    write_report(BaseName, Tail).


write_timings(_File, []) ->
    ok;
write_timings(File, [{Count, Runtime} | Timings]) ->
    io:format(File, "~p\t~p~n", [Count, Runtime]),
    write_timings(File, Timings).


write_output_files([BaseName]) ->
    lists:foreach(fun(Mod) ->
                      io:format("~s~n", [to_filename(BaseName, Mod)])
                  end,
                  module_list()).


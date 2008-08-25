%% Idea:
%%
%%    +--------------------- test_ack -----------------------+
%%    |                                                      |
%%    |  +------ test_ack -----+             +- new_next -+  |
%%    |  |                     |             |            |  |
%%    V  V                     |             V            |  |
%%   counter -> sieve(2) -> sieve(3) ... -> sieve(N) -> archive

-module(prime_sieve_circle).
-export([start/0, primelist/0]).
-export([controller/4, counter/2, sieve/2]).


print_action(Index, TestMe, {max_index, MaxIndex}) when Index < MaxIndex ->
    io:format("~p ~p~n", [Index, TestMe]);
print_action(Index, TestMe, {max_prime, MaxPrime}) when TestMe < MaxPrime ->
    io:format("~p ~p~n", [Index, TestMe]).


controller(Index, List, Action, ActionConfig)
  when is_integer(Index), is_list(List), is_function(Action) ->
    receive
	{Prev, test, TestMe} ->
	    NewSieve = spawn_link(?MODULE, sieve, [TestMe, self()]),
	    Prev ! {self(), new_next, NewSieve},
	    counter ! {self(), test_ack, TestMe},
	    Action(Index, TestMe, ActionConfig),
	    controller(Index+1, [TestMe|List], Action, ActionConfig)
    end.


counter(N, Next) when is_integer(N), N >= 1 ->
    receive
	{_SieveOrController, test_ack, N} ->
	    Next ! {self(), test, N+1},
	    counter(N+1, Next);
	{Next, new_next, NewNext} ->
	    counter(N, NewNext)
    end.


sieve(N, Next) when is_integer(N), N >= 2 ->
    receive
	{_Prev, test, TestMe} when (TestMe rem N) =:= 0 ->
	    counter ! {self(), test_ack, TestMe},
	    sieve(N, Next);
	{_Prev, test, TestMe} ->
	    Next ! {self(), test, TestMe},
	    sieve(N, Next);
	{Next, new_next, NewNext} ->
	    sieve(N, NewNext)
    end.


primelist() ->
    not_implemented_yet.


start() ->
    Controller = spawn_link(?MODULE, controller, [0, [],
						  fun print_action/3,
						  {max_index, 65536}
						 ]),
    Counter = spawn_link(?MODULE, counter, [1, Controller]),
    register(controller, Controller),
    register(counter, Counter),
    io:format("Controller: ~p, Counter: ~p~n", [Controller, Counter]),
    Counter ! {self(), test_ack, 1},
    ok.

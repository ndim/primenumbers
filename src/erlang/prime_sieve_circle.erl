%% Idea:
%%
%%    +--------------------- test_ack -----------------------+
%%    |                                                      |
%%    |  +------ test_ack -----+             +- new_next -+  |
%%    |  |                     |             |            |  |
%%    V  V                     |             V            |  |
%%   counter -> sieve(2) -> sieve(3) ... -> sieve(N) -> controller

-module(prime_sieve_circle).
-export([start/0, primelist/0]).
-export([controller/2, counter/2, sieve/2]).


-record(action_state,
	{index = 0,
	 prime_list = [],
	 max_index}).

-record(collect_state,
	{index = 0,
	 prime_list = [],
	 max_index}).


print_action(TestMe, #action_state{index=Index,
				   prime_list=PrimeList,
				   max_index=MaxIndex})
  when Index < MaxIndex ->
    io:format("~p ~p~n", [Index, TestMe]),
    #action_state{index=Index+1,
		  prime_list=[TestMe|PrimeList],
		  max_index=MaxIndex};
print_action(TestMe, State) ->
    io:format("MOOOOO! ~p ~p~n", [TestMe, State]).


collect_action(TestMe, #collect_state{index=Index,
				      max_index=MaxIndex,
				      prime_list=PrimeList})
  when Index < MaxIndex ->
    io:format("C ~p ~p ~p~n", [Index, TestMe, PrimeList]),
    #collect_state{index=Index+1,
		   max_index=MaxIndex,
		   prime_list=[TestMe|PrimeList]};
collect_action(TestMe, State) ->
    io:format("BBAAAAAR! ~p ~p~n", [TestMe, State]).


controller(Action, ActionState)
  when is_function(Action) ->
    receive
	{Someone, done} ->
	    Someone ! {self(), done, ActionState};
	{Prev, test, TestMe} ->
	    NewSieve = spawn_link(?MODULE, sieve, [TestMe, self()]),
	    Prev ! {self(), new_next, NewSieve},
	    counter ! {self(), test_ack, TestMe},
	    NextActionState = Action(TestMe, ActionState),
	    controller(Action, NextActionState)
    end.


counter(N, Next) when is_integer(N), N >= 1 ->
    receive
	{_Controller, done} ->
	    Next ! {self(), done};
	{_SieveOrController, test_ack, N} ->
	    Next ! {self(), test, N+1},
	    counter(N+1, Next);
	{Next, new_next, NewNext} ->
	    counter(N, NewNext)
    end.


sieve(N, Next) when is_integer(N), N >= 2 ->
    receive
	{_Controller, done} ->
	    Next ! {self(), done};
	{_Prev, test, TestMe} when (TestMe rem N) =:= 0 ->
	    counter ! {self(), test_ack, TestMe},
	    sieve(N, Next);
	{_Prev, test, TestMe} ->
	    Next ! {self(), test, TestMe},
	    sieve(N, Next);
	{Next, new_next, NewNext} ->
	    sieve(N, NewNext)
    end.


start(Action, ActionState) ->
    Controller = spawn_link(?MODULE, controller,
			    [Action, ActionState]),
    Counter = spawn_link(?MODULE, counter, [1, Controller]),
    register(controller, Controller),
    register(counter, Counter),
    io:format("Controller: ~p, Counter: ~p~n", [Controller, Counter]),
    Counter ! {self(), test_ack, 1},
    receive
    after 2 ->
	    ok
    end,
    Counter ! {self(), done},
    receive
	{Counter, done, Result} ->
	    io:format("RESULT: ~p~n", [Result]),
	    ok
    end.


primelist() ->
    start(fun collect_action/2, #collect_state{max_index=65536}).


start() ->
    start(fun print_action/2, #action_state{max_index=65536}).

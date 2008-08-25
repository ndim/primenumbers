%% The basic idea for message flow between the processes:
%%
%%                        (if N is prime)
%%  +------------------------ test_ack --------------------------+
%%  |                                                            |    COMPUTE
%%  |  +------ test_ack ------+                  +- new_next -+  |    PRIME
%%  |  |   (if N not prime)   |                  |            |  |    NUMBERS
%%  V  V                      |                  V            |  |
%% counter --> sieve(2) --> sieve(3) ... --> sieve(N) --> controller
%%  ^  |        ^   |        ^  |              ^   |        ^  |  |   REPORT
%%  |  |        |   |        |  |              |   |        |  |  |   RESULTS
%%  |  +--done--+   +--done--+  +-- ... -done--+   +--done--+  |  |   TO CALLER
%%  |                                                          |  |
%%  +-------------------------- done --------------------------+  +---> run()
%%            (when controller has seen "enough" primes)
%%
%% Compared to the n-tier model, the above architecture significantly
%% reduces the total number of messages to be sent:
%%
%%           test         test             test         test
%%   counter --> sieve(2) --> sieve(3) ... --> sieve(N) --> finalist
%%           <--          <--              <--          <--
%%           ack          ack              ack          ack
%%

-module(prime_sieve_circle).
-export([start/0]).
-export([primelist/0, primelist/1]).
-export([controller/3, counter/2, sieve/2]).


%-define(DEFAULT_MAX_INDEX, 5).
-define(DEFAULT_MAX_INDEX, 65536).


-record(index_state,
	{index = 0,
	 prime_list = [],
	 max_index}).


print_action(TestMe, #index_state{index=Index,
				  prime_list=PrimeList,
				  max_index=MaxIndex})
  when Index < MaxIndex ->
    io:format("~p ~p~n", [Index, TestMe]),
    #index_state{index=Index+1,
		 prime_list=[TestMe|PrimeList],
		 max_index=MaxIndex};
print_action(_TestMe, _State) ->
    done.


collect_action(TestMe, #index_state{index=Index,
				    max_index=MaxIndex,
				    prime_list=PrimeList})
  when Index < MaxIndex ->
    #index_state{index=Index+1,
		   max_index=MaxIndex,
		   prime_list=[TestMe|PrimeList]};
collect_action(_TestMe, _State) ->
    done.


controller(Action, ActionState, Heir)
  when is_function(Action) ->
    receive
        {_Prev, done} ->
	    %% counter and all sieves are_dead now.
	    %% Only we still have any record of what happened.
	    %% Pass it on to our heir and die in peace.
	    Heir ! {self(), done, ActionState};
	{Prev, test, TestMe} ->
	    case Action(TestMe, ActionState) of
		done ->
		    counter ! {self(), done},
		    controller(Action, ActionState, Heir);
		NextActionState ->
		    NewSieve = spawn_link(?MODULE, sieve, [TestMe, self()]),
		    Prev ! {self(), new_next, NewSieve},
		    receive
			{Prev, new_next_ack, NewSieve} ->
			    ok
		    end,
		    counter ! {self(), test_ack, TestMe},
		    controller(Action, NextActionState, Heir)
	    end
    end.


counter(N, Next) when is_integer(N), N >= 1 ->
    receive
	{_Controller, done} ->
	    Next ! {self(), done};
	{_SieveOrController, test_ack, N} ->
	    Next ! {self(), test, N+1},
	    counter(N+1, Next);
	{Next, new_next, NewNext} ->
	    Next ! {self(), new_next_ack, NewNext},
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
	    Next ! {self(), new_next_ack, NewNext},
	    sieve(N, NewNext)
    end.


run(Action, ActionState) ->
    Controller = spawn_link(?MODULE, controller,
			    [Action, ActionState, self()]),
    Counter = spawn_link(?MODULE, counter, [1, Controller]),
    register(controller, Controller),
    register(counter, Counter),
    Counter ! {self(), test_ack, 1},
    receive
	{Controller, done, Result} ->
	    Result
    end.


primelist(MaxIndex) ->
    #index_state{prime_list=PrimeList}
	= run(fun collect_action/2, #index_state{max_index=MaxIndex}),
    PrimeList.


primelist() ->
    primelist(?DEFAULT_MAX_INDEX).


start() ->
    #index_state{max_index=?DEFAULT_MAX_INDEX}
	= run(fun print_action/2, #index_state{max_index=?DEFAULT_MAX_INDEX}),
    ok.

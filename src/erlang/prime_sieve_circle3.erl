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
%% Idea:
%%   Have controller stop adding new sieves when it can be shown
%%   that the new sieves would never catch a non-prime number.
%%   FIXME: Is there a useful estimation for the nth prime number?
%%          Knowing that the 2^16th prime number is between 800000
%%          and 900000 would be sufficient.
%%
%% Idea:
%%   Have counter count 2,3,5,7,9,11,etc.

-module(prime_sieve_circle3).
-export([start/0, start/1]).
-export([primelist/1]).
-export([controller/4, counter/2, sieve/2]).


-define(DEFAULT_MAX_COUNT, 65536).


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


controller(Action, ActionState, Heir, MaxPrime)
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
		    controller(Action, ActionState, Heir, MaxPrime);
		NextActionState when TestMe*TestMe > MaxPrime ->
		    counter ! {self(), test_ack, TestMe},
		    controller(Action, NextActionState, Heir, MaxPrime);
		NextActionState ->
		    NewSieve = spawn_link(?MODULE, sieve, [TestMe, self()]),
		    Prev ! {self(), new_next, NewSieve},
		    receive
			{Prev, new_next_ack, NewSieve} ->
			    ok
		    end,
		    counter ! {self(), test_ack, TestMe},
		    controller(Action, NextActionState, Heir, MaxPrime)
	    end
    end.


counter(N, Next) when is_integer(N), N >= 3 ->
    receive
	{_Controller, done} ->
	    Next ! {self(), done};
	{_SieveOrController, test_ack, N} ->
	    Next ! {self(), test, N+2},
	    counter(N+2, Next);
	{Next, new_next, NewNext} ->
	    Next ! {self(), new_next_ack, NewNext},
	    counter(N, NewNext)
    end;
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


run(Action, ActionState, MaxPrime) ->
    Controller = spawn_link(?MODULE, controller,
			    [Action, ActionState, self(), MaxPrime]),
    Counter = spawn_link(?MODULE, counter, [1, Controller]),
    register(controller, Controller),
    register(counter, Counter),
    Counter ! {self(), test_ack, 1},
    receive
	{Controller, done, Result} ->
	    Result
    end.


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


primelist(MaxCount) ->
    #index_state{prime_list=PrimeList}
	= run(fun collect_action/2, #index_state{max_index=MaxCount},
	      prime_count_to_value(MaxCount)),
    PrimeList.


start([Max]) when is_atom(Max) ->
    start(list_to_integer(atom_to_list(Max)));
start(Max) when is_integer(Max) ->
    #index_state{max_index=Max}
	= run(fun print_action/2, #index_state{max_index=Max},
	      prime_count_to_value(Max)),
    ok.


start() ->
    start(?DEFAULT_MAX_COUNT).

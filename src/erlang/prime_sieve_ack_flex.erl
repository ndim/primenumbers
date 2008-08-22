%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prime number sieve (different algorithm)
%  Copyright (C) 2007 Hans Ulrich Niedermann <primes@n-dimensional.de>
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Prime number sieve.
%% 
%% Race condition fixed by using synchronous message passing
%% (i.e. always wait for an acknowledging answer message).
%%
%% The idea behind this program is detailed starting 28:30 in this video:
%% <a href="http://video.google.com/videoplay?docid=810232012617965344">http://video.google.com/videoplay?docid=810232012617965344</a>
%% 
%% Ideas for improvement:
%%   - do not instantiate filter for N if we are sure we will not need it
%%     ( N*N > largest_prime_in_array )
%%   - or, directly send the new number N to test to the filter for
%%     sqrt(N)
%%
%% FIXME: Terminate the sieve processes properly.

-module(prime_sieve_ack_flex).
-export([start/0, start/1, primelist/1]).
-export([sieve/3, counter/3, receiver/4]).


%% @spec test_next(Next, TestMe) -> Pid
%%
%% @doc Decide whether <tt>TestMe</tt> is prime, handle it
%% accordingly, and return the proper next sieve in the chain for
%% future checks.
test_next(none, TestMe, Receiver)
    when is_integer(TestMe), is_pid(Receiver) ->
    %% Hooray! We found a prime number!
    % io:format("XXX ~w~n", [TestMe]),
    Receiver ! {self(), prime, TestMe},
    receive
        {Receiver, ack} ->
            %% Add a sieve removing the multiples of this prime number.
            spawn_link(?MODULE, sieve, [none, TestMe, Receiver]);
        {Receiver, done} ->
            none % FIXME
    end;
test_next(Next, TestMe, Receiver)
    when is_pid(Next), is_integer(TestMe), is_pid(Receiver) ->
    %% Delegate prime testing to next sieve in sieve chain.
    Next ! {self(), test, TestMe},
    receive {Next, ack, TestMe} -> Next end.


%% @spec counter(Controller, Next, Counter, Max) -> irrelevant_value
%% @doc Counter sending the numbers to the next process in a chain.
%%
%% Counts to <tt>Max</tt>, sending each number to <tt>Next</tt>.
%% Signals <tt>Next</tt> and <tt>Controller</tt> when <tt>Max</tt> is
%% reached.
counter(Next, Counter, Receiver)
    when is_integer(Counter),
         is_pid(Receiver) ->
    NewNext = test_next(Next, Counter, Receiver),
    counter(NewNext, Counter+1, Receiver).


%% @spec sieve(Next, N) -> irrelevant_value
%% @doc Sieve filtering out all multiples of N, passing on all other numbers.
sieve(Next, N, Receiver)
    when is_integer(N), is_pid(Receiver) ->
    receive
	{Sender, done} ->
    	    %% Make sure the rest of the sieve chain is done.
            case Next of
                none ->
                    done;
                Next when is_pid(Next) ->
                    Next ! {self(), done},
                    receive {Next, done} -> done end
            end,
	    %% signal that we (and the whole Next tail) are done
	    Sender ! {self(), done};
	{Sender, test, TestMe} when (TestMe rem N) =:= 0 ->
	    %% TestMe is definitely not a prime number. Signal sender
	    %% that we are done with this number...
	    Sender ! {self(), ack, TestMe},
	    %% ...and wait for the next prime candiate.
	    sieve(Next, N, Receiver);
	{Sender, test, TestMe} ->
	    %% TestMe may be a prime number. Delegate testing...
	    NewNext = test_next(Next, TestMe, Receiver),
	    %% ...and signal sender that we are done with this number...
	    Sender ! {self(), ack, TestMe},
	    %% ...and wait for the next prime candiate.
	    sieve(NewNext, N, Receiver)
    end.


%% @spec receiver(Index, List)
receiver(Controller, Count, Index, List)
    when is_pid(Controller), is_integer(Count),
         is_integer(Index), is_list(List) ->
    receive
        {Pid, prime, _Prime} when Index >= Count ->
            %% We have generated one prime too many here - ignore it
            Pid ! {self(), done},
            Controller ! {self(), done, List};
        {Pid, prime, Prime} ->
            %% io:format("PP ~w ~w~n", [Index, Prime]),
            Pid ! {self(), ack},
            receiver(Controller, Count, Index+1, [Prime|List]);
        {Pid, list_primes} ->
            Pid ! {self(), prime_list, List},
            receiver(Controller, Count, Index, List)
    end.


primelist(Count) when is_integer(Count), Count > 0 ->
    Receiver = spawn_link(?MODULE, receiver, [self(), Count, 0, []]),
    Receiver ! {self(), prime, 2},
    Sieve = spawn_link(?MODULE, sieve, [none, 2, Receiver]),
    Counter = spawn_link(?MODULE, counter, [Sieve, 2, Receiver]),
    receive
        {Receiver, done, PrimeList} ->
             Counter ! {self(), done},
             PrimeList
    end.


%% @spec start(Count) -> done
%% @doc Start generating prime numbers smaller than <tt>Count</tt>.
start(Count) when is_integer(Count) ->
    List = primelist(Count),
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, List).


%% @spec start() -> done
%% @doc Start generating prime numbers.
start() ->
    start(65536).

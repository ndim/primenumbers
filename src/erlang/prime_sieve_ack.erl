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

-module(prime_sieve_ack).
-export([start/0, start/1]).
-export([sieve/2, counter/4]).


%% @spec test_next(Next, TestMe) -> Pid
%%
%% @doc Decide whether <tt>TestMe</tt> is prime, handle it
%% accordingly, and return the proper next sieve in the chain for
%% future checks.
test_next(none, TestMe) ->
    %% Hooray! We found a prime number!
    io:format("~w~n", [TestMe]),
    %% Add a sieve removing the multiples of this prime number.
    spawn(?MODULE, sieve, [none, TestMe]);
test_next(Next, TestMe) ->
    %% Delegate prime testing to next sieve in sieve chain.
    Next ! {self(), test, TestMe},
    receive {Next, ack, TestMe} -> Next end.


%% @spec done_next(Next) -> done
%%
%% @doc Wait until all processes in sieve chain are done.
done_next(none) ->
    done;
done_next(Next) ->
    %% Make sure the rest of the sieve chain is done.
    Next ! {self(), done},
    receive {Next, done} -> done end.


%% @spec counter(Controller, Next, Counter, Max) -> irrelevant_value
%% @doc Counter sending the numbers to the next process in a chain.
%%
%% Counts to <tt>Max</tt>, sending each number to <tt>Next</tt>.
%% Signals <tt>Next</tt> and <tt>Controller</tt> when <tt>Max</tt> is
%% reached.
counter(Controller, Next, Counter, Max) when Counter < Max ->
    NewNext = test_next(Next, Counter),
    counter(Controller, NewNext, Counter+1, Max);
counter(Controller, Next, _Counter, _Max) ->
    done_next(Next),
    Controller ! {self(), done}.


%% @spec sieve(Next, N) -> irrelevant_value
%% @doc Sieve filtering out all multiples of N, passing on all other numbers.
sieve(Next, N) ->
    receive
	{Sender, done} ->
	    %% wait for next filter to finish
	    done_next(Next),
	    %% signal that we (and the whole Next tail) are done
	    Sender ! {self(), done};
	{Sender, test, TestMe} when (TestMe rem N) =:= 0 ->
	    %% TestMe is definitely not a prime number. Signal sender
	    %% that we are done with this number...
	    Sender ! {self(), ack, TestMe},
	    %% ...and wait for the next prime candiate.
	    sieve(Next, N);
	{Sender, test, TestMe} ->
	    %% TestMe may be a prime number. Delegate testing...
	    NewNext = test_next(Next, TestMe),
	    %% ...and signal sender that we are done with this number...
	    Sender ! {self(), ack, TestMe},
	    %% ...and wait for the next prime candiate.
	    sieve(NewNext, N)
    end.


%% @spec start(Max) -> done
%% @doc Start generating prime numbers smaller than <tt>Max</tt>.
start(Max) when is_integer(Max) ->
    Sieve2 = spawn(?MODULE, sieve, [none, 2]),
    Counter = spawn(?MODULE, counter, [self(), Sieve2, 2, Max]),
    receive
	{Counter, done} ->
	    done
    end.


%% @spec start() -> done
%% @doc Start generating prime numbers smaller than a default value.
start() ->
    start(1000).
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

-module(prime_sieve_ack).
-export([start/0, start/1, start/2]).
-export([sieve/5, counter/6]).
-export([receiver/4, primelist/1]).


-compile([export_all]).


-define(MAX_VALUE, 821642). %% limit for the first 2^16 prime numbers


%% @spec test_next(Next, TestMe) -> Pid
%%
%% @doc Decide whether <tt>TestMe</tt> is prime, handle it
%% accordingly, and return the proper next sieve in the chain for
%% future checks.
test_next(none, Index, TestMe, Max, ReportFun) ->
    %% Hooray! We found a prime number!
    ok = ReportFun(Index, TestMe),
    %% Add a sieve removing the multiples of this prime number.
    case (TestMe*TestMe) =< Max of
	true ->
	    {Index, spawn_link(?MODULE, sieve, [none, Index+1, TestMe, Max, ReportFun])};
	false ->
	    {Index+1, none}
    end;
test_next(Next, Index, TestMe, _Max, _ReportFun) ->
    %% Delegate prime testing to next sieve in sieve chain.
    Next ! {self(), test, TestMe},
    receive {Next, ack, TestMe} -> {Index, Next} end.


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
counter(Controller, Next, Index, Counter, Max, ReportFun) when Counter < Max ->
    receive
        {Someone, done} ->
            done_next(Next),
            Controller ! {self(), done},
            Someone ! {self, done}
        after 0 -> true
    end,
    {NewIndex, NewNext} = test_next(Next, Index, Counter, Max, ReportFun),
    counter(Controller, NewNext, NewIndex, Counter+1, Max, ReportFun);
counter(Controller, Next, _Index, _Counter, _Max, _ReportFun) ->
    done_next(Next),
    Controller ! {self(), done}.


%% @spec sieve(Next, N) -> irrelevant_value
%% @doc Sieve filtering out all multiples of N, passing on all other numbers.
sieve(Next, Index, N, Max, ReportFun) ->
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
	    sieve(Next, Index, N, Max, ReportFun);
	{Sender, test, TestMe} ->
	    %% TestMe may be a prime number. Delegate testing...
	    {NewIndex, NewNext} = test_next(Next, Index, TestMe, Max, ReportFun),
	    %% ...and signal sender that we are done with this number...
	    Sender ! {self(), ack, TestMe},
	    %% ...and wait for the next prime candiate.
	    sieve(NewNext, NewIndex, N, Max, ReportFun)
    end.


%% @spec receive(Index, PrimeList, Count) -> whatever
%% @doc Process accumulating a list of primes.
receiver(Index, PrimeList, Count, Controller) ->
    receive
        {Pid, prime, I, P} ->
            Pid ! {self(), ack, I, P},
            if
                Index >= Count ->
                    Controller ! {self(), done},
                    receive {Controller, done} -> done end;
                true -> ok
            end,
            receiver(Index+1, [P|PrimeList], Count, Controller);
        {Pid, prime_list} ->
            Pid ! {self(), prime_list, Index, PrimeList}
    end.


%% @spec primelist(Count) -> list()
%% @doc Return list of the first Count prime numbers.
primelist(Count) when is_integer(Count), Count > 0 ->
    Receiver = spawn_link(?MODULE, receiver, [0, [], Count, counter]),
    ReportFun = fun(I,P) ->
                   Receiver ! {self(), prime, I, P},
                   receive {Receiver, ack, I, P} -> ok end
                end,
    start(ReportFun),
    Receiver ! {self(), prime_list},
    receive
        {Receiver, prime_list, _Index, PrimeList} ->
            PrimeList
    end.


%% trace_primelist(Count) ->
%%     dbg:tracer(),
%%     lists:foreach(fun(F) ->
%%         dbg:tpl(?MODULE, F, '_', dbg:fun2ms(fun(_) -> return_trace() end))
%%     end, [start, primelist, trace_primelist,
%%           counter, sieve, receiver,
%%           done_next, test_next]),
%%     dbg:p(all, [c]),
%%     primelist(Count).


%% @spec default_report(Index, Prime) -> ok
%% @doc Default prime reporting function, printing both index and prime.
default_report(Index, Prime) when is_integer(Index), is_integer(Prime) ->
    io:format("~p ~p~n", [Index, Prime]).


%% @spec start(Max) -> done
%% @doc Start generating prime numbers smaller than <tt>Max</tt>.
start(Max) when is_integer(Max) ->
    start(Max, fun default_report/2);
start(ReportFun) when is_function(ReportFun) ->
    start(?MAX_VALUE, ReportFun).


%% @spec start() -> done
%% @doc Start generating prime numbers smaller than a default value.
start() ->
    start(?MAX_VALUE, fun default_report/2).


%% The actual implementation.
start(Max, ReportFun) when is_integer(Max), is_function(ReportFun) ->
    ok = ReportFun(0,2),
    Sieve2 = spawn_link(?MODULE, sieve, [none, 1, 2, Max, ReportFun]),
    Counter = spawn_link(?MODULE, counter, [self(), Sieve2, 0, 2, Max, ReportFun]),
    register(counter, Counter), % FIXME: race condition!
    receive
	{Counter, done} ->
	    done
    end.

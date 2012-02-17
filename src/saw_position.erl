%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(saw_position).
-define(DELAY, 2 * 1000 div 256).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([nulldurchlauf/2, change_offset/1]).

%% ====================================================================
%% External functions
%% ====================================================================
nulldurchlauf(T_abs, Durchlaufzeit) ->
    gen_server:cast(?MODULE, {nulldurchlauf, T_abs, Durchlaufzeit}).

change_offset(Offset) ->
    gen_server:cast(?MODULE, {change_offset, Offset}).


%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {time, col, offset, run_state}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
	start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{time={0,0}, col={64, up}, offset=0, run_state=ready}, 0}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Msg, _From, State) ->
	{reply, ok, State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({nulldurchlauf, T_abs, Durchlaufzeit}, #state{time={TAbs, TRun}, col={Col, Direction}, offset=Offset, run_state=RunState}=State) ->
    NewState = case RunState of
		  running ->
		      State#state{time={T_abs, Durchlaufzeit}, col={64 + Offset, up}};
		  ready ->
		       {NextCol, NextDirection, Delay} = find_next_column(64 + Offset, up, Durchlaufzeit, T_abs),
		       erlang:send_after(Delay, ?MODULE, column_changed),
		       State#state{time={T_abs, Durchlaufzeit}, col={NextCol, NextDirection}, run_state=running};
		  stopped ->
		      State
    end,
%%    error_logger:info_msg("start: col 64 up", []),
    {noreply, NewState};
handle_cast({change_offset, Offset}, State) ->
    {noreply, State#state{offset=Offset}};

handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(column_changed, #state{time={TAbs, TRun}, col={Col, Direction}, offset=Offset, run_state=RunState}=State) ->
    {NextCol, NextDirection, Delay} = find_next_column(Col, Direction, TRun, TAbs),
    erlang:send_after(Delay, ?MODULE, column_changed),
%    error_logger:info_msg("Current col:~p", [NextCol]),
    saw_sliding_w:print(Col),
    {noreply, State#state{col={NextCol, NextDirection}}};

handle_info(Msg, State) ->	
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
send_message() ->
	ok.

next_column(0, down) ->
    {0, up};
next_column(127, up) ->
    {127, down};
next_column(N, down) ->
    {N-1, down};
next_column(N, up) ->
    {N+1, up}.

find_next_column(Column, Direction, TRun, TAbs) ->
    {NextCol, NextDirection} = next_column(Column, Direction),
    Delay = delay(NextCol, NextDirection, TRun, TAbs),
    if
	Delay >= 0 ->
	    {NextCol, NextDirection, Delay};
	true ->
	    error_logger:info_msg("Skipping column ~p, ~p", [NextCol, NextDirection]),
	    find_next_column(NextCol, NextDirection, TRun, TAbs)
    end.


delay(Durchlaufzeit) ->
    Delay = (Durchlaufzeit div 256) div 1000,
 %%   error_logger:info_msg("Durchlaufzeit ~p, Delay ~p", [Durchlaufzeit, Delay]),
	Delay.

delay(Index, Direction, Durchlaufzeit, T_abs) ->
    T_diff = timer:now_diff(erlang:now(), T_abs),
    T_ist = T_diff rem Durchlaufzeit,
    T_soll = erlang:round(winkel((Index / 63.5 - 1), Direction, Durchlaufzeit) * Durchlaufzeit / (2 * math:pi())),
%    error_logger:info_msg("Index: ~p, Direction: ~p, T_ist: ~p, T_soll: ~p", [Index, Direction, T_ist, T_soll]),
    (T_soll - T_ist) div 1000.

winkel(Y , down, Durchlaufzeit) when Y < 0 ->	
	math:pi() - math:asin(Y);
winkel(Y, down, Durchlaufzeit) ->	
	math:pi() - math:asin(Y);
winkel(Y, up, Durchlaufzeit) when Y < 0 ->	
	math:asin(Y) + (2 * math:pi());
winkel(Y, up, Durchlaufzeit) ->	
	math:asin(Y).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
winkel_test() ->
	?assertEqual(0.0 ,winkel(0, up, 2000)),
	?assertEqual(erlang:round(math:pi() /4 * 10000) , erlang:round(winkel(math:sin(math:pi() / 4) , up, 2000) * 10000)),
	?assertEqual(math:pi() / 2 ,winkel(1, up, 2000)),
 	?assertEqual(math:pi() / 2 ,winkel(1, down, 2000)),
	?assertEqual(erlang:round(3 * math:pi() /4 * 10000) , erlang:round(winkel(math:sin(3 * math:pi() / 4) , down, 2000) * 10000)),
	?assertEqual(math:pi() ,winkel(0, down, 2000)),
	?assertEqual(erlang:round(5 * math:pi() /4 * 10000) , erlang:round(winkel(math:sin(5 * math:pi() / 4) , down, 2000) * 10000)),
	?assertEqual(3 * math:pi() / 2 ,winkel(-1, down, 2000)),
	?assertEqual(3 * math:pi() / 2 ,winkel(-1, up, 2000)),	
	?assertEqual(erlang:round(7 * math:pi() /4 * 10000) , erlang:round(winkel(math:sin(7 * math:pi() / 4) , up, 2000) * 10000)).
	
-endif.

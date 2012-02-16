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
-export([nulldurchlauf/2]).

%% ====================================================================
%% External functions
%% ====================================================================
nulldurchlauf(T_abs, Durchlaufzeit) ->
    gen_server:cast(?MODULE, {nulldurchlauf, T_abs, Durchlaufzeit}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {t_abs, durchlaufzeit}).
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
    {ok, #state{}, 0}.

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
handle_cast({nulldurchlauf, T_abs, Durchlaufzeit}, State) ->
    TimerRef = erlang:send_after(?DELAY, ?MODULE, {col, 64, up}),
%%    error_logger:info_msg("start: col 64 up", []),
    {noreply, #state{t_abs=T_abs, durchlaufzeit=Durchlaufzeit}};

handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info({col, 127, up}, State) ->
    erlang:send_after(delay(State#state.durchlaufzeit), ?MODULE, {col, 127, down}),
%    error_logger:info_msg("col 127 up", []),
    saw_sliding_w:print(127),
    {noreply, State};

handle_info({col, No, up}, State) ->
    erlang:send_after(delay(State#state.durchlaufzeit), ?MODULE, {col, No+1, up}),
%    error_logger:info_msg("col ~p up", [No]),
    saw_sliding_w:print(No),
    {noreply, State};

handle_info({col, 0, down}, State) ->
    erlang:send_after(delay(State#state.durchlaufzeit), ?MODULE, {col, 0, up}),
%    error_logger:info_msg("col 0 down", []),
    saw_sliding_w:print(0),
    {noreply, State};

handle_info({col, No, down}, State) ->
    erlang:send_after(delay(State#state.durchlaufzeit), ?MODULE, {col, No-1, down}),
%    error_logger:info_msg("col ~p down", [No]),
    saw_sliding_w:print(No),
    {noreply, State};

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

col(No, Direction) ->
    gen_server:cast(?MODULE, {col, No, Direction}).

delay(Durchlaufzeit) ->
    Delay = (Durchlaufzeit div 256) div 1000,
    error_log:info_msg("Durchlaufzeit ~p, Delay ~p", [Durchlaufzeit, Delay]).

delay(Index, Direction, Durchlaufzeit, T_abs) ->
    T_diff = timer:now_diff(erlang:now(), T_abs),
    T_ist = T_diff rem Durchlaufzeit,
    T_soll = sollDelay(),
    (T_soll - T_ist) div 1000.

sollDelay() ->
     ok.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.

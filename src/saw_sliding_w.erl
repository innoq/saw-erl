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
-module(saw_sliding_w).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, send_message_test/0]).
-export([start_link/0]).
-export([start/0]).
-export([print/1, set_content/1, scroll/0]).

-define(PORT, 13664).
-define(HOST, {127,0,0,1}).

%% ====================================================================
%% External functions
%% ====================================================================
print(Spalten_index) ->
	gen_server:cast(?MODULE, {print, Spalten_index}).

set_content(Content) ->
	gen_server:cast(?MODULE, {set_content, Content}).

scroll() ->
	gen_server:cast(?MODULE, {scroll}).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {	contentraw = "Das ist der default",
					content,
					contentlength,
			   		sawsock}).
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
	{ok, Socket} = gen_udp:open(13000),
	{ContentLength, Content} = prepare_dummy_array(),
    {ok, #state{sawsock=Socket, content=Content, contentlength=ContentLength}, 0}.

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
handle_cast({print, Spalten_index}, State) ->	
	send_message(State, Spalten_index),
    {noreply, State};

handle_cast({scroll}, State) ->		
    {noreply, State};

handle_cast({set_content, Content}, State) ->	
	{noreply, State#state{content = Content, contentlength = erlang:length(Content)}}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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
send_message(State, Spalten_index) ->
	ByteIndex = Spalten_index rem State#state.contentlength,
	Byte = lists:nth(ByteIndex+1, State#state.content),
	
	Resp= gen_udp:send(State#state.sawsock, ?HOST, ?PORT, <<Byte:8>>),
%%	io:format("ByteIndex: ~p => ~p => ~p~n", [ByteIndex, Byte, Resp]),
	ok.

prepare_dummy_array() ->
	List = [1,2,4,8,16,32,64,128,64,32,16,8,4,2],
	{length(List), List}.

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------

	
send_message_thelper([], _) -> ok;
send_message_thelper([H|T], State) -> 
	send_message(State, H),
	send_message_thelper(T, State).
	
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

send_message_test() ->
	{ok, State, _} = init([]),
	%%io:format("~n~p_~p~n", [State#state.content, State#state.contentlength]),
	Seq = lists:seq(1, 100000),
	send_message_thelper(Seq, State),
	gen_udp:close(State#state.sawsock).
-endif.
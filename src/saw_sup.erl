%% Copyright 2012 Ulf Angermann
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
%%% Created : 19.11.2011
%%% -------------------------------------------------------------------
-module(saw_sup).
-behaviour(supervisor).

-define(STAKT, {saw_taktgeber, {saw_taktgeber, start_link, []}, transient, 10000, worker, []}).
-define(SPOS, {saw_position, {saw_position, start_link, []}, transient, 10000, worker, []}).
-define(SWIN, {saw_sliding_w, {saw_sliding_w, start_link, []}, transient, 10000, worker, []}).
-define(SWINTAKT, {saw_sliding_w_takt, {saw_sliding_w_takt, start_link, []}, transient, 10000, worker, []}).
-define(SINPUT, {saw_input, {saw_input, start_link, []}, transient, 10000, worker, []}).
-define(WEBMACHINE(WebConfig), {webmachine_mochiweb, {webmachine_mochiweb, start, [WebConfig]}, permanent, 5000, worker, dynamic}).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0, start/0]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
	start_link().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
	Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
	{ok, Dispatch} = file:consult("./priv/dispatch.conf"),	
	WebConfig = [
                 {ip, Ip},				
                 {backlog, 1000},
                 {port, 8080 },
                 {log_dir, "log/weblog"},
                 {dispatch, Dispatch}],
	MaxRestart = 1,
    MaxTime = 10000,
    {ok, {{one_for_one, MaxRestart, MaxTime}, [?STAKT, ?SPOS, ?SWIN, ?SWINTAKT, ?SINPUT, ?WEBMACHINE(WebConfig)]}}.
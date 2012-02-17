-module(saw_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
	ensure_started(crypto),
    ensure_started(webmachine),
	ensure_started(?MODULE).

start(_StartType, _StartArgs) ->
	ensure_started(crypto),
    ensure_started(webmachine),
	saw_sup:start().

stop(_State) ->	
    ok.
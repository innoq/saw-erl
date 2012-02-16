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
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(saw_content_resource).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1,  allowed_methods/2, resource_exists/2]).
-include_lib("../deps/webmachine/include/webmachine.hrl").
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(context, {}).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
init(_Config) -> 
	{{trace, "/tmp"}, #context{}}.
	%%{ok, #context{}}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

to_html(ReqData, Context) ->
     {ok, Content} = about_dtl:render([{version, wurfler:getVersion()}]),
     {Content, ReqData, Context}.

resource_exists(ReqData, Context) ->
	{true, ReqData, Context}.
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
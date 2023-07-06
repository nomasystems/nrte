%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
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
%% limitations under the License
-module(nrte).
-behaviour(application).

%%% APPLICATION EXPORTS
-export([start/2, stop/1]).

%%%-----------------------------------------------------------------------------
%%% APPLICATION EXPORTS
%%%-----------------------------------------------------------------------------
start(_, _) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, nrte, "tester.html"}},
            {"/eventsource", nrte_eventsource, []},
            {"/websocket", nrte_websocket, []},
            {"/[...]", nrte_rest, []}
        ]}
    ]),
    cowboy:start_clear(nrte_listener, [{port, 2080}], #{
        enable_connect_protocol => true, env => #{dispatch => Dispatch}
    }),
    nrte_sup:start_link().

stop(_) ->
    cowboy:stop_listener(nrte_listener),
    ok.

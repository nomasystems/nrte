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

%%% EXTERNAL EXPORTS
-export([publish/2, subscribe/1]).

%%%-----------------------------------------------------------------------------
%%% APPLICATION EXPORTS
%%%-----------------------------------------------------------------------------
start(_, _) ->
    BaseRoutes = [
        {"/eventsource", nrte_eventsource, []},
        {"/websocket", nrte_websocket, #{}},
        {"/[...]", nrte_rest, []}
    ],
    Routes =
        case nrte_conf:serve_priv_dir() of
            true -> [{"/priv/[...]", cowboy_static, {priv_dir, nrte, ""}} | BaseRoutes];
            _ -> BaseRoutes
        end,
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    cowboy:start_clear(nrte_listener, [{port, nrte_conf:port()}], #{
        enable_connect_protocol => true, env => #{dispatch => Dispatch}
    }),
    nrte_sup:start_link().

stop(_) ->
    cowboy:stop_listener(nrte_listener),
    ok.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec publish(binary(), binary()) -> ok.
publish(Topic, Message) ->
    ExpandedTopics = expand_topic(Topic),
    lists:foreach(fun(T) -> ebus:pub(T, Message) end, ExpandedTopics).

-spec subscribe([iodata()]) -> ok.
subscribe(TopicList) ->
    nrte_ebus_handler:subscribe(TopicList).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
expand_topic(Topic) ->
    Subtopics = [binary:part(Topic, {0, Pos}) || {Pos, _} <- binary:matches(Topic, <<":">>)],
    [Topic | Subtopics].

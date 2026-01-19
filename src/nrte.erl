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

%%% START/STOP SERVER EXPORTS
-export([start_listening/0, start_listening/1, stop_listening/0, stop_listening/1]).

%%% EXTERNAL EXPORTS
-export([publish/2, subscribe/1]).

%%% TYPES
-type opts() :: #{
    auth_type => {always_allow, nrte_auth:nrte_auth_value()} | {auth_mod, atom()},
    name => atom(),
    port => pos_integer(),
    serve_priv_dir => boolean()
}.

%%%-----------------------------------------------------------------------------
%%% APPLICATION EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_listening() -> {ok, pid()} | {error, any()}.
start_listening() ->
    start_listening(#{}).

-spec start_listening(opts()) -> {ok, pid()} | {error, any()}.
start_listening(Opts) ->
    BaseRoutes = [
        {"/eventsource", nrte_eventsource, Opts},
        {"/websocket", nrte_websocket, Opts},
        {"/[...]", nrte_rest, Opts}
    ],
    Routes =
        case nrte_conf:serve_priv_dir(Opts) of
            true -> [{"/priv/[...]", cowboy_static, {priv_dir, nrte, ""}} | BaseRoutes];
            _ -> BaseRoutes
        end,
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    cowboy:start_clear(nrte_conf:name(Opts), [{port, nrte_conf:port(Opts)}], #{
        enable_connect_protocol => true, env => #{dispatch => Dispatch}
    }).

-spec stop_listening() -> ok.
stop_listening() ->
    stop_listening(nrte_conf:name(#{})).

-spec stop_listening(atom()) -> ok.
stop_listening(Name) ->
    cowboy:stop_listener(Name),
    ok.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec publish(binary(), binary()) -> ok.
publish(Topic, Message) ->
    ExpandedTopics = expand_topic(Topic),
    lists:foreach(fun(T) -> ebus:pub(T, {Topic, Message}) end, ExpandedTopics).

-spec subscribe([iodata()]) -> ok.
subscribe(TopicList) ->
    nrte_ebus_handler:subscribe(TopicList).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
expand_topic(Topic) ->
    Subtopics = [binary:part(Topic, {0, Pos}) || {Pos, _} <- binary:matches(Topic, <<":">>)],
    [Topic | Subtopics].

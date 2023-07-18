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
-module(nrte_rest).
-behaviour(cowboy_handler).

%%% COWBOY HANDLER EXPORTS
-export([init/2]).

%%%-----------------------------------------------------------------------------
%%% COWBOY HANDLER EXPORTS
%%%-----------------------------------------------------------------------------
init(Req, Opts) ->
    case nrte_auth:authorization(Req, publish) of
        {authorized, TopicList} ->
            init_authorized(Req, Opts, TopicList);
        {unauthorized, Req2} ->
            {stop, Req2, Opts}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
init_authorized(#{path := <<"/message">>} = Req, Opts, [<<>>]) ->
    {stop, cowboy_req:reply(400, #{}, <<"Topics required">>, Req), Opts};
init_authorized(#{path := <<"/message">>} = Req, Opts, TopicList) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    lists:foreach(fun(T) -> nrte:publish(T, Body) end, TopicList),
    {ok, cowboy_req:reply(200, #{}, <<>>, Req2), Opts};
init_authorized(Req, Opts, _TopicList) ->
    {stop, cowboy_req:reply(404, #{}, <<>>, Req), Opts}.

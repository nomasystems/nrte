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
-module(nrte_eventsource).
-behaviour(cowboy_loop).

%%% COWBOY LOOP EXPORTS
-export([init/2, info/3]).

%%% MACROS
-define(CONTENT_TYPE, <<"text/event-stream">>).

%%%-----------------------------------------------------------------------------
%%% COWBOY LOOP EXPORTS
%%%-----------------------------------------------------------------------------
init(Req, Opts) ->
    case nrte_auth:authorization(Req, Opts, subscribe) of
        {authorized, TopicList} ->
            nrte:subscribe(TopicList),
            nrte_publications:subscription_init_link_terminate(<<"eventsource">>, TopicList),
            Req2 = cowboy_req:stream_reply(200, #{<<"content-type">> => ?CONTENT_TYPE}, Req),
            {cowboy_loop, Req2, Opts};
        {unauthorized, Req2} ->
            {stop, Req2, Opts}
    end.

info({nrte_message, Data}, Req, Opts) ->
    ok = cowboy_req:stream_events(#{data => Data}, nofin, Req),
    {ok, Req, Opts}.

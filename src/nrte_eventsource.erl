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

%%% CALLBACK EXPORTS
-export([handle_ebus_message/3]).

%%%-----------------------------------------------------------------------------
%%% COWBOY LOOP EXPORTS
%%%-----------------------------------------------------------------------------
init(Req, Opts) ->
    case nrte_auth:is_authorized(Req) of
        true -> start_loop(Req, Opts);
        false -> {ok, cowboy_req:reply(401, #{}, <<>>, Req), Opts}
    end.

info({ebus_message, {Topic, Message}}, Req, Opts) ->
    Data = [Topic, ";", Message],
    ok = cowboy_req:stream_events(#{data => Data}, nofin, Req),
    {ok, Req, Opts}.

%%%-----------------------------------------------------------------------------
%%% CALLBACK EXPORTS
%%%-----------------------------------------------------------------------------
handle_ebus_message(Message, Topic, Pid) ->
    Pid ! {ebus_message, {Topic, Message}}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
start_loop(Req, Opts) ->
    Topics = proplists:get_value(<<"topics">>, cowboy_req:parse_qs(Req)),
    TopicList = binary:split(Topics, <<";">>, [global]),
    lists:foreach(
        fun(T) ->
            Handler = ebus_proc:spawn_handler(fun ?MODULE:handle_ebus_message/3, [T, self()], [link]),
            ebus:sub(Handler, T)
        end,
        TopicList
    ),
    Req2 = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>}, Req),
    {cowboy_loop, Req2, Opts}.

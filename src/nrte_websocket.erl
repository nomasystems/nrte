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
-module(nrte_websocket).
-behaviour(cowboy_websocket).

%%% COWBOY WEBSOCKET EXPORTS
-export([init/2, websocket_handle/2, websocket_info/2]).

%%%-----------------------------------------------------------------------------
%%% COWBOY WEBSOCKET EXPORTS
%%%-----------------------------------------------------------------------------
init(Req, Opts) ->
    case nrte_auth:is_authorized(Req) of
        true -> {cowboy_websocket, Req, []};
        false -> {ok, cowboy_req:reply(401, #{}, <<>>, Req), Opts}
    end.

websocket_handle({text, <<"topics: ", Topics/binary>>}, State) ->
    TopicList = binary:split(Topics, <<";">>, [global]),
    lists:foreach(
        fun(T) ->
            Handler = nrte_ebus_handler:spawn(T, self()),
            ebus:sub(Handler, T)
        end,
        TopicList
    ),
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({ebus_message, Data}, State) ->
    {reply, {text, Data}, State};
websocket_info(_Info, State) ->
    {ok, State}.

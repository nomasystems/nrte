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

%%% INCLUDE FILES

%%% COWBOY WEBSOCKET EXPORTS
-export([init/2, websocket_handle/2, websocket_info/2]).

%%% CALLBACK EXPORTS
-export([handle_ebus_message/3]).

%%% EXTERNAL EXPORTS
-export([]).

%%% MACROS

%%% RECORDS
-record(st, {user :: undefined | binary()}).

%%%-----------------------------------------------------------------------------
%%% COWBOY WEBSOCKET EXPORTS
%%%-----------------------------------------------------------------------------
init(Req, _Opts) ->
    {cowboy_websocket, Req, #st{}}.

websocket_handle({text, <<"authorization: ", User/binary>>}, #st{user = undefined} = State) ->
    {ok, State#st{user = User}};
websocket_handle(_Data, #st{user = undefined}) ->
    {stop, auth_needed};
websocket_handle({text, <<"topics: ", Topics/binary>>}, State) ->
    TopicList = binary:split(Topics, <<";">>, [global]),
    lists:foreach(
        fun(T) ->
            Handler = ebus_proc:spawn_handler(fun ?MODULE:handle_ebus_message/3, [T, self()], [link]),
            ebus:sub(Handler, T)
        end,
        TopicList
    ),
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({ebus_message, {Topic, Message}}, State) ->
    Data = [Topic, ";", Message],
    {reply, {text, Data}, State};
websocket_info(_Info, State) ->
    {ok, State}.

%%%-----------------------------------------------------------------------------
%%% CALLBACK EXPORTS
%%%-----------------------------------------------------------------------------
handle_ebus_message(Message, Topic, Pid) ->
    Pid ! {ebus_message, {Topic, Message}}.

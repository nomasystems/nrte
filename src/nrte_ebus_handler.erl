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
-module(nrte_ebus_handler).

%%% EXTERNAL EXPORTS
-export([subscribe/1]).

%%% CALLBACK EXPORTS
-export([handle_message/3]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
subscribe(TopicList) ->
    lists:foreach(
        fun(Topic) ->
            Handler = ebus_proc:spawn_handler(fun ?MODULE:handle_message/3, [Topic, self()], [link]),
            ebus:sub(Handler, Topic)
        end,
        TopicList
    ).

%%%-----------------------------------------------------------------------------
%%% CALLBACK EXPORTS
%%%-----------------------------------------------------------------------------
handle_message(Message, Topic, Pid) ->
    Template = nrte_conf:data_template(),
    Replacements = [{<<"{{message}}">>, Message}, {<<"{{topic}}">>, Topic}],
    Data = lists:foldl(
        fun({Original, Replacement}, Acc) ->
            binary:replace(Acc, Original, Replacement, [global])
        end,
        Template,
        Replacements
    ),
    Pid ! {ebus_message, Data}.

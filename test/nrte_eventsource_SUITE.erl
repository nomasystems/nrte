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
-module(nrte_eventsource_SUITE).

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [connect, connect_unauthorized, receive_topic_message].

suite() ->
    [{timetrap, {seconds, 60}}].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
connect() ->
    [{userdata, [{doc, "Tests connecting to the websocket"}]}].

connect(_Conf) ->
    {ok, {Pid, _StreamRef}} = connect_es("topic1;topic2;topic3"),
    ok = gun:close(Pid).

connect_unauthorized() ->
    [{userdata, [{doc, "Tests unauthorized connecting to the websocket"}]}].

connect_unauthorized(_Conf) ->
    application:set_env(nrte, auth_type, {always, false}),
    unauthorized = connect_es("topic1;topic2;topic3"),
    application:unset_env(nrte, auth_type),
    ok.

receive_topic_message() ->
    [{userdata, [{doc, "Tests receiving a published topic message"}]}].

receive_topic_message(_Conf) ->
    {ok, {Pid, StreamRef}} = connect_es("topic"),
    ebus:pub(<<"topic">>, <<"message">>),
    {sse, #{data := [<<"topic;message">>]}} = gun:await(Pid, StreamRef, 1000),
    ok = gun:close(Pid).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
connect_es(Topics) ->
    Protocol = http2,
    {ok, Pid} = gun:open("localhost", 2080, #{
        protocols => [Protocol],
        http2_opts => #{content_handlers => [gun_sse_h, gun_data_h]}
    }),
    {ok, Protocol} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/eventsource?topics=" ++ Topics, [
        {<<"accept">>, <<"text/event-stream">>}
    ]),
    case gun:await(Pid, StreamRef, 1000) of
        {response, nofin, 200, Headers} ->
            {_, <<"text/event-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
            {ok, {Pid, StreamRef}};
        {response, fin, 401, _} ->
            unauthorized
    end.

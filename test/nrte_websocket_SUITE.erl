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
-module(nrte_websocket_SUITE).

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        connect,
        connect_unauthorized,
        coverage_completion,
        receive_topic_message,
        send_text,
        send_topics,
        send_unsupported_text
    ].

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
    {ok, {Pid, _StreamRef}} = connect_ws(),
    ok = gun:close(Pid).

connect_unauthorized() ->
    [{userdata, [{doc, "Tests connecting to the websocket without authorization"}]}].

connect_unauthorized(_Conf) ->
    application:set_env(nrte, auth_type, {always, false}),
    unauthorized = connect_ws(),
    application:unset_env(nrte, auth_type),
    ok.

coverage_completion() ->
    [{userdata, [{doc, "Ensure coverage completion by sending a direct info message"}]}].

coverage_completion(_Conf) ->
    _ = nrte_websocket:websocket_info(dummy, state).

receive_topic_message() ->
    [{userdata, [{doc, "Tests receiving a published topic message"}]}].

receive_topic_message(_Conf) ->
    {ok, {Pid, StreamRef}} = connect_ws(),
    ok = gun:ws_send(Pid, StreamRef, {text, <<"authorization: user:password">>}),
    ok = gun:ws_send(Pid, StreamRef, {text, <<"topics: topic">>}),
    % Clear the messages
    ok = gun:flush(StreamRef),
    ok = gun:ws_send(Pid, StreamRef, ping),
    % Wait for pong to ensure the server processed the topic subscription
    {ws, pong} = gun:await(Pid, StreamRef),
    ebus:pub(<<"topic">>, <<"message">>),
    {ws, {text, <<"topic;message">>}} = gun:await(Pid, StreamRef),
    ok = gun:close(Pid).

send_authorization() ->
    [{userdata, [{doc, "Tests sending the authorization"}]}].

send_authorization(_Conf) ->
    {ok, {Pid, StreamRef}} = connect_ws(),
    ok = gun:ws_send(Pid, StreamRef, {text, <<"authorization: user:password">>}),
    ok = gun:close(Pid).

send_text() ->
    [{userdata, [{doc, "Tests sending an unsupported text"}]}].

send_text(_Conf) ->
    {ok, {Pid, StreamRef}} = connect_ws(),
    ok = gun:ws_send(Pid, StreamRef, {text, <<"unsupported message">>}),
    ok = gun:close(Pid).

send_topics() ->
    [{userdata, [{doc, "Tests sending topics"}]}].

send_topics(_Conf) ->
    {ok, {Pid, StreamRef}} = connect_ws(),
    ok = gun:ws_send(Pid, StreamRef, {text, <<"topics: topic1;topic2">>}),
    ok = gun:close(Pid).

send_unsupported_text() ->
    [{userdata, [{doc, "Tests sending an unsupported text"}]}].

send_unsupported_text(_Conf) ->
    {ok, {Pid, StreamRef}} = connect_ws(),
    ok = gun:ws_send(Pid, StreamRef, {text, <<"unsupported text">>}),
    ok = gun:close(Pid).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
connect_ws() ->
    Protocol = http2,
    {ok, Pid} = gun:open("localhost", 2080, #{
        ws_opts => #{silence_pings => false},
        protocols => [Protocol],
        http2_opts => #{notify_settings_changed => true}
    }),
    {ok, Protocol} = gun:await_up(Pid),
    ok = do_await_enable_connect_protocol(Protocol, Pid),
    StreamRef = gun:ws_upgrade(Pid, "/websocket", []),
    case gun:await(Pid, StreamRef) of
        {upgrade, [<<"websocket">>], _} -> {ok, {Pid, StreamRef}};
        {response, fin, 401, _} -> unauthorized
    end.

do_await_enable_connect_protocol(http2, Pid) ->
    % We cannot do a CONNECT :protocol request until the server tells us we can.
    {notify, settings_changed, #{enable_connect_protocol := true}} = gun:await(Pid, undefined),
    ok.

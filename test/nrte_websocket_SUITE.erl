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
-behaviour(nrte_auth).

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).

%%% MACROS
-define(TOPIC, "topic1").

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        connect,
        connect_authorized_explicitly,
        connect_forbidden_subscription,
        connect_unauthorized,
        coverage_completion,
        receive_topic_message,
        send_text,
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

connect_authorized_explicitly() ->
    [{userdata, [{doc, "Tests connecting to the websocket with explicit authorization"}]}].

connect_authorized_explicitly(_Conf) ->
    application:set_env(nrte, auth_type, {auth_mod, ?MODULE}),
    {ok, {Pid, _StreamRef}} = connect_ws(),
    application:unset_env(nrte, auth_type),
    ok = gun:close(Pid).

connect_forbidden_subscription() ->
    [{userdata, [{doc, "Tests connecting to the websocket without subscription authorization"}]}].

connect_forbidden_subscription(_Conf) ->
    application:set_env(nrte, auth_type, {always_allow, #{allowed_publications => all}}),
    forbidden = connect_ws(),
    application:unset_env(nrte, auth_type),
    ok.

connect_unauthorized() ->
    [{userdata, [{doc, "Tests connecting to the websocket without authorization"}]}].

connect_unauthorized(_Conf) ->
    application:set_env(nrte, auth_type, {always_allow, unauthorized}),
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
    % Clear the messages
    ok = gun:flush(StreamRef),
    ok = gun:ws_send(Pid, StreamRef, ping),
    % Wait for pong to ensure the server processed the topic subscription
    {ws, pong} = gun:await(Pid, StreamRef),
    ebus:pub(?TOPIC, <<"message">>),
    {ws, {text, <<?TOPIC, ";message">>}} = gun:await(Pid, StreamRef),
    ok = gun:close(Pid).

send_text() ->
    [{userdata, [{doc, "Tests sending an unsupported text"}]}].

send_text(_Conf) ->
    {ok, {Pid, StreamRef}} = connect_ws(),
    ok = gun:ws_send(Pid, StreamRef, {text, <<"unsupported message">>}),
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
    StreamRef = gun:ws_upgrade(Pid, "/websocket?topics=" ++ ?TOPIC, []),
    case gun:await(Pid, StreamRef) of
        {upgrade, [<<"websocket">>], _} -> {ok, {Pid, StreamRef}};
        {response, fin, 401, _} -> unauthorized;
        {response, _, 403, _} -> forbidden
    end.

do_await_enable_connect_protocol(http2, Pid) ->
    % We cannot do a CONNECT :protocol request until the server tells us we can.
    {notify, settings_changed, #{enable_connect_protocol := true}} = gun:await(Pid, undefined),
    ok.

nrte_auth(_) ->
    #{allowed_subscriptions => [<<?TOPIC>>]}.

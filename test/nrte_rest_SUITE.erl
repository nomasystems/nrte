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
-module(nrte_rest_SUITE).

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).

%%% MACROS
-define(TEXT, <<"text">>).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        get_auth_cookie,
        get_auth_cookie_via_auth_cookie,
        get_auth_cookie_via_auth_cookie_expired,
        get_auth_cookie_via_auth_cookie_used,
        get_auth_cookie_via_auth_mod,
        get_auth_cookie_via_auth_mod_no_auth_header,
        get_unauthorized,
        get_undocumented_path,
        post_empty_body,
        post_topic_message,
        post_topic_with_subtopics
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
get_auth_cookie() ->
    [{userdata, [{doc, "Tests getting an auth cookie"}]}].

get_auth_cookie(_Conf) ->
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/auth"),
    {response, fin, 200, Headers} = gun:await(Pid, StreamRef, 1000),
    {<<"set-cookie">>, <<"nrte_auth=", _/binary>>} = lists:keyfind(<<"set-cookie">>, 1, Headers),
    ok.

get_auth_cookie_via_auth_cookie() ->
    [{userdata, [{doc, "Tests getting an auth cookie with an auth cookie"}]}].

get_auth_cookie_via_auth_cookie(_Conf) ->
    application:set_env(nrte, auth_type, {?MODULE, auth_fun}),
    {ok, Pid} = gun:open("localhost", 2080, #{cookie_store => gun_cookies_list:init(#{})}),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/auth", [{<<"authorization">>, <<"basic-auth">>}]),
    {response, fin, 200, _} = gun:await(Pid, StreamRef, 1000),
    StreamRef2 = gun:get(Pid, "/auth"),
    {response, fin, 200, _} = gun:await(Pid, StreamRef2, 1000),
    application:unset_env(nrte, auth_type),
    ok.

get_auth_cookie_via_auth_cookie_expired() ->
    [{userdata, [{doc, "Tests not getting an auth cookie with an expired auth cookie"}]}].

get_auth_cookie_via_auth_cookie_expired(_Conf) ->
    application:set_env(nrte, auth_type, {?MODULE, auth_fun}),
    application:set_env(nrte, token_cleanup_seconds, 0),
    application:set_env(nrte, token_expiration_seconds, 0),
    % Ensure the nrte_auth takes the new cleanup value instantly
    ok = gen_server:stop(nrte_auth),
    {ok, Pid} = gun:open("localhost", 2080, #{cookie_store => gun_cookies_list:init(#{})}),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/auth", [{<<"authorization">>, <<"basic-auth">>}]),
    {response, fin, 200, _} = gun:await(Pid, StreamRef, 1000),
    StreamRef3 = gun:get(Pid, "/auth"),
    {response, fin, 401, _} = gun:await(Pid, StreamRef3, 1000),
    application:unset_env(nrte, token_cleanup_seconds),
    application:unset_env(nrte, auth_type),
    application:unset_env(nrte, token_expiration_seconds),
    ok.

get_auth_cookie_via_auth_cookie_used() ->
    [{userdata, [{doc, "Tests not getting an auth cookie with an used auth cookie"}]}].

get_auth_cookie_via_auth_cookie_used(_Conf) ->
    application:set_env(nrte, auth_type, {?MODULE, auth_fun}),
    {ok, Pid} = gun:open("localhost", 2080, #{cookie_store => gun_cookies_list:init(#{})}),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/auth", [{<<"authorization">>, <<"basic-auth">>}]),
    {response, fin, 200, _} = gun:await(Pid, StreamRef, 1000),
    StreamRef2 = gun:get(Pid, "/consume-cookie"),
    {response, fin, _, _} = gun:await(Pid, StreamRef2, 1000),
    StreamRef3 = gun:get(Pid, "/auth"),
    {response, fin, 401, _} = gun:await(Pid, StreamRef3, 1000),
    application:unset_env(nrte, auth_type),
    ok.

get_auth_cookie_via_auth_mod() ->
    [{userdata, [{doc, "Tests getting an auth cookie with an auth mod configured"}]}].

get_auth_cookie_via_auth_mod(_Conf) ->
    application:set_env(nrte, auth_type, {?MODULE, auth_fun}),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/auth", [{<<"authorization">>, <<"basic-auth">>}]),
    {response, fin, 200, _} = gun:await(Pid, StreamRef, 1000),
    application:unset_env(nrte, auth_type),
    ok.

get_auth_cookie_via_auth_mod_no_auth_header() ->
    [{userdata, [{doc, "Tests not getting an auth cookie with an auth mod configured"}]}].

get_auth_cookie_via_auth_mod_no_auth_header(_Conf) ->
    application:set_env(nrte, auth_type, {?MODULE, auth_fun}),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/auth"),
    {response, fin, 401, _} = gun:await(Pid, StreamRef, 1000),
    application:unset_env(nrte, auth_type),
    ok.

get_unauthorized() ->
    [{userdata, [{doc, "Tests getting an unauthorized path"}]}].

get_unauthorized(_Conf) ->
    application:set_env(nrte, auth_type, {always, false}),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/auth"),
    {response, fin, 401, _Headers} = gun:await(Pid, StreamRef, 1000),
    application:unset_env(nrte, auth_type),
    ok.

get_undocumented_path() ->
    [{userdata, [{doc, "Tests getting an undocumented path"}]}].

get_undocumented_path(_Conf) ->
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/undocumented"),
    {response, fin, 404, _Headers} = gun:await(Pid, StreamRef, 1000),
    ok.

post_empty_body() ->
    [{userdata, [{doc, "Tests posting an empty body"}]}].

post_empty_body(_Conf) ->
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:post(Pid, "/message", #{}, <<>>),
    {response, fin, 400, _Headers} = gun:await(Pid, StreamRef, 1000),
    ok.

post_topic_message() ->
    [{userdata, [{doc, "Tests posting a topic message"}]}].

post_topic_message(_Conf) ->
    ebus:sub(self(), "topic"),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    Body = njson:encode(#{<<"topics">> => [<<"topic">>], <<"message">> => ?TEXT}),
    StreamRef = gun:post(Pid, "/message", #{<<"content-type">> => <<"application/json">>}, Body),
    {response, fin, 200, _Headers} = gun:await(Pid, StreamRef, 1000),
    [?TEXT] = ebus_proc:messages(self()),
    ok = gun:close(Pid).

post_topic_with_subtopics() ->
    [{userdata, [{doc, "Tests posting a topic with subtopics"}]}].

post_topic_with_subtopics(_Conf) ->
    Topic = <<"topic:subtopic:subsubtopic:subsubsubtopic">>,
    ExpectedTopicPubs = [
        Topic, <<"topic:subtopic:subsubtopic">>, <<"topic:subtopic">>, <<"topic">>
    ],
    ok = lists:foreach(fun(T) -> ebus:sub(self(), T) end, ExpectedTopicPubs),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    Body = njson:encode(#{<<"topics">> => [Topic], <<"message">> => ?TEXT}),
    StreamRef = gun:post(Pid, "/message", #{<<"content-type">> => <<"application/json">>}, Body),
    {response, fin, 200, _Headers} = gun:await(Pid, StreamRef, 1000),
    true = length(ExpectedTopicPubs) =:= length(ebus_proc:messages(self())),
    ok = gun:close(Pid).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
auth_fun(_) ->
    true.

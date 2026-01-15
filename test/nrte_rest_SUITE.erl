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
-behaviour(nrte_auth).

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).

%%% MACROS
-define(HEADERS, #{<<"content-type">> => <<"application/json">>}).
-define(TEXT, <<"text">>).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        get_priv_dir,
        get_priv_dir_404,
        get_unauthorized,
        get_unauthorized_via_auth_mod,
        get_undocumented_path,
        get_undocumented_path_via_auth_mod,
        post_empty_body,
        post_no_topic,
        post_topic_all_authorized,
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
get_priv_dir() ->
    [{userdata, [{doc, "Tests getting a file from the priv dir"}]}].

get_priv_dir(_Conf) ->
    application:set_env(nrte, serve_priv_dir, true),
    application:stop(nrte),
    application:start(nrte),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/priv/tester.html"),
    {response, _, 200, _} = gun:await(Pid, StreamRef, 1000),
    application:unset_env(nrte, serve_priv_dir),
    application:stop(nrte),
    application:start(nrte),
    ok.

get_priv_dir_404() ->
    [{userdata, [{doc, "Tests not getting a file from the priv dir with the default conf"}]}].

get_priv_dir_404(_Conf) ->
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/priv/tester.html"),
    {response, _, 404, _} = gun:await(Pid, StreamRef, 1000),
    ok.

get_unauthorized() ->
    [{userdata, [{doc, "Tests getting an unauthorized path"}]}].

get_unauthorized(_Conf) ->
    application:set_env(nrte, auth_type, {always_allow, unauthorized}),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/auth"),
    {response, fin, 401, _Headers} = gun:await(Pid, StreamRef, 1000),
    application:unset_env(nrte, auth_type),
    ok.

get_unauthorized_via_auth_mod() ->
    [{userdata, [{doc, "Tests getting an unauthorized path with auth mod"}]}].

get_unauthorized_via_auth_mod(_Conf) ->
    application:set_env(nrte, auth_type, {auth_mod, ?MODULE}),
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

get_undocumented_path_via_auth_mod() ->
    [{userdata, [{doc, "Tests getting an undocumented path with auth mod"}]}].

get_undocumented_path_via_auth_mod(_Conf) ->
    application:set_env(nrte, auth_type, {auth_mod, ?MODULE}),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:get(Pid, "/undocumented", [{<<"authorization">>, <<"some">>}]),
    {response, fin, 404, _Headers} = gun:await(Pid, StreamRef, 1000),
    application:unset_env(nrte, auth_type),
    ok.

post_empty_body() ->
    [{userdata, [{doc, "Tests posting an empty body"}]}].

post_empty_body(_Conf) ->
    ok = nrte:subscribe([<<"topic">>]),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:post(Pid, "/message?topics=topic", #{}, <<>>),
    {response, _, 200, _Headers} = gun:await(Pid, StreamRef, 1000),
    ok = nrte_receive(<<"topic">>, <<>>),
    ok = gun:close(Pid).

post_no_topic() ->
    [{userdata, [{doc, "Tests posting without topics"}]}].

post_no_topic(_Conf) ->
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:post(Pid, "/message", #{}, ?TEXT),
    {response, _, 400, _Headers} = gun:await(Pid, StreamRef, 1000),
    ok.

post_topic_all_authorized() ->
    [{userdata, [{doc, "Tests posting a topic message with all publications auth"}]}].

post_topic_all_authorized(_Conf) ->
    application:set_env(nrte, auth_type, {always_allow, all}),
    ok = nrte:subscribe([<<"topic">>]),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:post(Pid, "/message?topics=topic", ?HEADERS, ?TEXT),
    {response, fin, 200, _Headers} = gun:await(Pid, StreamRef, 1000),
    ok = nrte_receive(<<"topic">>, ?TEXT),
    application:unset_env(nrte, auth_type),
    ok = gun:close(Pid).

post_topic_message() ->
    [{userdata, [{doc, "Tests posting a topic message"}]}].

post_topic_message(_Conf) ->
    ok = nrte:subscribe([<<"topic">>]),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:post(Pid, "/message?topics=topic", ?HEADERS, ?TEXT),
    {response, fin, 200, _Headers} = gun:await(Pid, StreamRef, 1000),
    ok = nrte_receive(<<"topic">>, ?TEXT),
    ok = gun:close(Pid).

post_topic_with_subtopics() ->
    [{userdata, [{doc, "Tests posting a topic with subtopics"}]}].

post_topic_with_subtopics(_Conf) ->
    Topic = <<"topic:subtopic:subsubtopic:subsubsubtopic">>,
    ExpectedTopicPubs = [
        Topic, <<"topic:subtopic:subsubtopic">>, <<"topic:subtopic">>, <<"topic">>
    ],
    ok = nrte:subscribe(ExpectedTopicPubs),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:post(Pid, ["/message?topics=", Topic], ?HEADERS, ?TEXT),
    {response, fin, 200, _Headers} = gun:await(Pid, StreamRef, 1000),
    true = lists:all(fun(_Topic) -> nrte_receive(Topic, ?TEXT) == ok end, ExpectedTopicPubs),
    ok = gun:close(Pid).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
nrte_auth(Headers) ->
    case maps:is_key(<<"authorization">>, Headers) of
        true -> #{allowed_publications => all};
        false -> unauthorized
    end.

nrte_receive(Topic, Body) ->
    ExpectedMessage = <<Topic/binary, ";", Body/binary>>,
    receive
        {nrte_message, ExpectedMessage} -> ok
    after 1000 -> throw(timeout)
    end.

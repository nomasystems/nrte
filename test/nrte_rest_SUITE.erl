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

%%% INCLUDE FILES

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).

%%% MACROS

%%% RECORDS

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [post_empty_body, post_topic_message].

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
post_empty_body() ->
    [{userdata, [{doc, "Tests posting an empty body"}]}].

post_empty_body(_Conf) ->
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    StreamRef = gun:post(Pid, "/topics", #{}, <<>>),
    receive
        {gun_response, Pid, StreamRef, fin, 400, _Headers} -> ok
    after 1000 -> throw(timeout)
    end,
    ok.

post_topic_message() ->
    [{userdata, [{doc, "Tests posting a topic message"}]}].

post_topic_message(_Conf) ->
    ebus:sub(self(), "topic"),
    {ok, Pid} = gun:open("localhost", 2080),
    {ok, http} = gun:await_up(Pid),
    Body = njson:encode(#{<<"topics">> => [<<"topic">>], <<"message">> => <<"text">>}),
    StreamRef = gun:post(Pid, "/topics", #{<<"content-type">> => <<"application/json">>}, Body),
    receive
        {gun_response, Pid, StreamRef, fin, 200, _Headers} -> ok
    after 1000 -> throw(timeout)
    end,
    [<<"text">>] = ebus_proc:messages(self()),
    ok = gun:close(Pid).

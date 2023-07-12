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
-module(nrte_auth).

-type nrte_auth_value() :: unauthorized | all | none | [RegExp :: iodata()].
-callback nrte_auth(Headers :: #{binary() => binary()}) ->
    nrte_auth_value() | #{allowed_publications | allowed_subscriptions => nrte_auth_value()}.

%%% EXTERNAL EXPORTS
-export([authorization/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
authorization(Req, Type) ->
    AuthValue =
        case nrte_conf:auth_type() of
            {always_allow, Value} -> Value;
            {auth_mod, Mod} -> Mod:nrte_auth(maps:get(headers, Req))
        end,
    case {Type, AuthValue} of
        {publish, #{allowed_publications := Allowed}} -> is_authorized(Req, Allowed);
        {subscribe, #{allowed_subscriptions := Allowed}} -> is_authorized(Req, Allowed);
        {_, #{}} -> is_authorized(Req, none);
        {_, Generic} -> is_authorized(Req, Generic)
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
is_authorized(Req, unauthorized) ->
    {unauthorized, cowboy_req:reply(401, #{}, <<>>, Req)};
is_authorized(Req, Allowed) ->
    TopicList = topic_list(Req),
    case lists:search(fun(Topic) -> is_topic_allowed(Topic, Allowed) =:= false end, TopicList) of
        {value, Value} ->
            {unauthorized, cowboy_req:reply(403, #{}, <<"Unauthorized ", Value/binary>>, Req)};
        false ->
            {authorized, TopicList}
    end.

is_topic_allowed(_Topic, all) ->
    true;
is_topic_allowed(_Topic, none) ->
    false;
is_topic_allowed(Topic, Patterns) ->
    lists:any(fun(Pat) -> re:run(Topic, Pat, [anchored]) =/= nomatch end, Patterns).

topic_list(Req) ->
    Topics = proplists:get_value(<<"topics">>, cowboy_req:parse_qs(Req), <<>>),
    binary:split(Topics, <<";">>, [global]).

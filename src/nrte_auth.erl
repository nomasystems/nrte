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
-behaviour(gen_server).

%%% EXTERNAL EXPORTS
-export([start_link/0, generate_token/1, is_authorized/1]).
-ignore_xref([{?MODULE, start_link, 0}]).

%%% GEN_SERVER EXPORTS
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

generate_token(#{peer := {PeerIp, _}} = Req) ->
    Token = nuid:nuid2(),
    ExpirationSeconds = nrte_conf:token_expiration_seconds(),
    Req2 = cowboy_req:set_resp_cookie(<<"nrte_auth">>, Token, Req, #{
        max_age => ExpirationSeconds, http_only => true
    }),
    Expiration = now_seconds() + ExpirationSeconds,
    true = ets:insert(?MODULE, {Token, PeerIp, Expiration}),
    Req2.

is_authorized(#{headers := Headers} = Req) ->
    case {nrte_conf:auth_type(), maps:get(<<"authorization">>, Headers, undefined)} of
        {{always, Value}, _} -> Value;
        {{Mod, Fun}, Auth} when Auth =/= undefined -> Mod:Fun(Auth);
        _ -> validate_token_cookie(Req)
    end.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
init(State) ->
    ets:new(?MODULE, [named_table, public]),
    {ok, State, timeout()}.

handle_call(_, _, State) ->
    {reply, unsupported, State, timeout()}.

handle_cast(_, State) ->
    {noreply, State, timeout()}.

handle_info(timeout, State) ->
    expire_old_tokens(),
    {noreply, State, timeout()};
handle_info(_, State) ->
    {noreply, State, timeout()}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
expire_old_tokens() ->
    MS = [{{'_', '_', '$1'}, [{'>', now_seconds(), '$1'}], [true]}],
    ets:select_delete(?MODULE, MS).

now_seconds() ->
    erlang:system_time(seconds).

timeout() ->
    nrte_conf:token_cleanup_seconds() * 1000.

validate_token_cookie(#{peer := {PeerIp, _}} = Req) ->
    try cowboy_req:match_cookies([nrte_auth], Req) of
        #{nrte_auth := Token} ->
            MS = [
                {
                    {'$1', '$2', '$3'},
                    [{'=:=', '$1', Token}, {'=:=', '$2', {PeerIp}}, {'=<', now_seconds(), '$3'}],
                    [true]
                }
            ],
            ets:select_delete(?MODULE, MS) > 0
    catch
        _:_ -> false
    end.

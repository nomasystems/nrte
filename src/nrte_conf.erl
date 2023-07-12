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
-module(nrte_conf).

%%% EXTERNAL EXPORTS
-export([
    auth_type/0,
    data_template/0,
    port/0,
    serve_priv_dir/0,
    token_cleanup_seconds/0,
    token_expiration_seconds/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
auth_type() ->
    application:get_env(nrte, auth_type, {always, true}).

data_template() ->
    application:get_env(nrte, data_template, <<"{{topic}};{{message}}">>).

port() ->
    application:get_env(nrte, port, 2080).

serve_priv_dir() ->
    application:get_env(nrte, serve_priv_dir, false).

token_cleanup_seconds() ->
    application:get_env(nrte, token_cleanup_seconds, 60).

token_expiration_seconds() ->
    application:get_env(nrte, token_expiration_seconds, 60).

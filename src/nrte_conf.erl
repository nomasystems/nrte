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
    auth_type/1,
    data_template/0,
    name/1,
    port/1,
    serve_priv_dir/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
auth_type(Opts) ->
    maps:get(auth_type, Opts, {always_allow, all}).

data_template() ->
    application:get_env(nrte, data_template, <<"{{topic}};{{message}}">>).

name(Opts) ->
    maps:get(name, Opts, nrte_listener).

port(Opts) ->
    maps:get(port, Opts, 2080).

serve_priv_dir(Opts) ->
    maps:get(serve_priv_dir, Opts, false).

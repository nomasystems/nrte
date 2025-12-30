%%% Copyright 2025 Nomasystems, S.L. http://www.nomasystems.com
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
-module(nrte_publications).

%%% EXTERNAL EXPORTS
-export([subscription_init_link_terminate/2]).

%%% MACROS
-define(TOPIC_SEPARATOR, <<";">>).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec subscription_init_link_terminate(binary(), list(binary())) -> ok.
subscription_init_link_terminate(Source, TopicList) ->
    Topics = binary:join(TopicList, ?TOPIC_SEPARATOR),
    nrte:publish(<<"nrte:subscription_init:", Source/binary>>, Topics),
    Pid = self(),
    spawn_link(fun() ->
        process_flag(trap_exit, true),
        receive
            {'EXIT', Pid, _} ->
                nrte:publish(<<"nrte:subscription_terminate:", Source/binary>>, Topics)
        end
    end),
    ok.

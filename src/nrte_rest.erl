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
-module(nrte_rest).

%%% INCLUDE FILES

%%% COWBOY HANDLER EXPORTS
-export([init/2]).

%%% MACROS

%%% RECORDS

%%%-----------------------------------------------------------------------------
%%% COWBOY HANDLER EXPORTS
%%%-----------------------------------------------------------------------------
init(Req, Opts) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    #{<<"topics">> := Topics, <<"message">> := Message} = njson:decode(Data),
    lists:foreach(fun(T) -> ebus:pub(T, Message) end, Topics),
    Req3 = cowboy_req:reply(200, #{}, <<>>, Req2),
    {ok, Req3, Opts}.

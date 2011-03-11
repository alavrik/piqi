%% Copyright 2009, 2010, 2011 Anton Lavrik
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
%% limitations under the License.

%%
%% @doc OTP supervisor behavior for Piqi
%%
-module(piqi_sup).

-behaviour(supervisor).

-export([start_link/0, restart_piqi_tools_child/0]).
% OTP supervisor callbacks
-export([init/1]).


-define(SUPERVISOR, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).


restart_piqi_tools_child() ->
    supervisor:restart_child(?SUPERVISOR, piqi_tools).

%
% Supervisor callback
%

init(_Args) ->
    PiqiTools =
        {piqi_tools, % Piqi-Tools bindings for Erlang
            {piqi_tools, start_link, []},
            permanent, 1000, worker,
            [piqi_tools]
        },
    {ok, {{one_for_one, 10, 1}, [PiqiTools]}}.


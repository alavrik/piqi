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
%% @doc Piqi-RPC monitor (controls the state of Piqi-RPC)
%%
-module(piqi_rpc_monitor).

-behavior(gen_server).


-export([start_link/0, start/0, stop/0]).
% API
-export([add_service/2, remove_service/1, pause_service/1, resume_service/1,
         get_service_status/1, get_service_info/1,
         get_status/0, get_info/0]).
%-compile(export_all).
% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%-define(DEBUG, 1).
-include("debug.hrl").


% TODO: coordinate code upgrades of RPC-service implemenatation and rpc modules


% gen_server name
%-define(SERVER, ?MODULE).
-define(SERVER, piqi_rpc).


% time to wait before the next attempt to start Piqi tools server
-define(RESTART_RETRY_TIMEOUT, 10 * 1000). % 10 seconds


-type service_status() :: 'active' | 'paused'.


% Piqi-RPC service
-record(service, {
    rpc_mod :: atom(),
    impl_mod :: atom(),
    status = 'active' :: service_status()
}).


% gen_server state
-record(state, {
    piqi_tools_pid :: pid(),
    services = [] :: [ #service{} ]
}).


%
% starting gen_server manually
%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


% manual start (not as a part of supervision tree)
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


% manual stop
stop() ->
    gen_server:cast(?SERVER, stop).


%
% gen_server callbacks
%


%% @private
init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, Pid} = start_piqi_tools([]),
    State = #state{piqi_tools_pid = Pid},
    {ok, State}.


start_piqi_tools(PiqiList) ->
    % NOTE: we use start() instead of start_link() here, because piqi_toos will
    % be monitored and restarted in a special way by the Piqi-RPC server and we
    % don't want to make "piqi_tools" stop after Piqi-RPC server stops (such
    % stop is dictated by OTP principles and provided by OTP's gen_server when
    % it is started using gen_server:start_link)
    Res = piqi_tools:start(PiqiList),
    case Res of
        {ok, Pid} -> link(Pid);
        _ -> ok
    end,
    Res.


restart_piqi_tools(State) ->
    % XXX: track the number of restart attempts?
    % get the list of Piqi specifications from RPC modules
    PiqiList = lists:append(
        [ RpcMod:piqi() || #service{rpc_mod = RpcMod} <- State#state.services ]),
    case start_piqi_tools(PiqiList) of
        {ok, Pid} ->
            % XXX: log succesful restart
            ?PRINT("piqi_tools restarted successfully"),
            State#state{piqi_tools_pid = Pid};
        {error, _Error} ->
            % TODO: log the error
            ?PRINT({"piqi_tools restart failed", _Error}),
            % try starting Piqi tools again after timeout
            timer:send_after(?RESTART_RETRY_TIMEOUT, 'timeout'),
            State#state{piqi_tools_pid = 'undefined'}
    end.


%% @private
handle_call({add_service, ImplMod, RpcMod}, _From, State) ->
    % add a new Piqi-RPC service to the list of known services
    NewService = #service{rpc_mod = RpcMod, impl_mod = ImplMod},

    % add Piqi types of the PRC module to Piqi-tools:
    case State#state.piqi_tools_pid of
        'undefined' ->
            % we'll add types later when piqi_tools server is successfully
            % restarted
            ok;
        _ ->
            % loading the implementation module, otherwise
            % erlang:function_exported() called from Piqi-RPC runtime would
            % return false
            % NOTE: this will work correctly for both embedded and interactive
            % Erlang VM modes; also, we don't care about errors
            case code:is_loaded(ImplMod) of
                false -> code:load_file(ImplMod);
                _ -> ok
            end,

            % FIXME: handle error and possible exit:noproc exception, because
            % otherwise we'll just crash
            PiqiList = RpcMod:piqi(),
            ok = piqi_tools:add_piqi(PiqiList)
    end,
    NewState = State#state{ 
        services = add_serv(NewService, State#state.services)
    },
    {reply, ok, NewState};

handle_call({remove_service, ImplMod}, _From, State) ->
    % remove a Piqi-RPC service from the list of known services
    case take_serv(ImplMod, State#state.services) of
        {_Service, Services} ->
            NewState = State#state{ services = Services },
            {reply, ok, NewState};
        false ->
            {reply, {error, "no such service"}, State}
    end;

handle_call({update_status, ImplMod, NewStatus}, _From, State) ->
    case take_serv(ImplMod, State#state.services) of
        {Service, Services} ->
            NewService = Service#service{status = NewStatus},
            NewState = State#state{ 
                services = add_serv(NewService, Services)
            },
            {reply, ok, NewState};
        'undefined' ->
            {reply, {error, "no such service"}, State}
    end;


handle_call(get_status, _From, State)
        when State#state.piqi_tools_pid == 'undefined' ->
    % Piqi tools has crashed and hasn't started yet
    {reply, 'system_paused', State};

handle_call(get_status, _From, State) ->
    {reply, 'active', State};


handle_call({get_status, _ImplMod}, _From, State)
        when State#state.piqi_tools_pid == 'undefined' ->
    % Piqi tools has crashed and hasn't started yet
    {reply, 'system_paused', State};

handle_call({get_status, ImplMod}, _From, State) ->
    Response =
        case find_serv(ImplMod, State#state.services) of
            'undefined' -> 'undefined'; % no such service
            #service{ status = Status } -> Status
        end,
    {reply, Response, State};


handle_call(get_info, _From, State) ->
    Response = State#state.services, % return the list of services
    {reply, Response, State};

handle_call({get_info, ImplMod}, _From, State) ->
    Response =
        case find_serv(ImplMod, State#state.services) of
            'undefined' -> 'undefined'; % no such service
            Service -> Service
        end,
    {reply, Response, State};


handle_call(_Message, _From, State) ->
    % XXX:
    {noreply, State}.


%% @private
handle_cast(stop, State) ->
    {stop, normal, State}; 

handle_cast(_Msg, State) ->
    % XXX
    {noreply, State}.


%% @private
handle_info({'EXIT', Pid, _Reason}, State = #state{piqi_tools_pid = Pid}) ->
    ?PRINT({"Exit from piqi_tools", _Reason}),
    % Piqi tools server has exited -- restarting:
    NewState = restart_piqi_tools(State),
    {noreply, NewState};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    % EXIT from a linked process (one that called start_link/0)
    % -- just ignoring it
    {noreply, State};

handle_info('timeout', State) ->
    % try restarting Piqi tools again:
    NewState = restart_piqi_tools(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    % XXX
    {noreply, State}.


%% @private
terminate(_Reason, _State) -> ok.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%
% Utility functions
%

% interface for accessing the list of services by the name of implementation
% module

add_serv(Service, Services) ->
    [ Service | Services ].


find_serv(ImplMod, Services) ->
    case lists:keyfind(ImplMod, #service.impl_mod, Services) of
        false -> 'undefined'; % no such service
        X -> X
    end.


take_serv(ImplMod, Services) ->
    case lists:keytake(ImplMod, #service.impl_mod, Services) of
        {value, Service, OtherServices} ->
            {Service, OtherServices};
        false ->
            'undefined'
    end.


%
% API implementation
%

-spec add_service/2 ::
    ( ImplMod :: atom(), RpcMod :: atom() ) -> ok | {error, any()}.


add_service(ImplMod, RpcMod) ->
    gen_server:call(?SERVER, {add_service, ImplMod, RpcMod}).


-spec remove_service/1 ::
    ( ImplMod :: atom() ) -> ok | {error, any()}.

remove_service(ImplMod) ->
    gen_server:call(?SERVER, {remove_service, ImplMod}).


-spec pause_service/1 ::
    ( ImplMod :: atom() ) -> ok | {error, any()}.

pause_service(ImplMod) ->
    gen_server:call(?SERVER, {update_status, ImplMod, 'paused'}).


-spec resume_service/1 ::
    ( ImplMod :: atom() ) -> ok | {error, any()}.

resume_service(ImplMod) ->
    gen_server:call(?SERVER, {update_status, ImplMod, 'active'}).


-spec get_service_status/1 ::
    ( ImplMod :: atom() ) -> service_status() | 'undefined' | 'system_paused'.

get_service_status(ImplMod) ->
    gen_server:call(?SERVER, {get_status, ImplMod}).


-spec get_service_info/1 ::
    ( ImplMod :: atom() ) -> #service{} | 'undefined'.

get_service_info(ImplMod) ->
    gen_server:call(?SERVER, {get_info, ImplMod}).


-spec get_status/0 :: () -> 'active' | 'system_paused'.

get_status() ->
    gen_server:call(?SERVER, get_status).


-spec get_info/0 :: () -> [ #service{} ].

get_info() ->
    gen_server:call(?SERVER, get_info).


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
%% @doc Erlang bindings for Piqi tools
%%
%% This module contains Erlang binding for some of Piqi tools functions such as
%% "piqi convert". It is implemented as a gen_server that communicates with Piqi
%% tools server ("piqi server") via Erlang port interface.
%%
-module(piqi_tools).

-behavior(gen_server).


% TODO: tests, edoc

% XXX: debug mode when piqi-server started with --trace and warnings enabled?


-export([start_link/0, start_link/1, start/0, start/1, stop/0]).
% API
-export([add_piqi/1, convert/4, convert/5, ping/0]).
% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
%-compile(export_all).


-include("../include/piqirun.hrl").

-include("piqi_rpc_piqi.hrl").
-include("piqi_tools_piqi.hrl").


%-define(DEBUG, 1).
%-include("debug.hrl").


% gen_server name
-define(SERVER, ?MODULE).

-define(PIQI_TOOLS_ERROR, 'piqi_tools_error').


% gen_server state
-record(state, {
    port :: port(),
    prev_data :: binary()
}).


%
% starting gen_server
%

start_link() ->
    start_link([]).


start_link(PiqiList) ->
    case start_common(start_link, PiqiList) of
        {error, {already_started, Pid}} ->
            % the server can be started from many processes, but only one
            % instance of the server will be running
            link(Pid),
            {ok, Pid};
        Res -> Res
    end.


% independent start -- not as a part of OTP supervision tree
start() ->
    start([]).

start(PiqiList) ->
    case start_common(start, PiqiList) of
        {error, {already_started, Pid}} ->
            % the server can be started from many processes, but only one
            % instance of the server will be running
            {ok, Pid};
        Res -> Res
    end.


start_common(StartFun, PiqiList) ->
    gen_server:StartFun({local, ?SERVER}, ?MODULE, PiqiList, []).


% manual stop (when started by start/0 *)
stop() ->
    gen_server:cast(?SERVER, stop).


%
% gen_server callbacks
%


%% @private
init(PiqiList) ->
    erlang:process_flag(trap_exit, true),
    Command = "piqi server --no-warnings",
    %Command = "tee ilog | piqi server --trace | tee olog",

    % TODO: handle initialization error and report it as {error, Error} tuple
    % XXX: use {spawn_executable, Command} instead to avoid shell's error
    % messages printed on stderr?
    Port = erlang:open_port({spawn, Command}, [stream, binary]), % exit_status?
    State = #state{ port = Port, prev_data = <<>> }, % no previous data on start

    NewState = add_piqi_local(PiqiList, State),
    {ok, NewState}.


%% @private
handle_call({rpc, PiqiMod, Name, ArgsData}, From, State)
        when PiqiMod =/= 'undefined' ->
    % make sure that Piqi server is provided with relevant type information
    % before calling the actual handler
    NewState = add_piqi_local(PiqiMod, State),
    % now call the actual handler
    handle_call({rpc, 'undefined', Name, ArgsData}, From, NewState);

handle_call({rpc, _PiqiMod, Name, ArgsData}, _From, State) ->
    #state{port = Port, prev_data = PrevData} = State,
    send_rpc_request(Port, Name, ArgsData),
    {Response, Rest} = receive_rpc_response(Port, PrevData),
    NewState = State#state{ prev_data = Rest },
    {reply, Response, NewState}.


%% @private
handle_cast(stop, State) ->
    {stop, normal, State}; 

handle_cast(_Msg, State) ->
    % XXX
    {noreply, State}.


%% @private
handle_info({'EXIT', Port, Reason}, State = #state{port = Port}) ->
    % Port command has exited
    StopReason = {?PIQI_TOOLS_ERROR, {'port_command_exited', Reason}},
    {stop, StopReason, State};

handle_info({'EXIT', _Port, _Reason}, State) ->
    % EXIT from a linked process, just ignoring it
    {noreply, State};

handle_info(Info, State) ->
    % XXX: log a message
    erlang:port_close(State#state.port),
    StopReason = {?PIQI_TOOLS_ERROR, {'unexpected_message', Info}},
    {stop, StopReason, State}.


%% @private
terminate(_Reason, State) ->
    % don't bother checking if the port is still valid, e.g. after EXIT
    catch erlang:port_close(State#state.port),
    ok.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%
% Piqi-RPC wire packets 
%

send_rpc_packet(Port, Data) ->
    Message = piqirun:gen_block(Data),
    erlang:port_command(Port, Message).


receive_rpc_packet(Port, PrevData) ->
    receive
        % packet from the rpc server
        {Port, {data, NewData}} ->
            Data = <<PrevData/binary, NewData/binary>>,
            try
                piqirun:parse_block(Data)
            catch
                {'piqirun_error', 'not_enough_data'} ->
                    % receive the next portion if there's not enough data in
                    % this message
                    receive_rpc_packet(Port, Data)
            end;

        {'EXIT', Port, Reason} ->
            StopReason = {?PIQI_TOOLS_ERROR, {'port_command_exited', Reason}},
            exit(StopReason)
    end.


%
% Piqi-RPC request/response
%

send_rpc_request(Port, Name, ArgsData) ->
    Request = #piqi_rpc_request{ name = Name, data = ArgsData },
    Data = piqi_rpc_piqi:gen_request('undefined', Request),
    send_rpc_packet(Port, Data).


receive_rpc_response(Port, PrevData) ->
    {Data, Rest} = receive_rpc_packet(Port, PrevData),
    Response = piqi_rpc_piqi:parse_response(Data),
    {Response, Rest}.


%
% API implementation
%

-spec rpc/3 :: (
    PiqiMod :: 'undefined' | atom(),
    Name :: binary() | string(),
    ArgsData :: 'undefined' | iodata() ) -> piqi_rpc_response().

% make an RPC call for function name "Name" and Protobuf-encoded arguments
% "ArgsData". "PiqiMod" is the name of the Erlang module that was generated by
% "piqic erlang". This module must contain type information for the function
% arguments.
%
% XXX: timeouts?
rpc(PiqiMod, Name, ArgsData) ->
    BinData =
        case ArgsData of
            'undefined' -> 'undefined';
            _ -> iolist_to_binary(ArgsData)
        end,
    gen_server:call(?SERVER, {rpc, PiqiMod, Name, BinData}).


-spec rpc/2 :: (
    Name :: binary() | string(),
    ArgsData :: 'undefined' | iodata() ) -> piqi_rpc_response().

% make an RPC call of function "Name" and Protobuf-encoded arguments
% "ArgsData"
rpc(Name, ArgsData) ->
    rpc(_PiqiMod = 'undefined', Name, ArgsData).


-spec rpc/1 :: ( Name :: binary() | string() ) -> piqi_rpc_response().

% make an RPC call of function "Name" that doesn't have input parameters
rpc(Name) ->
    rpc(Name, _ArgsData = 'undefined').



% NOTE: this function is called only from init/1 during execution of
% add_piqi_local/1
call_local(Name, BinData, State) ->
    % XXX: not expecting any other response from the call hander
    {reply, Response, NewState} =
        handle_call(
            {rpc, _PiqiMod = 'undefined', Name, BinData},
            _From = 'undefined',
            State),
    {Response, NewState}.


-spec ping/0 :: () -> ok.

% a simple service livecheck function
ping() ->
    rpc(<<"ping">>).


-spec add_piqi/1 :: (BinPiqiList :: [binary()]) -> ok | {error, string()}.

% add a Protobuf-encoded Piqi module specifications to Piqi tools. Added types
% will be used later by "convert" and other functions.
add_piqi(BinPiqiList) ->
    BinInput = encode_add_piqi_input(BinPiqiList),
    Output = rpc(<<"add-piqi">>, BinInput),
    decode_add_piqi_output(Output).


add_piqi_local(PiqiMod, State) when is_atom(PiqiMod) ->
    % add type information from the PiqiMod to the Piqi-tools server
    % NOTE: using process dictionary as a fast SET container for PiqiMod
    case erlang:get(PiqiMod) of
        'undefined' ->
            % XXX: check if the function is exported?
            BinPiqiList = PiqiMod:piqi(),
            NewState = add_piqi_local(BinPiqiList, State),
            % memorize that we've added type information
            erlang:put(PiqiMod, 'add_piqi'),
            NewState;
        'add_piqi' ->
            % type information from this module has been added already
            State
    end;

add_piqi_local(BinPiqiList, State) ->
    BinInput = encode_add_piqi_input(BinPiqiList),
    {Output, NewState} = call_local(<<"add-piqi">>, BinInput, State),
    % XXX: don't handle errors, assuming that this call is safea since we've
    % added these Piqi modules before without a problem
    ok = decode_add_piqi_output(Output),
    NewState.


encode_add_piqi_input(BinPiqiList) ->
    Input = #piqi_tools_add_piqi_input{
        format = 'pb',
        data = BinPiqiList
    },
    BinInput = piqi_tools_piqi:gen_add_piqi_input('undefined', Input),
    iolist_to_binary(BinInput).


decode_add_piqi_output(Output) ->
    case Output of
        ok -> ok;
        {error, BinError} ->
            Buf = piqirun:init_from_binary(BinError),
            Error = piqi_tools_piqi:parse_add_piqi_error(Buf),
            % NOTE: parsed strings are represented as binaries
            {error, binary_to_list(Error)};
        X ->
            handle_common_result(X)
    end.


-spec convert/5 :: (
    PiqiMod :: 'undefined' | atom(), % Erlang module generated by "piqic erlang"
    TypeName :: string() | binary(),
    InputFormat :: piqi_tools_format(),
    OutputFormat :: piqi_tools_format(),
    Data :: binary() ) -> {ok, Data :: binary()} | {error, string()}.

% convert `Data` of type `TypeName` from `InputFormat` to `OutputFormat`
convert(PiqiMod, TypeName, InputFormat, OutputFormat, Data) ->
    Input = #piqi_tools_convert_input{
        type_name = TypeName,
        input_format = InputFormat,
        output_format = OutputFormat,
        data = Data
    },
    BinInput = piqi_tools_piqi:gen_convert_input('undefined', Input),
    case rpc(PiqiMod, <<"convert">>, BinInput) of
        {ok, BinOutput} ->
            Buf = piqirun:init_from_binary(BinOutput),
            Output = piqi_tools_piqi:parse_convert_output(Buf),
            Res = Output#piqi_tools_convert_output.data,
            {ok, Res};
        {error, BinError} ->
            Buf = piqirun:init_from_binary(BinError),
            Error = piqi_tools_piqi:parse_convert_error(Buf),
            % NOTE: parsed strings are represented as binaries
            {error, binary_to_list(Error)};
        X ->
            handle_common_result(X)
    end.


-spec convert/4 :: (
    TypeName :: string() | binary(),
    InputFormat :: piqi_tools_format(),
    OutputFormat :: piqi_tools_format(),
    Data :: binary() ) -> {ok, Data :: binary()} | {error, string()}.

% convert `Data` of type `TypeName` from `InputFormat` to `OutputFormat`
convert(TypeName, InputFormat, OutputFormat, Data) ->
    convert('undefined', TypeName, InputFormat, OutputFormat, Data).


handle_common_result({rpc_error, _} = X) ->
    % recoverable protocol-level error
    throw({?PIQI_TOOLS_ERROR, X}).


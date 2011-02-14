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


-export([start_link/0, start/0, stop/0]).
% API
-export([add_piqi/1, convert/4, ping/0]).
% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
%-compile(export_all).


-include_lib("piqirun/include/piqirun.hrl").

-include("piqi_rpc_piqi.hrl").
-include("piqi_tools_piqi.hrl").


% gen_server name
-define(SERVER, ?MODULE).

-define(PIQI_ERROR, 'piqi_tools_error').


% gen_server state
-record(state, {
    port :: port(),
    prev_data :: binary()
}).


%
% starting gen_server manually
%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


% manual start (not as a part of supervision tree)
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


% manual stop (when started by start/0 *)
stop() ->
    gen_server:cast(?SERVER, stop).


%
% gen_server callbacks
%


%% @private
init([]) ->
    erlang:process_flag(trap_exit, true),
    Command = "piqi server --no-warnings",
    Port = erlang:open_port({spawn, Command}, [stream, binary]), % exit_status?
    State = #state{ port = Port, prev_data = <<>> }, % no previous data on start
    {ok, State}.


%% @private
handle_call({rpc, Name, ArgsData}, _From, State) ->
    #state{port = Port, prev_data = PrevData} = State,
    send_rpc_request(Port, Name, ArgsData),
    {Response, Rest} = receive_rpc_response(Port, PrevData),
    % XXX: handle {piqi_error, Reason} response?
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
    StopReason = {?PIQI_ERROR, {'port_command_exited', Reason}},
    {stop, StopReason, State};

handle_info(Info, State) ->
    erlang:port_close(State#state.port),
    StopReason = {?PIQI_ERROR, {'unexpected_message', Info}},
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
            StopReason = {?PIQI_ERROR, {'port_command_exited', Reason}},
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

-spec rpc/2 :: (
    Name :: binary() | string(),
    ArgsData :: 'undefined' | iodata() ) -> piqi_rpc_response().

% make an RPC call for function name "Name" and Protobuf-encoded arguments
% "ArgsData"
%
% XXX: timeouts?
rpc(Name, ArgsData) ->
    BinData =
        case ArgsData of
            'undefined' -> 'undefined';
            _ -> iolist_to_binary(ArgsData)
        end,
    gen_server:call(?SERVER, {rpc, Name, BinData}).


-spec rpc/1 :: ( Name :: binary() | string() ) -> piqi_rpc_response().

% make an RPC call for function name "Name" that doesn't have parameters
rpc(Name) ->
    rpc(Name, _ArgsData = 'undefined').


-spec ping/0 :: () -> ok.

% a simple service livecheck function
ping() ->
    rpc(<<"ping">>).


-spec add_piqi/1 :: (BinPiqi :: [binary()]) -> ok | {error, string()}.

% add a Protobuf-encoded Piqi module specifications to Piqi tools. Added types
% will be used later by "convert" and other functions.
add_piqi(BinPiqi) ->
    Input = #piqi_tools_add_piqi_input{
        format = 'pb',
        data = BinPiqi
    },
    BinInput = piqi_tools_piqi:gen_add_piqi_input('undefined', Input),
    case rpc(<<"add-piqi">>, BinInput) of
        ok -> ok;
        X -> handle_common_result(X)
    end.


-spec convert/4 :: (
    TypeName :: string() | binary(),
    InputFormat :: piqi_tools_format(),
    OutputFormat :: piqi_tools_format(),
    Data :: binary() ) -> {ok, Data :: binary()} | {error, string()}.

% convert `Data` of type `TypeName` from `InputFormat` to `OutputFormat`
convert(TypeName, InputFormat, OutputFormat, Data) ->
    Input = #piqi_tools_convert_input{
        type_name = TypeName,
        input_format = InputFormat,
        output_format = OutputFormat,
        data = Data
    },
    BinInput = piqi_tools_piqi:gen_convert_input('undefined', Input),
    case rpc(<<"convert">>, BinInput) of
        {ok, BinOutput} ->
            Buf = piqirun:init_from_binary(BinOutput),
            Output = piqi_tools_piqi:parse_convert_output(Buf),
            Res = Output#piqi_tools_convert_output.data,
            {ok, Res};
        X ->
            handle_common_result(X)
    end.


handle_common_result({error, X}) ->
    % NOTE: parsed strings are represented binaries
    Buf = piqirun:init_from_binary(X),
    Bin = piqi_tools_piqi:parse_piqi_error(Buf),
    S = binary_to_list(Bin),
    {error, S};

handle_common_result({piqi_error, X}) ->
    % recoverable protocol-level error
    S = binary_to_list(X),
    throw({?PIQI_ERROR, {'piqi_rpc_error', S}}).


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

-module(piqi_rpc).
-compile(export_all).


init(BinPiqiList) ->
    piqi_tools:add_piqi(BinPiqiList).
    

call(Mod, Name) ->
    Mod:Name().


call(Mod, Name, Input) ->
    Mod:Name(Input).


% TODO, XXX: return the list of Piqi modules in various formats
get_piqi(BinPiqiList) ->
    % return the Piqi module and all the dependencies encoded as a list of Piqi
    % each encoded using Protobuf binary format
    piqirun:gen_list('undefined', fun piqirun:string_to_block/2, BinPiqiList).


decode_input(Decoder, TypeName, InputFormat, InputData) ->
    BinInput =
        case piqi_tools:convert(TypeName, InputFormat, 'pb', InputData) of
            {ok, X} -> X;
            {error, Error} ->
                % input data validation error
                throw({'piqi_rpc_error', "error converting input: " ++ Error})
        end,
    Buf = piqirun:init_from_binary(BinInput),
    Decoder(Buf).


encode_common(Encoder, TypeName, OutputFormat, Output) ->
    BinOutput = Encoder('undefined', Output),
    piqi_tools:convert(TypeName, 'pb', OutputFormat, BinOutput).


encode_output(Encoder, TypeName, OutputFormat, Output) ->
    case encode_common(Encoder, TypeName, OutputFormat, Output) of
        {ok, OutputData} -> {ok, OutputData};
        {error, Error} ->
            % internal error
            {'piqi_error', "error converting output: " ++ Error}
    end.


encode_error(Encoder, TypeName, OutputFormat, Output) ->
    case encode_common(Encoder, TypeName, OutputFormat, Output) of
        {ok, ErrorData} -> {error, ErrorData};
        {error, Error} ->
            % internal error
            {'piqi_error', "error converting error: " ++ Error}
    end.


unknown_function(Name) ->
    todo.


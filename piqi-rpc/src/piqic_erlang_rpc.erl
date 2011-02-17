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

-module(piqic_erlang_rpc).
-compile(export_all).

-include("piqi.hrl").


main([Filename]) ->
    piqic(Filename);

main(_) ->
    erlang:halt(1).


command(Cmd) ->
    %os:cmd(Cmd).
    case eunit_lib:command(Cmd) of
        {0, _} -> ok;
        {_Code, Error} ->
            io:format("command \"~s\" failed with error: ~s~n", [Cmd, Error]),
            erlang:halt(1)
    end.


piqic(Filename) ->
    ExpandedPiqi = Filename ++ ".expanded.pb",
    try
        ExpandCmd = lists:concat([
            "piqic expand --erlang -b -o ", ExpandedPiqi, " ", Filename
        ]),
        command(ExpandCmd),

        Piqi = read_piqi(ExpandedPiqi),

        gen_piqi(Piqi)
    after
        file:delete(ExpandedPiqi)
    end.


read_piqi(Filename) ->
    {ok, Bytes} = file:read_file(Filename),
    Buf = piqirun:init_from_binary(Bytes),
    Piqi = piqi:parse_piqi(Buf),
    Piqi.


gen_piqi(Piqi) ->
    ErlMod = Piqi#piqi.erlang_module,
    FuncList = Piqi#piqi.func,
    Code = iod("\n\n", [
        gen_init(ErlMod),
        gen_get_piqi(ErlMod),
        gen_rpc(ErlMod, FuncList)
    ]),
    io:put_chars(Code),
    ok.


% TODO: as a part of initialization, check that the implementation functions is
% acutally defined using the followin BIF:
%
%    erlang:function_exported(Module, Function, Arity) -> bool()
gen_init(ErlMod) ->
    [
        "init() ->\n",
        "    piqi_rpc:init(", ErlMod, ":piqi()).\n"
    ].


gen_get_piqi(ErlMod) ->
    [
        "get_piqi() ->\n",
        "    piqi_rpc:get_piqi(", ErlMod, ":piqi()).\n"
    ].


gen_rpc(ErlMod, FuncList) ->
    FuncClauses = [ gen_func_clause(X, ErlMod) || X <- FuncList ],
    [
        "rpc(Mod, Name, InputData, InputFormat, OutputFormat) ->\n",
        "    case Name of\n",
            iod("\n", FuncClauses), "\n",
            gen_default_clause(),
        "    end.\n"
    ].


gen_default_clause() ->
    [
"        _ ->\n",
"            piqi_rpc:unknown_function(Name)\n"
    ].


gen_func_clause(F, ErlMod) ->
    Name = F#func.name,
    ErlName = F#func.erlang_name,
    InputCode =
        case F#func.input of
            'undefined' -> % the function doesn't have input
"            case piqi_rpc:call(Mod, ErlName) of\n";
            _ ->
                [
"            Input = piqi_rpc:decode_input(fun ", ErlMod, ":parse_", ErlName, "_input/1, <<\"", Name, "-input\">>, InputFormat, InputData),\n"
"            case piqi_rpc:call(Mod, ErlName, Input) of\n"
                ]
        end,

    OutputCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce output
"                ok -> ok";
            _ ->
                [
"                {ok, Output} ->\n"
"                    piqi_rpc:encode_output(", ErlMod, ":gen_", ErlName, "_output/2, <<\"", Name, "-output\">>, OutputFormat, Output)"
                ]
        end,

    ErrorCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce errors
                [];
            _ ->
                [[
"                {error, Error} ->\n"
"                    piqi_rpc:encode_error(", ErlMod, ":gen_", ErlName, "_error/2, <<\"", Name, "-error\">>, OutputFormat, Error)"
                ]]
        end,

    Code = [
"        <<\"", Name, "\">> ->\n",
                InputCode,
                iod(";\n", [OutputCode | ErrorCode]),
                "\n",
"            end;\n"
    ],
    Code.


iod(_Delim, []) -> [];
iod(Delim, [H|T]) ->
    lists:foldl(fun (X, Accu) -> [Accu, Delim, X] end, H, T).


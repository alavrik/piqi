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
    Mod = Piqi#piqi.module,
    ErlMod = Piqi#piqi.erlang_module,
    FuncList = Piqi#piqi.func,

    Filename = binary_to_list(ErlMod) ++ "_rpc.erl",
    {ok, File} = file:open(Filename, ['write']),

    Code = iod("\n\n", [
        [
            "-module(", ErlMod, "_rpc).\n",
            "-compile(export_all).\n"
        ],
        gen_init(ErlMod),
        gen_get_piqi(ErlMod),
        gen_rpc(Mod, ErlMod, FuncList)
    ]),
    io:put_chars(File, Code),
    ok.


gen_init(ErlMod) ->
    [
        "init() ->\n",
        "    piqi_rpc:init(", ErlMod, ":piqi()).\n"
    ].


gen_get_piqi(ErlMod) ->
    [
        "get_piqi(OutputFormat) ->\n",
        "    piqi_rpc:get_piqi(", ErlMod, ":piqi(), OutputFormat).\n"
    ].


gen_rpc(Mod, ErlMod, FuncList) ->
    FuncClauses = [ gen_func_clause(X, Mod, ErlMod) || X <- FuncList ],
    [
        "rpc(Mod, Name, InputData, _InputFormat, _OutputFormat) ->\n",
        "    try\n",
        "    case Name of\n",
            iod("\n", FuncClauses), "\n",
            gen_default_clause(),
        "    end\n",
        "    catch\n",
        "        Class:Reason -> piqi_rpc:handle_runtime_exception(Class, Reason)\n",
        "    end.\n"
    ].


gen_default_clause() ->
    [
"        _ ->\n",
"            piqi_rpc:handle_unknown_function()\n"
    ].


gen_func_clause(F, Mod, ErlMod) ->
    Name = F#func.name,
    ErlName = F#func.erlang_name,
    ScopedName = [ Mod, "/", Name ],
    InputCode =
        case F#func.input of
            'undefined' -> % the function doesn't have input
                [
"            piqi_rpc:check_empty_input(InputData),\n",
"            case piqi_rpc:call(Mod, ",  ErlName, ") of\n"
                ];
            _ ->
                [
"            Input = piqi_rpc:decode_input(fun ", ErlMod, ":parse_", ErlName, "_input/1, <<\"", ScopedName, "-input\">>, _InputFormat, InputData),\n"
"            case piqi_rpc:call(Mod, ", ErlName, ", Input) of\n"
                ]
        end,

    OutputCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce output
"                ok -> ok";
            _ ->
                [
"                {ok, Output} ->\n"
"                    piqi_rpc:encode_output(fun ", ErlMod, ":gen_", ErlName, "_output/2, <<\"", ScopedName, "-output\">>, _OutputFormat, Output)"
                ]
        end,

    ErrorCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce errors
                [];
            _ ->
                [[
"                {error, Error} ->\n"
"                    piqi_rpc:encode_error(fun ", ErlMod, ":gen_", ErlName, "_error/2, <<\"", ScopedName, "-error\">>, _OutputFormat, Error)"
                ]]
        end,

    DefaultCaseCode = [
"                X -> piqi_rpc:handle_invalid_result(Name, X)"
    ],

    Code = [
"        <<\"", Name, "\">> ->\n",
                InputCode,
                iod(";\n", [OutputCode] ++ ErrorCode ++ [DefaultCaseCode]),
                "\n",
"            end;\n"
    ],
    Code.


iod(_Delim, []) -> [];
iod(Delim, [H|T]) ->
    lists:foldl(fun (X, Accu) -> [Accu, Delim, X] end, H, T).


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

-include("piqi_piqi.hrl").


% TODO: check that the version of this plugin and "piqic version" are exactly
% the same


main(Args) ->
    {Filename, Odir} = parse_args(Args),
    piqic_erlang(Args),
    piqic_erlang_rpc(Filename, Odir, Args).


usage() ->
    io:format(
"Usage: piqic-erlang-rpc [options] <.piqi file>\n"
"Options:
  -I <dir> add directory to the list of imported .piqi search paths
  --no-warnings don't print warnings
  --trace turn on tracing
  --debug <level> debug level; any number greater than 0 turns on debug messages
  --noboot don't boot, i.e. don't use boot definitions while processing .piqi
  -C <output directory> specify output directory
  --normalize <true|false> normalize identifiers (default: true)
  -help  Display this list of options
  --help  Display this list of options
"
    ).


% extract filename (last argument) and output directory (argument following -C)
parse_args([]) ->
    usage(),
    erlang:halt(1);

parse_args([X]) when X == "-help" orelse X == "--help" ->
    usage(),
    erlang:halt(0);

parse_args(Args) ->
    parse_args(Args, _Odir = 'undefined').


parse_args([Filename], Odir) ->
    {Filename, Odir};

parse_args(["-C", Odir |T], _) ->
    parse_args(T, Odir);

parse_args([_|T], Odir) ->
    parse_args(T, Odir).


piqic_erlang(Args) ->
    CustomArgs =
        "--gen-default-impl --gen-defaults --embed-piqi --gen-impl-header ",
    PiqicErlang = lists:concat([
            "piqic erlang ", CustomArgs, join_args(Args)
        ]),
    command(PiqicErlang).


join_args(Args) ->
    Args1 = [escape_arg(X) || X <- Args],
    string:join(Args1, " ").


escape_arg(X) ->
    case lists:member($\ , X) of
        true -> "'" ++ X ++ "'";
        false -> X
    end.


set_cwd('undefined') -> ok;
set_cwd(Dir) ->
    ok = file:set_cwd(Dir).


piqic_erlang_rpc(Filename, Odir, Args) ->
    ExpandedPiqi = Filename ++ ".expanded.pb",
    try
        PiqicExpand = lists:concat([
            "piqic expand --erlang -b -o ", ExpandedPiqi, " ", join_args(Args)
        ]),
        command(PiqicExpand),

        set_cwd(Odir),

        Piqi = read_piqi(ExpandedPiqi),

        gen_piqi(Piqi)
    after
        file:delete(ExpandedPiqi)
    end.


command(Cmd) ->
    %os:cmd(Cmd).
    case eunit_lib:command(Cmd) of
        {0, X} ->
            io:put_chars(X),
            ok;
        {_Code, Error} ->
            io:format("command \"~s\" failed with error: ~s~n", [Cmd, Error]),
            erlang:halt(1)
    end.


read_piqi(Filename) ->
    {ok, Bytes} = file:read_file(Filename),
    Buf = piqirun:init_from_binary(Bytes),
    Piqi = piqi_piqi:parse_piqi(Buf),
    Piqi.


gen_piqi(Piqi) ->
    Mod = Piqi#piqi.module,
    ErlMod = Piqi#piqi.erlang_module,
    FuncList = Piqi#piqi.func,

    Filename = binary_to_list(ErlMod) ++ "_rpc.erl",

    Code = iod("\n\n", [
        [
            "-module(", ErlMod, "_rpc).\n",
            "-compile(export_all).\n"
        ],
        gen_init(ErlMod),
        gen_get_piqi(ErlMod),
        gen_rpc(Mod, ErlMod, FuncList)
    ]),
    ok = file:write_file(Filename, Code).


gen_init(ErlMod) ->
    [
        "piqi() ->\n",
        "    ", ErlMod, ":piqi().\n"
    ].


gen_get_piqi(_ErlMod) ->
    [
        "get_piqi(OutputFormat) ->\n",
        "    piqi_rpc_runtime:get_piqi(piqi(), OutputFormat).\n"
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
        "        Class:Reason -> piqi_rpc_runtime:handle_runtime_exception(Class, Reason)\n",
        "    end.\n"
    ].


gen_default_clause() ->
    [
"        _ ->\n",
"            piqi_rpc_runtime:handle_unknown_function()\n"
    ].


gen_func_clause(F, Mod, ErlMod) ->
    Name = F#func.name,
    ErlName = F#func.erlang_name,
    ScopedName = [ Mod, "/", Name ],
    InputCode =
        case F#func.input of
            'undefined' -> % the function doesn't have input
                [
"            piqi_rpc_runtime:check_empty_input(InputData),\n",
"            case piqi_rpc_runtime:call(Mod, ",  ErlName, ", 'undefined') of\n"
                ];
            _ ->
                [
"            Input = piqi_rpc_runtime:decode_input(?MODULE, fun ", ErlMod, ":parse_", ErlName, "_input/1, <<\"", ScopedName, "-input\">>, _InputFormat, InputData),\n"
"            case piqi_rpc_runtime:call(Mod, ", ErlName, ", Input) of\n"
                ]
        end,

    OutputCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce output
"                ok -> ok";
            _ ->
                [
"                {ok, Output} ->\n"
"                    piqi_rpc_runtime:encode_output(?MODULE, fun ", ErlMod, ":gen_", ErlName, "_output/1, <<\"", ScopedName, "-output\">>, _OutputFormat, Output)"
                ]
        end,

    ErrorCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce errors
                [];
            _ ->
                [[
"                {error, Error} ->\n"
"                    piqi_rpc_runtime:encode_error(?MODULE, fun ", ErlMod, ":gen_", ErlName, "_error/1, <<\"", ScopedName, "-error\">>, _OutputFormat, Error)"
                ]]
        end,

    DefaultCaseCode = [
"                X -> piqi_rpc_runtime:handle_invalid_result(Name, X)"
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


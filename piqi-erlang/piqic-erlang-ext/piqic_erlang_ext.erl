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

-module(piqic_erlang_ext).
-compile(export_all).

-include("piqi_piqi.hrl").


% TODO: check that the version of this plugin and "piqic version" are exactly
% the same


main(Args) ->
    {Filename, Odir} = parse_args(Args),
    piqic_erlang(Args),
    piqic_erlang_ext(Filename, Odir, Args).


usage() ->
    io:format(
"Usage: piqic-erlang-ext [options] <.piqi file>\n"
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
        "--embed-piqi ",
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


piqic_erlang_ext(Filename, Odir, Args) ->
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
    Defs = Piqi#piqi.piqdef,

    Filename = binary_to_list(ErlMod) ++ "_ext.erl",

    Code = iod("\n\n", [
        [
            "-module(", ErlMod, "_ext).\n",
            "-compile(export_all).\n"
        ],
        gen_init(ErlMod),
        [ gen_parse(Mod, ErlMod, X) || X <- Defs ],
        [ gen_gen(Mod, ErlMod, X) || X <- Defs ]
    ]),
    ok = file:write_file(Filename, Code).


gen_init(ErlMod) ->
    [
        "piqi() ->\n",
        "    ", ErlMod, ":piqi().\n"
    ].


piqdef_name({piq_record, X}) -> X#piq_record.name;
piqdef_name({variant, X}) -> X#variant.name;
piqdef_name({enum, X}) -> X#variant.name;
piqdef_name({alias, X}) -> X#alias.name;
piqdef_name({piq_list, X}) -> X#piq_list.name.


piqdef_erlname({piq_record, X}) -> X#piq_record.erlang_name;
piqdef_erlname({variant, X}) -> X#variant.erlang_name;
piqdef_erlname({enum, X}) -> X#variant.erlang_name;
piqdef_erlname({alias, X}) -> X#alias.erlang_name;
piqdef_erlname({piq_list, X}) -> X#piq_list.erlang_name.


gen_parse(Mod, ErlMod, Def) ->
    Name = piqdef_name(Def),
    ErlName = piqdef_erlname(Def),
    [
        "parse_", ErlName, "(X, Format) ->\n",
        "    ", ErlMod, ":parse_", ErlName, "(\n",
        "        ", gen_convert(Mod, Name, "Format", "'pb'", "X"), ").\n\n"
    ].


gen_gen(Mod, ErlMod, Def) ->
    Name = piqdef_name(Def),
    ErlName = piqdef_erlname(Def),
    [
        "gen_", ErlName, "(X, Format) ->\n",
        "    Iolist = ", ErlMod, ":gen_", ErlName, "(X),\n",
        "    ", gen_convert(Mod, Name, "'pb'", "Format", "iolist_to_binary(Iolist)"), ".\n\n"
    ].


gen_convert(Mod, Name, InputFormat, OutputFormat, Data) ->
    ScopedName = [ Mod, "/", Name ],
    [
        "piqirun_ext:convert(?MODULE, ",
        iod(", ", [
            ["<<\"", ScopedName, "\">>"], InputFormat, OutputFormat, Data
        ]),
        ")"
    ].


iod(_Delim, []) -> [];
iod(Delim, [H|T]) ->
    lists:foldl(fun (X, Accu) -> [Accu, Delim, X] end, H, T).


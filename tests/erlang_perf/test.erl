-module(test).
-compile(export_all).

-include("addressbook_piqi.hrl").
-include("piqi_piqi.hrl").


run() ->
    test_piqi_server(),
    test_addressbook(),
    test_piqi(),
    ok.


test_piqi_server() ->
    io:format("*** testing piqi_tools:ping() i.e. 'piqi server' roundtrip ***~n~n"),
    N = 60000,
    F = fun () -> piqi_tools:ping() end,
    test(F, N).


test_addressbook() ->
    io:format("*** testing Erlang serialization of medium objects ***~n~n"),
    Filename = "addressbook.piq.pb",

    % Read the addressbook encoded in Protobuf format
    {ok, Bytes} = file:read_file(Filename),

    Reader = fun addressbook_piqi_ext:parse_address_book/2,
    Writer = fun addressbook_piqi_ext:gen_address_book/2,

    N = 30000,

    %test_rw(Reader, Writer, 'pb', Bytes, N),
    %test_rw(Reader, Writer, 'json', Bytes, N),
    %test_rw(Reader, Writer, 'xml', Bytes, N),
    %test_rw(Reader, Writer, 'piq', Bytes, N),

    test_rw_all(Reader, Writer, Bytes, N),

    ok.


test_piqi() ->
    io:format("*** testing Erlang serialization of big objects ***~n~n"),
    Filename = "piqi.piq.pb",

    % Read the Piqi-self specification encoded in Protobuf format
    {ok, Bytes} = file:read_file(Filename),

    Reader = fun piqi_piqi_ext:parse_piqi/2,
    Writer = fun piqi_piqi_ext:gen_piqi/2,

    N = 3000,

    %test_rw(Reader, Writer, 'pb', Bytes, N),
    %test_rw(Reader, Writer, 'json', Bytes, N),
    %test_rw(Reader, Writer, 'xml', Bytes, N),
    %test_rw(Reader, Writer, 'piq', Bytes, N),

    test_rw_all(Reader, Writer, Bytes, N),

    ok.


test_rw_all(Reader, Writer, Bytes, N) ->
    Formats = ['pb', 'json', 'xml', 'piq'],
    lists:foreach(fun (X) -> test_rw(Reader, Writer, X, Bytes, N) end, Formats).


test_rw(Reader, Writer, Format, Bytes, N) ->
    io:format("size of Protobuf binary: ~w~n", [size(Bytes)]),

    % read the object into Erlang term representation
    Output = Reader(Bytes, 'pb'),

    % write the object into desired test input format
    Input = Writer(Output, Format),
    %io:format("input: ~p~n", [Input]),

    io:format("reading ~w objects...~n", [Format]),
    IRate = test_convert(Reader, Format, Input, N),
    io:format("writing ~w objects...~n", [Format]),
    ORate = test_convert(Writer, Format, Output, N),
    io:format("~w read/write rate: ~w/~w~n~n", [Format, IRate, ORate]),
    ok.


test_convert(Codec, Format, Input, N) ->
    Fun = fun () -> Codec(Input, Format) end,
    test(Fun, N).


test(Fun, N) ->
    io:format("count: ~w~n", [N]),
    {Time, _} = timer:tc(?MODULE, repeat, [Fun, N]),

    Seconds = Time / 1000000,
    PerSecond = (N * 1000000) div Time,

    io:format("time: ~w seconds~n", [Seconds]),
    io:format("rate: ~w calls per second~n~n", [PerSecond]),

    PerSecond.


repeat(_Fun, 0) -> ok;
repeat(Fun, N) -> Fun(), repeat(Fun, N-1).


repeat2(Fun, N) -> repeat_n(2, Fun, N).
repeat3(Fun, N) -> repeat_n(3, Fun, N).
repeat4(Fun, N) -> repeat_n(4, Fun, N).


repeat_n(Factor, Fun, N) ->
    SpawnFun =
        fun () ->
            spawn(?MODULE, repeat_spawned, [self(), Fun, N div Factor])
        end,
    WaitFun =
        fun () ->
            receive done -> ok end
        end,
    do_n(SpawnFun, Factor),
    do_n(WaitFun, Factor).


do_n(_Fun, 0) -> ok;
do_n(Fun, N) ->
    Fun(),
    do_n(Fun, N-1).


repeat_spawned(Parent, Fun, N) ->
    repeat(Fun, N),
    Parent ! done.


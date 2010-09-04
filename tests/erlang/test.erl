-module(test).
-compile(export_all).


run() ->
    {ok, InBytes} = file:read_file("piqi.piqi.pb"),
    Buf = piqirun:init_from_binary(InBytes),
    Piqi = piqi:parse_piqi(Buf),

    OutBytes = piqi:gen_piqi('undefined', Piqi),
    ok = file:write_file("piqi.piqi.pb.pb", OutBytes),
    ok.


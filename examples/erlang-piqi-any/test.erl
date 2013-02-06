-module(test).
-compile(export_all).

-include("person_piqi.hrl").


run([Filename]) ->
    % Read the existing person book Piq format
    {ok, Bytes} = file:read_file(Filename),

    Person = person_piqi_ext:parse_person(Bytes, 'piq'),
    io:format("~n~nErlang from Piq: ~n~n~p~n", [Person]),

    Protobuf = person_piqi_ext:gen_person(Person, 'pb'),
    PersonProtobuf = person_piqi_ext:parse_person(Protobuf, 'pb'),
    io:format("~n~nErlang from Protobuf: ~n~n~p~n", [PersonProtobuf]),

    JsonPretty = person_piqi_ext:gen_person(Person, 'json_pretty'),
    io:format("~n~nJSON pretty: ~n~n~s~n", [JsonPretty]),
    PersonJSON = person_piqi_ext:parse_person(JsonPretty, 'json'),
    io:format("~n~nErlang from JSON: ~n~n~p~n", [PersonJSON]),

    XmlPretty = person_piqi_ext:gen_person(Person, 'xml_pretty'),
    io:format("~n~nXML pretty: ~n~n~s~n", [XmlPretty]),
    PersonXML = person_piqi_ext:parse_person(XmlPretty, 'xml'),
    io:format("~n~nErlang from XML: ~n~n~p~n", [PersonXML]),

    Piq = person_piqi_ext:gen_person(Person, 'piq'),
    io:format("~n~nPiq: ~n~n~s~n", [Piq]),
    ok;

run(_) ->
    erlang:halt(1).
 

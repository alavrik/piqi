-module(io_json_xml_pb).
-compile(export_all).

-include("addressbook_piqi.hrl").


run([Filename]) ->
    % Read the existing address book in Protobuf format
    {ok, Bytes} = file:read_file(Filename),

    AddressBook = addressbook_piqi_ext:parse_address_book(Bytes, 'pb'),
    io:format("~n~nErlang: ~n~n~p~n", [AddressBook]),


    Json = addressbook_piqi_ext:gen_address_book(AddressBook, 'json'),
    io:format("~n~nJSON: ~n~n~s~n", [Json]),
    AddressBook = addressbook_piqi_ext:parse_address_book(Json, 'json'),


    Xml = addressbook_piqi_ext:gen_address_book(AddressBook, 'xml'),
    io:format("~n~nXML: ~n~n~s~n", [Xml]),
    AddressBook = addressbook_piqi_ext:parse_address_book(Xml, 'xml'),


    Piq = addressbook_piqi_ext:gen_address_book(AddressBook, 'piq'),
    io:format("~n~nPiq: ~n~n~s~n", [Piq]),
    AddressBook = addressbook_piqi_ext:parse_address_book(Piq, 'piq'),
    ok;

run(_) ->
    erlang:halt(1).
 

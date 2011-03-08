-module(test).
-compile(export_all).

-include("addressbook_piqi.hrl").


run([Filename]) ->
    % Read the existing address book.
    {ok, Bytes} = file:read_file(Filename),
    AddressBook = addressbook_piqi_ext:parse_address_book(Bytes, 'pb'),
    io:format("addressbook: ~n~p~n", [AddressBook]),
    io:nl(),
    io:nl(),
    Json = addressbook_piqi_ext:gen_address_book(AddressBook, 'json'),
    io:format("json: ~n~s~n", [Json]),
    AddressBook = addressbook_piqi_ext:parse_address_book(Json, 'json'),
    ok;

run(_) ->
    erlang:halt(1).
 

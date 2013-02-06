% ex: ft=erlang

rr(test).
{ok, Bytes} = file:read_file("person.piq"),
io:format("~n~nErlang from Piq: ~n~n"),
Person = person_piqi_ext:parse_person(Bytes, 'piq').

JsonPretty = person_piqi_ext:gen_person(Person, 'json_pretty'),
io:format("~n~nJSON pretty: ~n~n~s~n", [JsonPretty]),
io:format("~n~nErlang from JSON: ~n~n"),
PersonJSON = person_piqi_ext:parse_person(JsonPretty, 'json').

halt().

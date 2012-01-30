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

%%
%% @doc This module implements a Webmachine resource for Piqi-RPC over HTTP
%%
-module(piqi_rpc_webmachine_resource).

-compile(export_all).


-include_lib("webmachine/include/webmachine.hrl").


%-define(DEBUG, 1).
-include("debug.hrl").


% XXX: set charset=UTF-8 in "Content-Type" response header for text (Piq), Json
% and XML responses?

% XXX: append Piqi-RPC/<version> to the "Server" response header? (Webmachine
% doesn't seem to allow that)

% XXX: Return "Etag" along with response to HTTP GET (i.e. get_piqi)?


%
% Webmachine request context and callbacks
%

-record(context, {
    rpc_mod,  % Piqi-RPC skeleton module generated by piqic-erlang-rpc
    impl_mod,  % Piqi-RPC implementation module containing callback functions

    request_body :: binary()  % body of a POST request
}).


init([ImplMod, RpcMod]) ->
    ?PRINT({init, ImplMod, RpcMod}),
    Context = #context{
        rpc_mod = RpcMod,
        impl_mod = ImplMod
    },
    {init_result(), Context}.


-ifndef(DEBUG).
init_result() -> ok.
-else.
% enables Webmachine tracing
init_result() -> {trace, "/tmp"}.
-endif.


% an RPC-service can be temporarilty 'paused' e.g. during a code upgrade
% procedure
service_available(ReqData, Context) ->
    IsAvailable =
        % NOTE: using "catch" as a safeguard in case if piqi_rpc_monitor is down
        % at the moment
        case catch piqi_rpc_monitor:get_service_status(Context#context.impl_mod) of
            'active' -> true;
            _ -> false
        end,
    NewReqData =
        case IsAvailable of
            true -> ReqData;
            false ->
                set_string_error("service temporarily unavailable", ReqData)
        end,
    % if false, return 503 "Service Unavailable"
    {IsAvailable, NewReqData, Context}.


allowed_methods(ReqData, Context) ->
    ?PRINT({allowed_methods, wrq:disp_path(ReqData)}),

    % 'POST' is used for making an actual RPC call
    %
    % 'GET' is used for get_piqi request
    %
    % NOTE: we will return the same Piqi modules regardless of the URL -- this
    % makes is easier for the user
    %
    % XXX: return the base service URL somehow? Maybe using some HTTP header?
    Methods = ['POST', 'GET'],

    % otherwise, return 405 Method not allowed
    {Methods, ReqData, Context}.


malformed_request(ReqData, Context) ->
    ?PRINT(malformed_request),
    try
        % NOTE, XXX: this check is actually not necessary as having a query
        % string doesn't affect anything
        (wrq:req_qs(ReqData) =:= []) orelse
            throw_malformed("empty query string expected"),

        % check that function name doesn't contain "/" characters
        case wrq:method(ReqData) of
            'POST' ->
                FuncName = wrq:disp_path(ReqData),
                lists:member($/, FuncName) andalso
                    throw_malformed("invalid function name: " ++ FuncName);
            _ -> ok
        end,

        {_IsMalformed = false, ReqData, Context}
    catch
        {'malformed_request', Err} ->
            NewReqData = set_string_error(Err, ReqData),
            % return 400 Bad request
            {true, NewReqData, Context}
    end.


throw_malformed(X) ->
    throw({'malformed_request', X}).


% called for POST request
known_content_type(ReqData, Context) ->
    ?PRINT(known_content_type),

    % NOTE: Webmachine returns <<>> as a body even if "Content-Length" header is
    % not present.
    Body = wrq:req_body(ReqData),
    ContentType = wrq:get_req_header("content-type", ReqData),

    IsKnownType =
        case ContentType of
            "application/json" -> true;
            "application/xml" -> true;
            "application/x-protobuf" -> true;
            % allow content-type to be undefined when the body is empty
            'undefined' when Body == <<>> -> true;
            "" when Body == <<>> -> true;
            _ -> false
        end,

    % distinguish between "no body" and "empty body" cases: when communicating
    % using Protocol Buffers, empty body can appear when an empty list or an
    % empty record is sent as the input parameter
    RealBody =
        case Body of
            <<>> when ContentType =:= 'undefined';
                      ContentType =:= "";
                      ContentType =/= "application/x-protobuf" ->
                'undefined'; % no body
            _ -> Body
        end,
    NewContext = Context#context{ request_body = RealBody },

    % otherwise, return 415 Unsupported media type
    {IsKnownType, ReqData, NewContext}.


% Called for both GET and POST to find a match between provided content-types
% and "Accept" header.
%
% In addition to that, dispatches GET requests (the only GET request we support
% is to get the list of Piqi modules -- we can return it all sorts of different
% formats).
content_types_provided(ReqData, Context) ->
    ?PRINT(known_content_type),
    % for all other types 406 Not Acceptable will be returned
    ContentTypes =
        case wrq:method(ReqData) of
            'GET' -> % get Piqi: by default return it in Piq format
                [
                    {"text/plain", get_piqi_piq},
                    {"application/json", get_piqi_json},
                    {"application/xml", get_piqi_xml},
                    {"application/x-protobuf", get_piqi_pb}
                ];
            'POST' ->
                [
                    {"application/json", unused_value},
                    {"application/xml", unused_value},
                    {"application/x-protobuf", unused_value},
                    {"text/plain", unused_value}
                ]
        end,
    {ContentTypes, ReqData, Context}.


% process POST requests
process_post(ReqData, Context) ->
    ContentType = wrq:get_req_header("content-type", ReqData),
    InputFormat = content_type_to_format(ContentType),
    rpc(ReqData, Context, InputFormat).


%
% Utility functions
%

format_to_content_type('pb') -> "application/x-protobuf";
format_to_content_type('json') -> "application/json";
format_to_content_type('xml') -> "application/xml";
format_to_content_type('piq') -> "text/plain".


content_type_to_format("application/x-protobuf") -> 'pb';
content_type_to_format("application/json") -> 'json';
content_type_to_format("application/xml") -> 'xml';
content_type_to_format("text/plain") -> 'piq';
content_type_to_format(_) -> 'undefined'.


set_data_response(Data, OutputFormat, ReqData) ->
    wrq:set_resp_header(
        "Content-Type", format_to_content_type(OutputFormat),
        wrq:set_resp_body(Data, ReqData)).


set_string_error(Str, ReqData) when is_binary(Str) ->
    wrq:set_resp_header("Content-Type", "text/plain",
        wrq:set_resp_body(["Piqi-RPC: ", Str], ReqData));

set_string_error(Str, ReqData) when is_list(Str) ->
    set_string_error(list_to_binary(Str), ReqData).


%
% Main RPC method handlers
%

get_piqi_piq(ReqData, Context) ->
    get_piqi(ReqData, Context, 'piq').


get_piqi_json(ReqData, Context) ->
    get_piqi(ReqData, Context, 'json').


get_piqi_xml(ReqData, Context) ->
    get_piqi(ReqData, Context, 'xml').


get_piqi_pb(ReqData, Context) ->
    get_piqi(ReqData, Context, 'pb').


get_piqi(ReqData, Context, OutputFormat) ->
    RpcMod = Context#context.rpc_mod,
    Body = RpcMod:get_piqi(OutputFormat),
    {Body, ReqData, Context}.


rpc(ReqData, Context, InputFormat) ->
    FuncName = list_to_binary(wrq:disp_path(ReqData)),
    ?PRINT({rpc, FuncName, InputFormat}),

    OutputFormat =
        case wrq:get_req_header("accept", ReqData) of
            'undefined' when InputFormat == 'undefined' ->
                % if the function doesn't specify "Content-Type" or "Accept"
                % headers, use 'json' as the default output format
                'json';
            'undefined' ->
                % if there's no "Accept" header, but the "Content-Type", e.g.
                % input type is defined then use it as the output format
                InputFormat;
            _ ->
                % determine output format based on "Accept" header: this has
                % already been figured out by Webmachine earlier, and we just
                % need to read the value
                RespContentType = wrq:get_resp_header("Content-Type", ReqData),
                content_type_to_format(RespContentType)
        end,

    RpcMod = Context#context.rpc_mod,
    ImplMod = Context#context.impl_mod,

    % this field is initialized above by the known_content_type/2 function
    InputData = Context#context.request_body,

    % make the "ReqData" handle available to the server implementation in case
    % it needs to analyze HTTP headers or some other parts of the original HTTP
    % request
    erlang:put(wrq, ReqData), % I guess "wrq" stands for "Webmachine request"

    % make the actual call
    RpcResponse = RpcMod:rpc(ImplMod, FuncName, InputData, InputFormat, OutputFormat),

    case RpcResponse of
        ok ->
            % return empty 204 "No Content"
            % NOTE: we don't need Content-Type for empty response which
            % Webmachine inserts by default
            NewReqData = wrq:remove_resp_header("Content-Type", ReqData),
            {true, NewReqData, Context};

        {ok, OutputData} ->
            % return 200 OK
            NewReqData = set_data_response(OutputData, OutputFormat, ReqData),
            {true, NewReqData, Context};

        {error, ErrorData} -> % application error
			% If the HTTP header "X-Piqi-RPC-return-http-status-via-header" is 
			% set to "true" then return 200 OK. Solves problems for CORS/JSONP.   
            % Otherwise return 500 "Internal Server Error" with the structured error
            % desciption formatted according to the desired output format
            %
            % NOTE, XXX: {error, } and {'prc_error', {'internal_error', _}} use
            % the same 500 HTTP status code. The the only difference between
            % them is the "Content-Type" header which is set to "text/plain" in
            % the latter case.
            NewReqData = set_data_response(ErrorData, OutputFormat, ReqData),
			
			case wrq:get_req_header("X-Piqi-RPC-return-http-status-via-header", ReqData) of
				"true" ->
						NewReqDataWithHttpStatus = wrq:set_resp_header("X-Piqi-RPC-http-status", "500", NewReqData),
						{true, NewReqDataWithHttpStatus, Context};
					_ ->
						{{halt, 500}, NewReqData, Context}
			end;

        % input-related errors:
        {'rpc_error', 'unknown_function'} ->
            % return 404 "Not Found"
            NewReqData =
                set_string_error("unknown function: " ++ FuncName, ReqData),
            {{halt, 404}, NewReqData, Context};

        {'rpc_error', 'missing_input'} ->
            % return 411 "Length required"
            NewReqData = set_string_error("non-empty input expected", ReqData),
            {{halt, 411}, NewReqData, Context};

        {'rpc_error', {'invalid_input', Err}} ->
            % return 400 "Bad Request"
            NewReqData = set_string_error(Err, ReqData),
            {{halt, 400}, NewReqData, Context};

        % server errors:
        {'rpc_error', {'invalid_output', Err}} ->
            % return 502 "Bad Gateway"
            NewReqData = set_string_error(Err, ReqData),
            {{halt, 502}, NewReqData, Context};

        {'rpc_error', {'internal_error', Err}} ->
            % return 500 "Internal Server Error" NOTE: using the same status
            % code as for application error, but different Content-Type. See the
            % "{error, _}" case for the details.
            NewReqData = set_string_error(Err, ReqData),
            {{halt, 500}, NewReqData, Context};

        {'rpc_error', {'service_unavailable', Err}} ->
            % return 503 "Service Unavailable"
            NewReqData = set_string_error(Err, ReqData),
            {{halt, 503}, NewReqData, Context}

        % NOTE: Piqi-RPC over HTTP never generates this one because protocol
        % validation is taken care of by the HTTP server
        %{'rpc_error', {'protocol_error', Err}}
  end.


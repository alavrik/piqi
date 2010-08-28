%% Copyright 2009, 2010 Anton Lavrik
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
%% This code is based on Protobuffs library.
%% The original code was taken from here:
%%      http://github.com/ngerakines/erlang_protobuffs
%%
%% Below is the original copyright notice and the license:
%%
%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

%%
%% @doc Piqi runtime library
%%
-module(piqirun).
-compile(export_all).


-define(TYPE_VARINT, 0).
-define(TYPE_64BIT, 1).
-define(TYPE_STRING, 2).
-define(TYPE_START_GROUP, 3).
-define(TYPE_END_GROUP, 4).
-define(TYPE_32BIT, 5).


% TODO: tags


%% @hidden
encode_field_tag(Code, FieldType) when Code band 16#3fffffff =:= Code ->
    encode_varint((Code bsl 3) bor FieldType).


%% @hidden
encode_varint_field(Code, Integer) ->
    [encode_field_tag(Code, ?TYPE_VARINT), encode_varint(Integer)].


%% @hidden
encode_varint(I) ->
    encode_varint(I, []).


%% @hidden
encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)),
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint(First_X_Bits, [With_Leading_Bit|Acc]).


%% @hidden
decode_varint(Bytes) ->
    decode_varint(Bytes, []).
decode_varint(<<0:1, I:7, Rest/binary>>, Acc) ->
    Acc1 = [I|Acc],
    Result = 
        lists:foldl(
            fun(X, Acc0) ->
                (Acc0 bsl 7 bor X)
            end, 0, Acc1),
    {Result, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Acc) ->
    decode_varint(Rest, [I | Acc]).



integer_to_varint(Code, X) when X >= 0 ->
    integer_to_signed_varint(Code, X).

integer_to_signed_varint(Code, X) ->
    encode_varint_field(Code, X).


integer_to_zigzag_varint(Code, X) when X >= 0 ->
    encode_varint_field(Code, X bsl 1);
integer_to_zigzag_varint(Code, X) -> % when  X < 0
    encode_varint_field(Code, bnot (X bsl 1)).


boolean_to_varint(Code, true) ->
    encode_varint_field(Code, 1);
boolean_to_varint(Code, false) ->
    encode_varint_field(Code, 0).


integer_to_fixed32(Code, X) when X >= 0 ->
    integer_to_signed_fixed32(Code, X).

integer_to_signed_fixed32(Code, X) ->
    [encode_field_tag(Code, ?TYPE_32BIT), <<X:32/little-integer>>].


integer_to_fixed64(Code, X) when X >= 0 ->
    integer_to_signed_fixed64(Code, X).

integer_to_signed_fixed64(Code, X) ->
    [encode_field_tag(Code, ?TYPE_64BIT), <<X:64/little-integer>>].


float_to_fixed64(Code, X) when is_float(X) ->
    [encode_field_tag(Code, ?TYPE_64BIT), <<X:64/little-float>>];
float_to_fixed64(Code, X) when is_integer(X) ->
    float_to_fixed64(Code, X + 0.0).


float_to_fixed32(Code, X) when is_float(X) ->
    [encode_field_tag(Code, ?TYPE_32BIT), <<X:32/little-float>>];
float_to_fixed32(Code, X) when is_integer(X) ->
    float_to_fixed32(Code, X + 0.0).


binary_to_block(Code, X) when is_binary(X) ->
    [encode_field_tag(Code, ?TYPE_STRING), encode_varint(size(X)), X];
binary_to_block(Code, X) when is_list(X) ->
    binary_to_block(Code, list_to_binary(X)).


%
% Decoders and parsers
%

parse_field_header(Bytes) ->
    {Tag, Rest} = decode_varint(Bytes),
    Code = Tag bsr 3,
    WireType = Tag band 7,
    {Code, WireType, Rest}.


parse_field(Bytes) ->
    {FieldCode, WireType, Content} = parse_field_header(Bytes),
    {FieldValue, Rest} =
        case WireType of
            ?TYPE_VARINT -> decode_varint(Content);
            ?TYPE_STRING ->
                {Length, R1} = decode_varint(Content),
                {Value, R2} = split_binary(R1, Length),
                {{'block', Value}, R2};
            ?TYPE_64BIT ->
                split_binary(Content, 8);
            ?TYPE_32BIT ->
                split_binary(Content, 4)
        end,
    {{FieldCode, FieldValue}, Rest}.


parse_record({'block', Bytes}) ->
    parse_record_buf(Bytes, []).


parse_record_buf(<<>>, Accu) ->
    lists:reverse(Accu);
parse_record_buf(Bytes, Accu) ->
    {Value, Rest} = parse_field(Bytes),
    parse_record_buf(Rest, [Value | Accu]).


parse_variant(Bytes) ->
    [Res] = parse_record(Bytes),
    Res.


integer_of_varint(X) when is_integer(X) -> X.


integer_of_signed_varint(X)
        when is_integer(X) andalso (X band 16#8000000000000000 =/= 0) ->
    X - 16#10000000000000000;
integer_of_signed_varint(X) -> X.


integer_of_zigzag_varint(X) when is_integer(X) ->
    (X bsr 1) bxor (-(X band 1)).


boolean_of_varint(1) -> true;
boolean_of_varint(0) -> false.


integer_of_fixed32(<<X:32/little-unsigned-integer>>) -> X.

integer_of_signed_fixed32(<<X:32/little-signed-integer>>) -> X.


integer_of_fixed64(<<X:64/little-unsigned-integer>>) -> X.

integer_of_signed_fixed64(<<X:64/little-signed-integer>>) -> X.


float_of_fixed64(<<X:64/little-float>>) -> X + 0.0.

float_of_fixed32(<<X:32/little-float>>) -> X + 0.0.


binary_of_block({'block', X}) -> X.


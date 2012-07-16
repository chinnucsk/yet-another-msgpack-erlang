-module(msgpack_proper).

-export([positive_fixnum/0,
         negative_fixnum/0,
         int8/0,
         int16/0,
         int32/0,
         int64/0,
         uint8/0,
         uint16/0,
         uint32/0,
         uint64/0,
         nil/0,
         raw16/0,
         raw32/0]).

-include_lib("proper/include/proper.hrl").

positive_fixnum() ->
    choose(0, 127).

negative_fixnum() ->
    choose(-32, -1).

int8() ->
    choose(-16#80, 16#7F).

int16() ->
    choose(-16#8000, 16#7FFF).

int32() ->
    choose(-16#80000000, 16#7FFFFFFF).

int64() ->
    choose(-16#8000000000000000, 16#7FFFFFFFFFFFFFFF).

uint8() ->
    choose(0, 16#FF).

uint16() ->
    choose(0, 16#FFFF).

uint32() ->
    choose(0, 16#FFFFFFFF).

uint64() ->
    choose(0, 16#FFFFFFFFFFFFFFFF).

nil() ->
    nil.

%% boolean/0
%% binary/0 -> binary(choose(0, 31))

raw16() ->
    ?LET(Integer, uint16(),
        ?LET(Binary, binary(Integer), Binary)).

raw32() ->
    ?LET(Integer, uint32(),
        ?LET(Binary, binary(Integer), Binary)).

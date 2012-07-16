-module(msgpack_proper).

-export([choose_type/0]).

-include_lib("proper/include/proper.hrl").

choose_type() ->
    oneof([positive_fixnum(), negative_fixnum(),
           int8(), int16(), int32(), int64(),
           uint8(), uint16(), uint32(), uint64(),
           nil(), boolean(),
           fix_raw(), raw16(), raw32(),
           fix_array(),
           fix_map()]).

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

fix_array() ->
    ?LET(Integer, choose(0, 15),
         proper_gen:list_gen(Integer, choose_type())).

%% array16() ->
%%     ?LET(Integer, choose(16, 16#FFFF),
%%          proper_gen:list_gen(Integer, choose_type())).

%% array32() ->
%%     ?LET(Integer, choose(16#FFFF, 16#010000),
%%          proper_gen:list_gen(Integer, choose_type())).

fix_map() ->
    ?LET(Integer, choose(0, 15),
         {proper_gen:list_gen(Integer, {choose_type(), choose_type()})}).

%% map16() ->
%%     ?LET(Integer, choose(16, 16#FFFF),
%%         proper_gen:list_gen(Integer, {choose_type(), choose_type()})).

%% map32() ->
%%     ?LET(Integer, choose(16#FFFF, 16#010000),
%%         proper_gen:list_gen(Integer, {choose_type(), choose_type()})).

fix_raw() ->
    ?LET(Integer, choose(0, 31),
        ?LET(Binary, binary(Integer), Binary)).

raw16() ->
    ?LET(Integer, uint16(),
        ?LET(Binary, binary(Integer), Binary)).

raw32() ->
    ?LET(Integer, uint32(),
        ?LET(Binary, binary(Integer), Binary)).

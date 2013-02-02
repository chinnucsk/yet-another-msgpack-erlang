-module(msgpack).

-export([pack/1, unpack/1]).

-export([pack_term/1, unpack_term/1]).

-include_lib("eunit/include/eunit.hrl").

-type msgpack_map() :: {[{msgpack_term(), msgpack_term()}]}.
-type msgpack_array() :: [msgpack_term()].
-type msgpack_term() :: msgpack_array() | msgpack_map() | integer() | float() | binary().

-spec unpack(binary()) -> msgpack_term().
unpack(Binary) when is_binary(Binary) ->
    unpack(Binary, []);
unpack(Term) ->
    error({badarg, Term}).

unpack(<<>>, []) ->
    error(incomplete);
unpack(<<>>, [Term]) ->
    Term;
unpack(<<>>, Acc) ->
    lists:reverse(Acc);
unpack(Binary, Acc) ->
    case unpack_term(Binary) of
        {ok, Term, Rest} ->
            unpack(Rest, [Term|Acc]);
        {error, Reason} ->
            error(Reason)
    end.

-spec unpack_term(binary()) -> {ok, msgpack_term(), binary()} | {error, term()}.
%% atom
unpack_term(<<16#C0, Rest/binary>>) ->
    {ok, nil, Rest};
unpack_term(<<16#C2, Rest/binary>>) ->
    {ok, false, Rest};
unpack_term(<<16#C3, Rest/binary>>) ->
    {ok, true, Rest};
%% float
unpack_term(<<16#CA, V:32/float-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_term(<<16#CB, V:64/float-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
%% unsigned integer
unpack_term(<<16#CC, V:8/unsigned-integer, Rest/binary>>) ->
    {ok, V, Rest};
unpack_term(<<16#CD, V:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_term(<<16#CE, V:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_term(<<16#CF, V:64/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
%% signed integer
unpack_term(<<16#D0, V:8/signed-integer, Rest/binary>>) ->
    {ok, V, Rest};
unpack_term(<<16#D1, V:16/big-signed-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_term(<<16#D2, V:32/big-signed-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_term(<<16#D3, V:64/big-signed-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
%% raw byte
unpack_term(<<16#DA, L:16/unsigned-integer-unit:1, V:L/binary, Rest/binary>>) ->
    {ok, V, Rest};
unpack_term(<<16#DB, L:32/unsigned-integer-unit:1, V:L/binary, Rest/binary>>) ->
    {ok, V, Rest};
%% array
unpack_term(<<16#DC, L:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_array(Rest, L, []);
unpack_term(<<16#DD, L:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_array(Rest, L, []);
%% map
unpack_term(<<16#DE, L:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_map(Rest, L, []);
unpack_term(<<16#DF, L:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_map(Rest, L, []);
%% Tag-encoded lengths (kept last, for speed)
unpack_term(<<0:1, V:7, Rest/binary>>) ->
    {ok, V, Rest};
%% negative int
unpack_term(<<2#111:3, V:5, Rest/binary>>) ->
    {ok, V - 2#100000, Rest};
% raw bytes
unpack_term(<<2#101:3, L:5, V:L/binary, Rest/binary>>) ->
    {ok, V, Rest};
%% array
unpack_term(<<2#1001:4, L:4, Rest/binary>>) ->
    unpack_array(Rest, L, []);
%% map
unpack_term(<<2#1000:4, L:4, Rest/binary>>) ->
    unpack_map(Rest, L, []);
%% Invalid data
unpack_term(Binary) ->
    case binary:first(Binary) of
        16#C1 ->
            {error, {badarg, Binary}};
        16#C4 ->
            {error, {badarg, Binary}};
        16#C5 ->
            {error, {badarg, Binary}};
        16#C6 ->
            {error, {badarg, Binary}};
        16#C7 ->
            {error, {badarg, Binary}};
        16#C8 ->
            {error, {badarg, Binary}};
        16#C9 ->
            {error, {badarg, Binary}};
        16#D4 ->
            {error, {badarg, Binary}};
        16#D5 ->
            {error, {badarg, Binary}};
        16#D6 ->
            {error, {badarg, Binary}};
        16#D7 ->
            {error, {badarg, Binary}};
        16#D8 ->
            {error, {badarg, Binary}};
        16#D9 ->
            {error, {badarg, Binary}};
        _ ->
            {error, incomplete}
    end.

-spec unpack_array(binary(), non_neg_integer(), [msgpack_term()]) -> {ok, [msgpack_term()], binary()} | {error, term()}.
unpack_array(Rest, 0, Acc) ->
    {ok, lists:reverse(Acc), Rest};
unpack_array(Binary, Len, Acc) ->
    case unpack_term(Binary) of
        {ok, Term, Rest} ->
            unpack_array(Rest, Len - 1, [Term|Acc]);
        Error ->
            Error
    end.

-spec unpack_map(binary(), non_neg_integer(), msgpack_map()) -> {ok, msgpack_map(), binary()} | {error, term()}.
unpack_map(Rest, 0, Acc) ->
    {ok, {lists:reverse(Acc)}, Rest};
unpack_map(Binary, Len, Acc) ->
    case unpack_term(Binary) of
        {ok, Key, Rest1} ->
            case unpack_term(Rest1) of
                {ok, Value, Rest2} ->
                    unpack_map(Rest2, Len - 1, [{Key, Value}|Acc]);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec pack(msgpack_term() | [msgpack_term()]) -> binary().
pack(Terms) ->
    pack_term(Terms).

-spec pack_term(msgpack_term()) -> <<_:8, _:_*8>>.
pack_term(I) when is_integer(I) andalso I < 0 ->
    pack_int(I);
pack_term(I) when is_integer(I) ->
    pack_uint(I);
pack_term(F) when is_float(F) ->
    pack_double(F);
pack_term(nil) ->
    <<16#C0:8>>;
pack_term(true) ->
    <<16#C3:8>>;
pack_term(false) ->
    <<16#C2:8>>;
pack_term(Bin) when is_binary(Bin) ->
    pack_raw(Bin);
pack_term({[] = Map}) ->
    pack_map(Map);
pack_term({[{_, _}|_] = Map}) ->
    pack_map(Map);
pack_term(List) when is_list(List) ->
    pack_array(List);
pack_term(Term) ->
    error({badarg, Term}).


-spec pack_uint(non_neg_integer()) -> {ok, <<_:8,_:_*8>>}.
%% positive fixnum
pack_uint(N) when N =< 16#7F ->
    <<2#0:1, N:7>>;
%% uint 8
pack_uint(N) when N =< 16#FF ->
    <<16#CC:8, N:8>>;
%% uint 16
pack_uint(N) when N =< 16#FFFF ->
    <<16#CD:8, N:16/big-unsigned-integer-unit:1>>;
%% uint 32
pack_uint(N) when N < 16#FFFFFFFF ->
    <<16#CE:8, N:32/big-unsigned-integer-unit:1>>;
%% uint 64
pack_uint(N) ->
    <<16#CF:8, N:64/big-unsigned-integer-unit:1>>.

-spec pack_int(integer()) -> {ok, <<_:8,_:_*8>>}.
%% negative fixnum
pack_int(N) when N >= -32 ->
    <<2#111:3, N:5>>;
%% int 8
pack_int(N) when N >= -16#80 ->
    <<16#D0:8, N:8/big-signed-integer-unit:1>>;
%% int 16
pack_int(N) when N >= -16#8000 ->
    <<16#D1:8, N:16/big-signed-integer-unit:1>>;
%% int 32
pack_int(N) when N >= -16#80000000 ->
    <<16#D2:8, N:32/big-signed-integer-unit:1>>;
%% int 64
pack_int(N) ->
    <<16#D3:8, N:64/big-signed-integer-unit:1>>.

-spec pack_double(float()) -> <<_:72>>.
%% float : erlang's float is always IEEE 754 64bit format.
%% pack_float(F) when is_float(F)->
%%    << 16#CA:8, F:32/big-float-unit:1 >>.
%%    pack_double(F).
%% double
pack_double(F) ->
    <<16#CB:8, F:64/big-float-unit:1>>.

-spec pack_raw(binary()) -> <<_:8, _:_*8>>.
%% raw bytes
pack_raw(Bin) ->
    case byte_size(Bin) of
	Len when Len < 32 ->
            <<2#101:3, Len:5, Bin/binary>>;
	Len when Len =< 16#FFFF ->
            <<16#DA:8, Len:16/big-unsigned-integer-unit:1, Bin/binary>>;
	Len ->
            <<16#DB:8, Len:32/big-unsigned-integer-unit:1, Bin/binary>>
    end.

-spec pack_array([msgpack_term()]) -> binary().
%% list
pack_array(L) ->
    Binary = << <<(pack_term(Term))/binary>> || Term  <- L >>,
    case length(L) of
        Len when Len < 16 ->
            <<2#1001:4, Len:4/integer-unit:1, Binary/binary>>;
        Len when Len =< 16#FFFF -> 
            <<16#DC:8, Len:16/big-unsigned-integer-unit:1, Binary/binary>>;
        Len ->
            <<16#DD:8, Len:32/big-unsigned-integer-unit:1, Binary/binary>>
    end.

-spec pack_map(msgpack_map()) -> {ok, <<_:8, _:_*8>>} | {error, {badarg, term()}}.
pack_map(M)->
    Binary = << <<(pack_term(Key))/binary,
                  (pack_term(Value))/binary>> || {Key, Value} <- M >>,
    case length(M) of
        Len when Len < 16 ->
            <<2#1000:4, Len:4/integer-unit:1, Binary/binary>>;
        %% 65536
        Len when Len =< 16#FFFF ->
            <<16#DE:8, Len:16/big-unsigned-integer-unit:1, Binary/binary>>;
        Len ->
            <<16#DF:8, Len:32/big-unsigned-integer-unit:1, Binary/binary>>
    end.

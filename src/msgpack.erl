-module(msgpack).

-export([pack/1, unpack/1]).

-include_lib("eunit/include/eunit.hrl").

-type msgpack_map() :: [{msgpack_term(), msgpack_term()}].
-type msgpack_array() :: [msgpack_term()].
-type msgpack_term() :: msgpack_array() | msgpack_map() | integer() | float() | binary().

-spec unpack(binary()) -> {ok, msgpack_term() | [msgpack_term()]} | {error, term()}.
unpack(Binary) when is_binary(Binary) ->
    unpack(Binary, []);
unpack(Term) ->
    {error, {badarg, Term}}.

unpack(<<>>, []) ->
    {error, {incomplete, <<>>}};
unpack(<<>>, [Term]) ->
    {ok, Term};
unpack(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
unpack(Binary, Acc) ->
    case unpack_(Binary) of
        {ok, Term, Rest} ->
            unpack(Rest, [Term|Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec unpack_(binary()) -> {ok, msgpack_term(), binary()} | {error, term()}.
%% atom
unpack_(<<16#C0, Rest/binary>>) ->
    {ok, nil, Rest};
unpack_(<<16#C2, Rest/binary>>) ->
    {ok, false, Rest};
unpack_(<<16#C3, Rest/binary>>) ->
    {ok, true, Rest};
%% float
unpack_(<<16#CA, V:32/float-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_(<<16#CB, V:64/float-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
%% unsigned integer
unpack_(<<16#CC, V:8/unsigned-integer, Rest/binary>>) ->
    {ok, V, Rest};
unpack_(<<16#CD, V:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_(<<16#CE, V:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_(<<16#CF, V:64/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
%% signed integer
unpack_(<<16#D0, V:8/signed-integer, Rest/binary>>) ->
    {ok, V, Rest};
unpack_(<<16#D1, V:16/big-signed-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_(<<16#D2, V:32/big-signed-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
unpack_(<<16#D3, V:64/big-signed-integer-unit:1, Rest/binary>>) ->
    {ok, V, Rest};
%% raw byte
unpack_(<<16#DA, L:16/unsigned-integer-unit:1, V:L/binary, Rest/binary>>) ->
    {ok, V, Rest};
unpack_(<<16#DB, L:32/unsigned-integer-unit:1, V:L/binary, Rest/binary>>) ->
    {ok, V, Rest};
%% array
unpack_(<<16#DC, L:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_array(Rest, L, []);
unpack_(<<16#DD, L:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_array(Rest, L, []);
%% map
unpack_(<<16#DE, L:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_map(Rest, L, []);
unpack_(<<16#DF, L:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_map(Rest, L, []);
%% Tag-encoded lengths (kept last, for speed)
unpack_(<<0:1, V:7, Rest/binary>>) ->
    {ok, V, Rest};
%% negative int
unpack_(<<2#111:3, V:5, Rest/binary>>) ->
    {ok, V - 2#100000, Rest};
% raw bytes
unpack_(<<2#101:3, L:5, V:L/binary, Rest/binary>>) ->
    {ok, V, Rest};
%% array
unpack_(<<2#1001:4, L:4, Rest/binary>>) ->
    unpack_array(Rest, L, []);
%% map
unpack_(<<2#1000:4, L:4, Rest/binary>>) ->
    unpack_map(Rest, L, []);
%% Invalid data
unpack_(Binary) ->
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
            {error, {incomplete, Binary}}
    end.

-spec unpack_array(binary(), non_neg_integer(), [msgpack_term()]) -> {ok, [msgpack_term()], binary()} | {error, term()}.
unpack_array(Rest, 0, Acc) ->
    {ok, lists:reverse(Acc), Rest};
unpack_array(Binary, Len, Acc) ->
    case unpack_(Binary) of
        {ok, Term, Rest} ->
            unpack_array(Rest, Len - 1, [Term|Acc]);
        Error ->
            Error
    end.

-spec unpack_map(binary(), non_neg_integer(), msgpack_map()) -> {ok, msgpack_map(), binary()} | {error, term()}.
unpack_map(Rest, 0, Acc) ->
    {ok, lists:reverse(Acc), Rest};
unpack_map(Binary, Len, Acc) ->
    case unpack_(Binary) of
        {ok, Key, Rest1} ->
            case unpack_(Rest1) of
                {ok, Value, Rest2} ->
                    unpack_map(Rest2, Len - 1, [{Key, Value}|Acc]);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec pack(msgpack_term() | [msgpack_term()]) -> {ok, binary()} | {error, term()}.
pack(<<>>) ->
    {error, {incomplete, <<>>}};
pack(Terms) ->
    case pack_(Terms) of
        {ok, Binary} ->
            {ok, Binary};
        {error, _Reason} when is_list(Terms) ->
            pack(Terms, []);
        {error, Reason} ->
            {error, Reason}
    end.

pack([], Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
pack([Term|Rest], Acc) ->
    case pack_(Term) of
        {ok, Binary} ->
            pack(Rest, [Binary|Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec pack_(msgpack_term()) -> binary() | no_return().
pack_(I) when is_integer(I) andalso I < 0 ->
    pack_int_(I);
pack_(I) when is_integer(I) ->
    pack_uint_(I);
pack_(F) when is_float(F) ->
    pack_double(F);
pack_(nil) ->
    {ok, <<16#C0:8>>};
pack_(true) ->
    {ok, <<16#C3:8>>};
pack_(false) ->
    {ok, <<16#C2:8>>};
pack_(Bin) when is_binary(Bin) ->
    pack_raw(Bin);
pack_([{_, _}|_] = Map) ->
    pack_map(Map);
pack_(List) when is_list(List) ->
    pack_array(List);
pack_(Term) ->
    {error, {badarg, Term}}.


-spec pack_uint_(non_neg_integer()) -> binary().
%% positive fixnum
pack_uint_(N) when N =< 16#7F ->
    {ok, <<2#0:1, N:7>>};
%% uint 8
pack_uint_(N) when N =< 16#FF ->
    {ok, <<16#CC:8, N:8>>};
%% uint 16
pack_uint_(N) when N =< 16#FFFF ->
    {ok, <<16#CD:8, N:16/big-unsigned-integer-unit:1>>};
%% uint 32
pack_uint_(N) when N < 16#FFFFFFFF ->
    {ok, <<16#CE:8, N:32/big-unsigned-integer-unit:1>>};
%% uint 64
pack_uint_(N) ->
    {ok, <<16#CF:8, N:64/big-unsigned-integer-unit:1>>}.

-spec pack_int_(integer()) -> binary().
%% negative fixnum
pack_int_(N) when N >= -32->
    {ok, <<2#111:3, N:5>>};
%% int 8
pack_int_(N) when N > -128 ->
    {ok, <<16#D0:8, N:8/big-signed-integer-unit:1>>};
%% int 16
pack_int_(N) when N > -32768 ->
    {ok, <<16#D1:8, N:16/big-signed-integer-unit:1>>};
%% int 32
pack_int_(N) when N > -16#FFFFFFFF ->
    {ok, <<16#D2:8, N:32/big-signed-integer-unit:1>>};
%% int 64
pack_int_(N) ->
    {ok, <<16#D3:8, N:64/big-signed-integer-unit:1>>}.

-spec pack_double(float()) -> binary().
%% float : erlang's float is always IEEE 754 64bit format.
%% pack_float(F) when is_float(F)->
%%    << 16#CA:8, F:32/big-float-unit:1 >>.
%%    pack_double(F).
%% double
pack_double(F) ->
    {ok, <<16#CB:8, F:64/big-float-unit:1>>}.

-spec pack_raw(binary()) -> binary().
%% raw bytes
pack_raw(Bin) ->
    case byte_size(Bin) of
	Len when Len < 6 ->
            {ok, <<2#101:3, Len:5, Bin/binary>>};
	Len when Len =< 16#FFFF ->
            {ok, <<16#DA:8, Len:16/big-unsigned-integer-unit:1, Bin/binary>>};
	Len ->
            {ok, <<16#DB:8, Len:32/big-unsigned-integer-unit:1, Bin/binary>>}
    end.

-spec pack_array([msgpack_term()]) -> {ok, binary()} | {error, term()}.
%% list
pack_array(L) ->
    case pack_array_(L, []) of
        {ok, Binary} ->
            case length(L) of
                Len when Len < 16 ->
                    {ok, <<2#1001:4, Len:4/integer-unit:1, Binary/binary>>};
                Len when Len =< 16#FFFF -> 
                    {ok, <<16#DC:8, Len:16/big-unsigned-integer-unit:1, Binary/binary>>};
                Len ->
                    {ok, <<16#DD:8, Len:32/big-unsigned-integer-unit:1, Binary/binary>>}
            end;
        Error ->
            Error
    end.

pack_array_([], Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
pack_array_([Term|Rest], Acc) ->
    case pack_(Term) of
        {ok, Binary} ->
            pack_array_(Rest, [Binary|Acc]);
        Error ->
            Error
    end.

-spec pack_map(msgpack_map()) -> binary() | no_return().
pack_map(M)->
    case pack_map_(M, []) of
        {ok, Binary} ->
            case length(M) of
                Len when Len < 16 ->
                    {ok, <<2#1000:4, Len:4/integer-unit:1, Binary/binary>>};
                %% 65536
                Len when Len =< 16#FFFF ->
                    {ok, <<16#DE:8, Len:16/big-unsigned-integer-unit:1, Binary/binary>>};
                Len ->
                    {ok, <<16#DF:8, Len:32/big-unsigned-integer-unit:1, Binary/binary>>}
            end;
        Error ->
            Error
    end.

pack_map_([], Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
pack_map_([{Key, Value}|Rest], Acc) ->
    case pack_(Key) of
        {ok, KeyBinary} ->
            case pack_(Value) of
                {ok, ValueBinary} ->
                    pack_map_(Rest, [ValueBinary, KeyBinary|Acc]);
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
pack_map_([Term|_Rest], _Acc) ->
    {error, {badarg, Term}}.

-ifdef(TEST).

test_([]) -> 0;
test_([Term|Rest])->
    {ok, Pack} = msgpack:pack(Term),
    ?assertEqual({ok, Term}, msgpack:unpack(Pack)),
    1 + test_(Rest).

test_data()->
    [true, false, nil,
     0, 1, 2, 123, 512, 1230, 678908, 16#FFFFFFFFFF,
     -1, -23, -512, -1230, -567898, -16#FFFFFFFFFF,
     123.123, -234.4355, 1.0e-34, 1.0e64,
     [23, 234, 0.23],
     <<"hogehoge">>, <<"243546rf7g68h798j", 0, 23, 255>>,
     <<"hoasfdafdas][">>,
     [0,42, <<"sum">>, [1,2]], [1,42, nil, [3]],
     -234, -40000, -16#10000000, -16#100000000,
     42
    ].

basic_test()->
    Tests = test_data(),
    Passed = test_(Tests),
    Passed = length(Tests).

array_test_()->
    [
        {"length 16",
            fun() ->
                    Longer = lists:seq(0, 655),
                    {ok, Binary} = msgpack:pack(Longer),
                    ?assertEqual({ok, Longer}, msgpack:unpack(Binary))
            end},
        {"length 32",
            fun() ->
                    Longer = lists:seq(0, 100000),
                    {ok, Binary} = msgpack:pack(Longer),
                    ?assertEqual({ok, Longer}, msgpack:unpack(Binary))
            end}
    ].


map_test_()->
    [
        {"map",
            fun() ->
                    Ints = lists:seq(0, 65),
                    Map = [ {X, X*2} || X <- Ints ] ++ [{<<"hage">>, 324}, {43542, [nil, true, false]}],
                    {ok, Bin1} = msgpack:pack(Map),
                    ?assertEqual({ok, Map}, msgpack:unpack(Bin1))
            end},
        {"empty map is empty list ...",
            fun() ->
                    EmptyMap = [],
                    {ok, Bin2} = msgpack:pack(EmptyMap),
                    ?assertEqual({ok, EmptyMap}, msgpack:unpack(Bin2))
            end}
    ].

incomplete_test()->
    ?assertEqual({error, {incomplete, <<>>}}, msgpack:unpack(<<>>)).


benchmark_test()->
    Data = [test_data() || _ <- lists:seq(0, 10000)],
    {ok, S} = ?debugTime("  serialize", pack(Data)),
    {ok, Data} = ?debugTime("deserialize", unpack(S)),
    ?debugFmt("for ~p KB test data.", [byte_size(S) div 1024]),
    ok.

error_test_()->
    [
        {"",
            ?_assertEqual({error, {badarg, atom}},
                          msgpack:pack(atom))},
        {"",
            fun() ->
                    Term = {"hoge", "hage", atom},
                    ?assertEqual({error, {badarg, Term}},
                                 msgpack:pack(Term))
            end}
    ].

long_binary_test()->
    {ok, A} = msgpack:pack(1),
    {ok, B} = msgpack:pack(10),
    {ok, C} = msgpack:pack(100),
    ?assertEqual({ok, [1,10,100]},
                 unpack(<<A/binary, B/binary, C/binary>>)),
    ok.

-endif.

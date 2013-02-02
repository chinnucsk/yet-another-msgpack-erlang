-module(msgpack_tests).

-import(msgpack, [pack/1, unpack/1,
                  stream_pack/1, stream_unpack/1]).

-include_lib("eunit/include/eunit.hrl").

msgpack_props_test_() ->
    {timeout,10000, ?_assertEqual([], proper:module(msgpack_props))}.

unpack_test_() ->
    [
        {"not binary",
            ?_assertError({badarg, []}, unpack([]))},

        {"incomplete: null binary",
            ?_assertError(incomplete, unpack(<<>>))},

        {"incomplete: unknown binary",
            ?_assertError(incomplete, unpack(<<16#DA>>))}
    ].

array_test_()->
    [
        {"length 16",
            fun() ->
                    List = lists:seq(0, 16),
                    Binary = pack(List),
                    ?assertEqual(List, unpack(Binary))
            end},
        {"length 32",
            fun() ->
                    List = lists:seq(0, 16#010000),
                    Binary = pack(List),
                    ?assertEqual(List, unpack(Binary))
            end},
        {"empty",
            fun() ->
                    EmptyList = [],
                    Binary = pack(EmptyList),
                    ?assertEqual(EmptyList, unpack(Binary))
            end}
    ].


map_test_()->
    [
        {"length 16",
            fun() ->
                    Map = {[ {X, X * 2} || X <- lists:seq(0, 16) ]},
                    Binary = pack(Map),
                    ?assertEqual(Map, unpack(Binary))
            end},
        {"length 32",
            fun() ->
                    Map = {[ {X, X * 2} || X <- lists:seq(0, 16#010000) ]},
                    Binary = pack(Map),
                    ?assertEqual(Map, unpack(Binary))
            end},
        {"empty",
            fun() ->
                    EmptyMap = {[]},
                    Binary = pack(EmptyMap),
                    ?assertEqual(EmptyMap, unpack(Binary))
            end}
    ].

int_test_() ->
    [
        {"",
            fun() ->
                    Term = -2147483649,
                    Binary = pack(Term),
                    ?assertEqual(Term, unpack(Binary))
            end}
    ].

error_test_()->
    [
        {"badarg atom",
            ?_assertError({badarg, atom},
                          pack(atom))},
        {"badarg tuple",
            fun() ->
                    Term = {"hoge", "hage", atom},
                    ?assertError({badarg, Term},
                                 pack(Term))
            end}
    ].

binary_test_() ->
    [
        {"0 byte",
            fun() ->
                    Binary = pack(<<>>),
                    ?assertEqual(<<>>, unpack(Binary))
            end}
    ].

long_binary_test_()->
    [
        {"long binary",
            fun() ->
                    A = pack(1),
                    B = pack(10),
                    C = pack(100),
                    ?assertEqual([1,10,100],
                                 unpack(list_to_binary([A, B, C])))
            end}
    ].

-module(msgpack_tests).

-include_lib("eunit/include/eunit.hrl").

msgpack_props_test_() ->
    {timeout,10000, ?_assertEqual([], proper:module(msgpack_props))}.


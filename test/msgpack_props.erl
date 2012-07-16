-module(msgpack_props).

-include_lib("proper/include/proper.hrl").

-import(msgpack_proper, [choose_type/0]).

prop_type() ->
    numtests(300,
        ?FORALL(Term, choose_type(),
                begin
                    {ok, Binary} = msgpack:pack(Term),
                    {ok, Term1} = msgpack:unpack(Binary),
                    Term =:= Term1
                end)).


-module(msgpack_props).

-include_lib("proper/include/proper.hrl").

-import(msgpack_proper, [positive_fixnum/0, negative_fixnum/0,
                         int8/0, int16/0, int32/0, int64/0,
                         uint8/0, uint16/0, uint32/0, uint64/0,
                         nil/0,
                         raw16/0, raw32/0]).

choose_type() ->
    oneof([positive_fixnum(), negative_fixnum(),
           int8(), int16(), int32(), int64(),
           uint8(), uint16(), uint32(), uint64(),
           nil(), boolean(),
           raw16(), raw32()]).

prop_type() ->
    numtests(300,
        ?FORALL(Term, choose_type(),
                begin
                    {ok, Binary} = msgpack:pack(Term),
                    {ok, Term1} = msgpack:unpack(Binary),
                    Term =:= Term1
                end)).


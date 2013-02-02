#########################################
Yet another MessagePack Erlang implements
#########################################

.. image:: https://secure.travis-ci.org/voluntas/yet-another-msgpack-erlang.png?branch=develop

:Original: https://github.com/msgpack/msgpack-erlang

How to try
==========

Easy try.

::

    $ git clone git://github.com/voluntas/yet-another-msgpack-erlang.git msgpack
    $ cd msgpack
    $ make
    $ erl -pa ebin
    Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false] [dtrace]

    Eshell V5.9.1  (abort with ^G)
    1> Binary = msgpack:pack([1,2,3]).
    <<147,1,2,3>>
    2> [1,2,3] = msgpack:unpack(Binary).
    [1,2,3]


How to use
==========

Use rebar and deps config.

rebar.config::

    {deps, [
        {msgpack, ".*",
         {git, "git://github.com/voluntas/yet-another-msgpack-erlang.git", {branch, "develop"}}}
      ]
    }.
    

Incompatibilities with Original
===============================

- Remove unpack_all/1
- Use Exception
- Return binary() or msgpack_term()
- Many test
- Add QC test (ProPer)
- Use a tail-recursive

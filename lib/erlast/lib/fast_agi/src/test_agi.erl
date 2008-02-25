%% @hidden
-module(test_agi).
-export([test/2]).

-define(DO(F),io:format("~s => ~p~n",[??F,F])).

test(Req,C) ->
%%    fast_agi:trace(off),
%%    io:format("test ~p~n",[Req]),
%%    ?DO(fast_agi:say_number(C,234,"3#")),
%%    ?DO(fast_agi:channel_status(C)),
%%    ?DO(fast_agi:stream_file(C,"demo-thanks")),
%%    ?DO(fast_agi:stream_file(C,"demo-thanks","1234")),
%%    ?DO(fast_agi:get_data(C,"demo-thanks",3000,3)),
    fast_agi:trace(on),
    ?DO(fast_agi:get_option(C,"demo-thanks","1234")),
%%     ?DO(fast_agi:get_option(C,"demo-thanks","1234",10)),
%%     ?DO(fast_agi:set_variable(C,"my_var","hej")),
%%     ?DO(fast_agi:get_full_variable(C,"my_var")),
%%     ?DO(fast_agi:say_number(C,456)),
%%     ?DO(fast_agi:say_number(C,456,"8")),
%%     ?DO(fast_agi:wait_for_digit1(C)),
%%     ?DO(fast_agi:wait_for_digit(C,5000)),
%%     fast_agi:trace(off),
    ?DO(fast_agi:hangup(C)).

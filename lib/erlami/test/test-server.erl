#!/usr/bin/env escript
%%
%% sample use of the erlami_server
%%

main(_) ->
	code:add_patha("../ebin"),

	erlami_server:start_link({local, ami}),
	erlami_server:connect(ami, drv, 'localhost', 5038),
	X = erlami_server:send_pdu(ami, drv, [{"Action", "Login"}, {"Username", "foo"}, {"Secret", "bar"}]),
	io:format("got ~p~n", [X]),
	loop().

loop() ->
	receive
		X -> io:format("received ~p~n", [X]), loop()
	end.
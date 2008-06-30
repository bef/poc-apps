#!/usr/bin/env escript
%%
%% sample use of the driver - erlami_drv
%%

main(_) ->
	code:add_patha("../ebin"),

	erlami_drv:start_link({local, drv}),
	erlami_drv:connect(drv, 'localhost', 5038),
	erlami_drv:send(drv, [{"Action", "Login"}, {"Username", "foo"}, {"Secret", "bar"}]),
	loop().

loop() ->
	receive
		X -> io:format("received ~p~n", [X]), loop()
	end.
#!/usr/bin/env escript
%%
%% redirect every IAX2/* call after 3 seconds
%%

main(_) ->
	code:add_patha("../ebin"),

	erlami:start_connect('localhost', 5038),
	X = erlami:action({'Login', "foo", "bar"}),
	io:format("got ~p~n", [X]),
	
	loop().

loop() ->
	receive
		{erlami, event, "Newchannel", Msg} ->
			[Channel] = erlami:parse(["Channel"], Msg),
			case Channel of
				"IAX2/"++_ ->
					spawn(fun() ->
						error_logger:info_msg("wait for it...~n", []),
						timer:sleep(3000),
						error_logger:info_msg("redirect: ~p~n", [
							erlami:action({'Redirect', Channel, "7399", "poc-bef", "1"})
						])
						end);
				_ -> ignore
			end,
			loop();
		{erlami, event, Name, _Event} ->
			io:format("event - ~p~n", [Name]),
			loop();
		X ->
			io:format("received ~p~n", [X]),
			loop()
	end.
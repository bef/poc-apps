#!/usr/bin/env escript
%%
%% sample use of the high-level client interface
%%

main(_) ->
	code:add_patha("../ebin"),

	erlami:start_connect('localhost', 5038),
	X = erlami:action({'Login', "foo", "bar"}),
	io:format("got ~p~n", [X]),
	
	io:format("ping: ~p~n", [erlami:action(erlami:pdu({'Ping'}))]),
	%io:format("status: ~p~n", [erlami:action({'Status'})]),
	%io:format("command: ~p~n", [erlami:action({'Command', "conference kick room_lobby 0"})]),
	
	loop().

loop() ->
	receive
		{erlami, event, "PeerStatus", Msg} ->
			ParsedMsg = erlami:parse(["Event", "Privilege", "Peer", "PeerStatus", "Cause"], Msg),
			io:format("event: ~p~n", [ParsedMsg]),
			loop();
		{erlami, event, "Registry", Msg} ->
			ParsedMsg = erlami:parse(["Event", "Privilege", "ChannelDriver", "Domain", "Status"], Msg),
			io:format("event: ~p~n", [ParsedMsg]),
			loop();
		
		X ->
			io:format("received ~p~n", [X]),
			loop()
	end.
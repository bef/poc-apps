#!/usr/bin/env escript
%%
%% simple tcp connect
%%

main(_) ->
	code:add_patha("../ebin"),
	
	case gen_tcp:connect('localhost', 5038, [list, {packet, line}], 5000) of
		{ok, Socket} ->
			loop(Socket);
		{error, Reason} ->
			error_logger:error_msg("connection error: ~p~n", [Reason])
	end.

loop(Socket) ->
	receive
		X ->
			io:format("line: ~p~n", [X]),
			loop(Socket)
	end.

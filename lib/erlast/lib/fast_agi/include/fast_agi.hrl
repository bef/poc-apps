-record(connection,{sock,
		    port,
		    peer_addr,
		    peer_port
		   }).

-define(DBG(O,X),case get(agi_trace) of 
		     true ->
			 io:format("*DBG fast_agi* ~p ~s ~s ~n",[self(),O,X]),
			 X;
		     _ ->
			 X
		 end). 

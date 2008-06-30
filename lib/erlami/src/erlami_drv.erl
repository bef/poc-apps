-module(erlami_drv).
-behaviour(gen_server).
-export([start_link/1,
	%% gen_server
	init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
	
	%% interface
	connect/3, send/2
]).

-record(state, {socket, host, port, acc = [], client}).

%% @doc start_link {@module} named {local, Name}
%% @spec start_link(Name::atom()) -> term()
start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [self()], []).

%% @doc gen_server callback
%% @spec init([Client::pid()]) -> term()
init([Client]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("~p starting: PID ~p~n", [?MODULE, self()]),

	{ok, #state{client = Client}}.


%% interface

%% @doc connect to host/port and assign a driver name
%% @spec connect(Drv::atom(), Host::string, Port::string()) -> ok
connect(Drv, Host, Port) ->
	gen_server:call(Drv, {connect, {Host, Port}}).

%% @doc send PDU cast-style
%% @spec send(Drv::atom(), Pdu::tuplelist()) -> nothing
send(Drv, Pdu) ->
	gen_server:cast(Drv, {send, Pdu}).


%% handlers

handle_call({connect, {Host, Port}}, _From, S) ->
	{ok, Sock} = do_connect(Host, Port),
	{reply, ok, S#state{socket = Sock, host = Host, port = Port}};

handle_call(Request, From, S) ->
	error_logger:warning_msg("~p received unknown request from ~p: ~p~n", [?MODULE, From, Request]),
	{noreply, S}.

handle_cast({send, Pdu}, S) ->
	do_send(S#state.socket, Pdu),
	{noreply, S};

handle_cast(Msg, S) ->
	error_logger:warning_msg("~p received unknown cast request: ~p~n", [?MODULE, Msg]),
	{noreply, S}.

handle_info({tcp, _, "Asterisk Call Manager/"++_}, S) ->
	%% ok: connected to asterisk
	{noreply, S};

handle_info({tcp, _, "Response: "++_ = Data}, S) ->
	{noreply, S#state{acc = [Data]}};

handle_info({tcp, _, "Event: "++_ = Data}, S) ->
	{noreply, S#state{acc = [Data]}};

handle_info({tcp, _, "\r\n"}, S) ->
	Input = lists:reverse(S#state.acc),
	Pdu = lists:map(fun([$\n, $\r | RLine]) ->
		Line = lists:reverse(RLine),
		SepPos = string:str(Line, ": "),
		%io:format("line: ~p, pos: ~p~n", [Line, SepPos]),
		case SepPos of
			0 -> {"unknown", Line};
			_ -> {string:substr(Line, 1, SepPos-1), string:substr(Line, SepPos+2)}
		end;
		(RLine) -> {"grrr", lists:reverse(RLine)}
	end, [lists:reverse(L) || L <- Input]),

	%io:format("message: ~p~n", [Pdu]),
	send_msg(Pdu, S#state.client),
	{noreply, S#state{acc = []}};

handle_info({tcp, _, Msg}, S) ->
	{noreply, S#state{acc = [Msg|S#state.acc]}};

handle_info({tcp_closed, _}, S) ->
	send_msg(closed, S#state.client),
	{noreply, S#state{socket = undefined, acc = []}};

handle_info({tcp_error, Socket, Reason}, S) ->
	error_logger:warning_msg("~p got tcp error ~p on socket ~p~n", [?MODULE, Reason, Socket]),
	send_msg(reconnect, S#state.client),
	
	%% auto-reconnect
	{ok, Sock} = do_connect(S#state.host, S#state.port),
	send_msg(reconnected, S#state.client),
	
	{noreply, S#state{socket = Sock, acc = []}};

handle_info(Info, S) ->
	error_logger:warning_msg("~p received unknown info request: ~p~n", [?MODULE, Info]),
	{noreply, S}.

terminate(Reason, _S) ->
	error_logger:info_msg("~p stopping: ~p~n", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.


%% internal functions

do_connect(Host, Port) ->
	case gen_tcp:connect(Host, Port, [list, {packet, line}], 5000) of
		{ok, Socket} ->
			{ok, Socket};
		{error, Reason} ->
			error_logger:error_msg("connection error: ~p~n", [Reason]),
			
			%% wait a bit and try again
			timer:sleep(2000),
			do_connect(Host, Port)
	end.
	
do_send(Sock, Pdu) -> %% pdu = protocol description unit / tuplelist
	Epdu = lists:flatten([K ++ ": " ++ V ++  "\r\n" || {K, V} <- Pdu] ++ "\r\n"),
	%io:format("sending: ~p~n", [Epdu]),
	gen_tcp:send(Sock, Epdu).

send_msg(Message, Client) ->
	Client ! {erlami_drv, self(), Message}.

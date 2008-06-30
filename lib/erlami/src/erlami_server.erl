-module(erlami_server).
-behaviour(gen_server).
-export([start_link/1,
	%% gen_server
	init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
	
	%% interface
	connect/4, send_pdu/3, async_send_pdu/3,
	
	do_send_sync/3, do_send_sync/4
]).

-record(state, {queue=[], aid=0, client}).


start_link(Name) ->
	gen_server:start_link(Name, ?MODULE, [self()], []).
	
init([Client]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("~p starting: PID ~p~n", [?MODULE, self()]),
	
	{ok, #state{client=Client}}.

%% interface

%% @doc connect server to driver
%% @spec connect(Me, Drv::atom(), Host::string(), Port::integer()) -> ok
%% where Me = pid() | atom()
connect(Me, Drv, Host, Port) ->
	gen_server:call(Me, {connect, {Drv, Host, Port}}).

%% @doc send PDU synchronously (wait)
send_pdu(Me, Drv, Pdu) ->
	gen_server:call(Me, {send_pdu, {Drv, Pdu}}, infinity).

%% @doc send PDU asynchronously (no wait)
async_send_pdu(Me, Drv, Pdu) ->
	gen_server:call(Me, {async_send_pdu, {Drv, Pdu}}).


%% handlers

handle_call({connect, {DrvName, Host, Port}}, _From, S) ->
	erlami_drv:start_link(DrvName),
	erlami_drv:connect(DrvName, Host, Port),
	{reply, ok, S};

handle_call({send_pdu, {Drv, Pdu}}, From, S) ->
	AID = integer_to_list(S#state.aid),
	Pdu2 = Pdu ++ [{"ActionID", AID}],
	PID = spawn(?MODULE, do_send_sync, [From, Drv, Pdu2]),
	{noreply, S#state{queue = [{AID, PID} | S#state.queue], aid = S#state.aid + 1}};

handle_call({async_send_pdu, {Drv, Pdu}}, _From, S) ->
	AID = integer_to_list(S#state.aid),
	Pdu2 = Pdu ++ [{"ActionID", AID}],
	erlami_drv:send(Drv, Pdu2),
	{reply, AID, S};

handle_call(Request, From, S) ->
	error_logger:warning_msg("~p received unknown request from ~p: ~p~n", [?MODULE, From, Request]),
	{noreply, S}.

handle_cast(Msg, S) ->
	error_logger:warning_msg("~p received unknown cast request: ~p~n", [?MODULE, Msg]),
	{noreply, S}.

handle_info({erlami_drv, _, [{"Response", _}|_]=Msg}, S) ->
	case lists:keysearch("ActionID", 1, Msg) of
		{value, {_, AID}} ->
			handle_response(AID, Msg, S);
		_ -> {noreply, S}
	end;

handle_info({erlami_drv, _, [{"Event", EventType}|_]=Msg}, S) ->
	do_notify(S#state.client, {erlami, event, EventType, Msg}),
	{noreply, S};

handle_info({erlami_drv, Drv, closed}, S) ->
	error_logger:info_msg("connection to driver ~p closed~n", [Drv]),
	do_notify(S#state.client, {erlami, event, closed}),
	{noreply, S};

handle_info(Info, S) ->
	error_logger:warning_msg("~p received unknown info request: ~p~n", [?MODULE, Info]),
	{noreply, S}.

handle_response(AID, [{"Response", Resp}|_]=Msg, S) ->
	S2 = case lists:keysearch(AID, 1, S#state.queue) of
		{value, {_, PID}} ->
			%% this response is a sync msg => release the waiting process
			try do_notify(PID, {Resp, Msg}) of
				_ -> ok
			catch
				_ -> ignore_errors
			end,
			Q = lists:keydelete(AID, 1, S#state.queue),
			S#state{queue = Q};
		_ ->
			%% this response is an async message => notify our client
			do_notify(S#state.client, {erlami, response, Resp, Msg}),
			S
	end,
	{noreply, S2}.

terminate(Reason, _S) ->
	error_logger:info_msg("~p stopping: ~p~n", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.


%% internal

do_send_sync(From, Drv, Pdu) ->
	do_send_sync(From, Drv, Pdu, infinity).

do_send_sync(From, Drv, Pdu, Timeout) ->
	erlami_drv:send(Drv, Pdu),
	Response = receive
		X -> X
	after Timeout ->
		{error, timeout}
	end,
	gen_server:reply(From, Response).

do_notify(PID, Msg) ->
	PID ! Msg.

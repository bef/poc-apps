%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Generic socket server behaviour.
%%% Provides a socket server behaviour with state management according to
%%% ITU-T X.731.
%%%
%%% The callback module must implement the following functions.
%%% 
%%% init/2,
%%% 
%%% handle_data/2,
%%% 
%%% handle_closed/1,
%%% 
%%% handle_info/2,
%%% 
%%% handle_error/2,
%%% 
%%% terminate/2
%%% @end 
%%% Created :  2 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_socket_server).

-behaviour(gen_server).

%% API
-export([
	 start_link/3,
	 start_link/4,
	 get_adm_state/1,set_adm_state/2,
	 get_capacity/1,set_capacity/2,
	 get_op_state/1,
	 get_stats/1,
	 get_usage_state/1,
	 send/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% gen_socket_server internal callbacks
-export([listener_init/1]).

-export([behavior_info/1]).

-include("gen_socket_server.hrl").

-record(listener_state,{cbmod,cbstate,conn}).
-record(state, {cbmod,
		cbpars,
		adm_state,
		op_state,
		op_state_reason,
		port,
		opts,
		sock,
		listener,
		connections=[],
		max_conns,
		cnt_total=0,
		cnt_normal=0,
		cnt_failed=0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(CBMod::atom(),CBPars::atom(),Port::integer()) -> {ok,Pid} | ignore | {error,Error}
%% @doc  Starts the server. Listening on port Port.
%% @end
%%--------------------------------------------------------------------
start_link(CBMod,CBPars,Port) ->
    start_link({local,?MODULE}, CBMod, CBPars, Port).

%%--------------------------------------------------------------------
%% @spec start_link(RName::Name,CBMod::atom(),CBPars::atom(),Port::integer()) -> 
%%              {ok,Pid} | ignore | {error,Error}
%% Name = {Type,atom()}
%% Type = local|global
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port.
%% @end
%%--------------------------------------------------------------------
start_link({Type,Name},CBMod,CBPars,Port) when Type==local;Type==global->
    start_link(Name,CBMod,CBPars,Port,[]).

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),CBMod::atom(),CBPars::atom(),Port::integer(),Opts::list()) -> 
%% {ok,Pid} | ignore | {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port. Options Opts.
%% @end
%%--------------------------------------------------------------------
start_link(Name,CBMod,CBPars,Port,Opts) when is_list(Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, 
			  [CBMod,CBPars,Port,Opts], []).

%%--------------------------------------------------------------------
%% @spec get_adm_state(Server) -> locked|unlocked|shutting_down
%% @doc Get the administrative state of the server.
%% <dl>
%% <dt>locked</dt><dd>The server is administratively prohibited from use.</dd>
%% <dt>unlocked</dt><dd>The server is not administratively prohibited from use</dd>
%% <dt>shutting_down</dt><dd>The server is shutting down.</dd>
%% </dl>
%% @end
%%--------------------------------------------------------------------
get_adm_state(Server) ->
    gen_server:call(Server,get_adm_state).

%%--------------------------------------------------------------------
%% @spec set_adm_state(Server::atom(),State) -> Result 
%% State =lock|unlock
%% @doc Set the administrative state of the server.
%% @end
%%--------------------------------------------------------------------
set_adm_state(Server,State) when State==lock;State==unlock ->
    gen_server:call(Server,{set_adm_state,State}).

%%--------------------------------------------------------------------
%% @spec get_capacity(Server) -> integer()
%% @doc Get the number of allowed connections
%% @end
%%--------------------------------------------------------------------
get_capacity(Server) ->
    gen_server:call(Server,get_capacity).

%%--------------------------------------------------------------------
%% @spec set_capacity(Server,Capacity::integer()) -> ok
%% @doc Set the number of allowed connections
%% @end
%%--------------------------------------------------------------------
set_capacity(Server,Capacity) when is_integer(Capacity), Capacity > 0 ->
    gen_server:call(Server,{set_capacity,Capacity}).

%--------------------------------------------------------------------
%% @spec get_op_state(Server) -> enabled|{disabled,Reason}
%% @doc Get the operational state of the server.
%% @end
%%--------------------------------------------------------------------
get_op_state(Server) ->
    gen_server:call(Server,get_op_state).

%%--------------------------------------------------------------------
%% @spec get_stats(Server) -> Statistics
%% @doc Get the statistics of the server
%% @end
%%--------------------------------------------------------------------
get_stats(Server) ->
    gen_server:call(Server,get_stats).

%%--------------------------------------------------------------------
%% @spec get_usage_state(Server) -> active|busy|idle
%% @doc Get the usage state of the server.
%% <dl>
%% <dt>active</dt><dd>There are some active sessions</dd>
%% <dt>busy</dt><dd>All sessions are active</dd>
%% <dt>idle</dt><dd>There are no active sessions</dd>
%% </dl>
%% @end
%%--------------------------------------------------------------------
get_usage_state(Server) ->
    gen_server:call(Server,get_usage_state).

%%--------------------------------------------------------------------
%% @spec send(Server,Data) -> ok | {error,Reason}
%% @doc Send message to connection client. This function should only be used by 
%% a callback module to send a message to its peer.
%% @end
%%--------------------------------------------------------------------
send(Conn,Data) ->
    gen_tcp:send(Conn#connection.sock,Data).

%%====================================================================
%% behaviour callbacks
%%====================================================================
%% @private
behavior_info(callbacks) ->
    [{init,2},{handle_data,2},{handle_closed,1},
     {handle_info,2},{handle_error,2},{terminate,2}];
behavior_info(_Other) ->
    undefined.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc  Initiates the server.
%% @end
%% @private
%%--------------------------------------------------------------------
init([CBMod,CBPars,Port,Opts]) ->
    process_flag(trap_exit, true),
    S1=#state{adm_state=enabled,
	      cbmod=CBMod,
	      cbpars=CBPars,
	      port = Port,
	      opts=Opts},
    S2=case open_listen_socket(S1) of
	   {success,State} ->
	       start_link_listener(State);
	   {failure,State} ->
	       State
       end,
    {ok,S2}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc  Handling call messages
%% @end
%% @private
%%--------------------------------------------------------------------
handle_call(get_adm_state, _From, State) ->
    Reply = State#state.adm_state,
    {reply, Reply, State};

handle_call({set_adm_state,NewState}, _From, State) 
  when State#state.adm_state==NewState ->
    {reply, ok, State};

handle_call(get_capacity, _From, State) ->
    Reply = State#state.max_conns,
    {reply, Reply, State};

handle_call({set_capacity,Capacity}, _From, State) ->
    State1=State#state.max_conns,
    {reply, ok, State1};

handle_call({set_adm_state,unlock}, _From, State) ->
    case open_listen_socket(State) of
	{success,S1} ->
	    S2=start_link_listener(S1),
	    S3=S2#state{adm_state=unlocked},
	    {reply, ok, S3};
	{failure,S1} ->
	    {reply, ok, S1}
    end;

handle_call({set_adm_state,lock}, _From, State) ->
    S1=close_listen_socket(State),
    S2=case S1#state.connections of
	   [] ->
	       S1#state{adm_state=locked};
	   _L ->
	       S1#state{adm_state=shutting_down}
       end,
    {reply, ok, S2};

handle_call(get_op_state, _From, State) ->
    case State#state.op_state of
	enabled ->
	    {reply, enabled, State};
	disabled ->
	    {reply, {disabled,State#state.op_state_reason}, State}
    end;

handle_call(get_stats, _From, State) ->
    {reply,{State#state.cnt_total,
	    State#state.cnt_normal,
	    State#state.cnt_failed},
     State};

handle_call(get_usage_state, _From, State) ->
    case length(State#state.connections) of
	0 ->
	    {reply, idle, State};
	N ->
	    case State#state.max_conns of
		undefined ->
		    {reply, active, State};
		M when M < N->
		    {reply, active, State};
		_M ->
		    {reply, busy, State}
	    end
    end;

handle_call(_Req, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc  Handling cast messages
%% @end
%% @private
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc  Handling all non call/cast messages
%% @end
%% @private
%%--------------------------------------------------------------------
handle_info({connected,Pid}, State) ->
    S1=start_link_listener(State),
    Cons=S1#state.connections,
    Cnt=S1#state.cnt_total,
    {noreply, S1#state{connections=[Pid|Cons],cnt_total=Cnt+1}};

handle_info({'EXIT', Pid, Reason}, #state{listener=Pid} = State) ->
    case State#state.adm_state of
	enabled ->
	    timer:send_after(2000,start_listener),
	    {noreply,State#state{op_state=disabled,
				 op_state_reason=Reason,
				 sock=undefined,
				 listener=undefined}};
	disabled ->
	    {noreply,State#state{listener=undefined,sock=undefined}}
    end;

handle_info({'EXIT', Pid, normal}, State) ->
    Conns = State#state.connections,
    case lists:member(Pid,Conns) of
	true ->
	    Cnt=State#state.cnt_normal,
	    S1=State#state{connections=lists:delete(Pid,Conns),cnt_normal=Cnt+1},
	    case {S1#state.adm_state,S1#state.connections} of
		{shutting_down,[]} ->
		    {noreply,S1#state{adm_state=locked}};
		_Else ->
		    {noreply,S1}
	    end;
	false ->
	    {noreply,State}
    end;

handle_info({'EXIT', Pid, Reason}, State) ->
    Conns = State#state.connections,
    case lists:member(Pid,Conns) of
	true ->
	    Cnt=State#state.cnt_failed,
	    S1=State#state{connections=lists:delete(Pid,Conns),cnt_failed=Cnt+1},
	    {noreply,S1};
	false ->
	    {stop,Reason,State}
    end;

handle_info(start_listener, State) ->
    case open_listen_socket(State) of
	{success,S1} ->
	    S2=start_link_listener(S1),
	    {noreply, S2};
	{failure,State} ->
	    timer:send_after(2000,start_listener),
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc  This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed
%% @end
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
open_listen_socket(State) ->
    case gen_tcp:listen(State#state.port,State#state.opts) of
	{ok,Socket} ->
	    {success,State#state{sock=Socket,op_state=enabled}};
	{error,Reason} ->
	    {failure,State#state{op_state=disabled,op_state_reason=Reason}}
    end.

close_listen_socket(State) ->
    Listener=State#state.listener,
    exit(Listener,stoppped),
    gen_tcp:close(State#state.sock),
    State#state{op_state=disabled,op_state_reason=adm_state}.

start_link_listener(State) ->
    Pid=proc_lib:spawn_link(?MODULE, listener_init, 
			    [{State#state.cbmod, State#state.cbpars, 
			      self(), State#state.sock}]),
    State#state{listener=Pid,op_state=enabled}.

%% @private
listener_init({CBMod,CBPars, Pid, LSocket}) ->
    case catch gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    %% Send the cast message to the listener process to 
	    %% create a new acceptor
	    Pid!{connected,self()},
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #connection{sock = Socket,
			    peer_addr = Addr,
			    peer_port = Port},
	    connected(access_control(CBMod,CBPars,C));
	{error,Reason} ->
	    error_logger:error_report([{application, gen_socket_server},
				       "Accept failed error",
				       io_lib:format("~p",[Reason])]),
	    exit({error, accept_failed, Reason})
    end.

%% @todo Do something intelligent here.
access_control(CBMod,CBPars,Conn) ->
    {allow,CBMod,CBPars,Conn}.

connected({allow,CBMod,CBPars,Conn}) ->
    {ok,S}=do_callback(CBMod,init,[CBPars,Conn]),
    listen_loop(CBMod,Conn,S,[]);

connected({deny,Conn}) ->
    gen_tcp:close(Conn#connection.sock).

listen_loop(CBMod,Conn,S,Cont) ->
    receive
	{tcp,_Socket,Data} ->
	    case do_callback(CBMod,handle_data,[Data++Cont],S) of
		{ok,Cont1,S1} ->
		    listen_loop(CBMod,Conn,S1,Cont1);
		{stop,Reason} ->
		    exit(Reason)
	    end;
	{tcp_closed,_Socket} ->
	    case do_callback(CBMod,handle_closed,[],S) of
		{stop,Reason} ->
		    exit(Reason);
		{ok,S1} ->
		    listen_loop(CBMod,Conn,S1,Cont)
	    end;
	{tcp_error,_Socket,Reason} ->
	    case do_callback(CBMod,handle_error,[Reason],S) of
		{ok,S1} ->
		    listen_loop(CBMod,Conn,S1,Cont);
		{stop,Reason} ->
		    exit(Reason)
	    end;
	{gss,terminate,Reason} ->
	    do_callback(CBMod,terminate,[Reason],S),
	    exit(Reason);
	Info ->
	    case do_callback(CBMod,handle_info,[Info],S) of
		{ok,S1} ->
		    listen_loop(CBMod,Conn,S1,Cont);
		{stop,Reason} ->
		    exit(Reason)
	    end
    end.

do_callback(Mod,Fun,Pars) ->
    do_callback1(Mod,Fun,Pars).

do_callback(Mod,Fun,Pars,State) ->
    do_callback1(Mod,Fun,Pars++[State]).

do_callback1(Mod,Fun,Pars) ->
    apply(Mod,Fun,Pars).

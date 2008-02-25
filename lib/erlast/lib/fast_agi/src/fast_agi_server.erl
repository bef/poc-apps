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
%%% File    : fast_agi.erl
%%% Created : 22 Feb 2006 by Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2006 Anders Nygren
%%% @version {@vsn}
%%% @doc Main process for the fast_agi server. 
%%% Spawns a listener process that listens on the assigned port.
%%% When a connection is made the listener calls fast_agi_server:create/2
%%% to create a new listener process and continues handling the request.
%%% @end
%%%-------------------------------------------------------------------
-module(fast_agi_server).

-behaviour(gen_server).

%% API
-export([start_link/0,start_link/1,start_link/2,
	 create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER,?MODULE).
-define(PORT,4573).

-record(state, {listen_socket,
                port,
                acceptor}).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Start the server.
%% @equiv start_link(4573,[])
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(?PORT).

%%--------------------------------------------------------------------
%% @spec start_link(Port::integer()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Start the server.
%% @equiv start_link(Port,[])
%% @end
%%--------------------------------------------------------------------
start_link(Port) when is_integer(Port)->
    start_link(Port,[]).

%%--------------------------------------------------------------------
%% @spec start_link(Port::integer(),Opts) -> {ok,Pid} | 
%%                                           ignore | 
%%                                           {error,Error}
%% Opts = [Opt]
%% @doc Start the server. See inet:setopts/2 for values of Opt.
%% @end
%%--------------------------------------------------------------------
start_link(Port,Opts) when is_integer(Port), is_list(Opts) ->
    gen_server:start_link({local,?SERVER},?MODULE,[Port,Opts],[]).

%%--------------------------------------------------------------------
%% spec create(ServerPid, Pid) -> ok
%% @doc Create a new listener process.
%% @end
%% @private
%%--------------------------------------------------------------------
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
%% @private
init([Port,Opts]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, Opts++[list, {packet, line},
				     {reuseaddr,true},
				     {active, false}]) of
	{ok, Listen_socket} ->
	    %%Create first accepting process
	    Pid = fast_agi_socket:start_link(self(), Listen_socket, ?PORT),
	    {ok, #state{listen_socket = Listen_socket,
                        port = ?PORT,
			acceptor = Pid}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({create,_Pid},#state{listen_socket = Listen_socket} = State) ->
    New_pid = fast_agi_socket:start_link(self(), 
					 Listen_socket, 
					 State#state.port),
    {noreply, State#state{acceptor=New_pid}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% The current acceptor has died, wait a little and try again
%% @private
handle_info({'EXIT', Pid, _Abnormal}, #state{acceptor=Pid} = State) ->
    timer:sleep(2000),
    fast_agi_socket:start_link(self(), 
			       State#state.listen_socket, 
			       State#state.port),
    {noreply,State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

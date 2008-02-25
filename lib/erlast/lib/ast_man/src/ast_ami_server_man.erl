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
%%% @doc Asterisk Manager Interface server manager. Manages all 
%%% instances of ami servers.
%%% @end 
%%% Created : 10 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_ami_server_man).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 del_server/1,
	 new_server/1,
	 start_server/1,
	 stop_server/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER,?MODULE).

-record(server_def, {name,cbpars,port,opts}).
-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc  Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

del_server(Server) ->
    gen_server:call(?SERVER,{del_server,Server}).

new_server(Pars) ->
    gen_server:call(?SERVER,{new_server,Pars}).

start_server(Server) ->
    gen_server:call(?SERVER,{start_server,Server}).

stop_server(Server) ->
    gen_server:call(?SERVER,{stop_server,Server}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc  Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
%% Start all proxies
    State=ok, %%start_all(),
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc  Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({del_server,Server}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({new_server,Pars}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({start_server,Server}, _From, State) ->
    Def=ok,%%get_def(Server),
    start_server1(Def),
    Reply = ok,
    {reply, Reply, State};
handle_call({stop_server,Server}, _From, State) ->
%%    stop_server1(Server),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc  Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc  Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc  This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_server1(#server_def{name=Name,cbpars=CBPars,port=Port,opts=Opts}) ->
    case gen_socket_server:start_link(Name,CBPars,Port,Opts) of
	{ok,Pid} ->
	    Pid;
	Error ->
	    Error
    end.

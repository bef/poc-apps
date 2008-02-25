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
%%% File    : sup_fast_agi.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2006 Anders Nygren
%%% @version {@vsn}
%%% @doc Supervisor for fast_agi server. 
%%% @private
%%%
%%% Created : 24 Feb 2006 by Anders Nygren
%%%-------------------------------------------------------------------
-module(sup_fast_agi).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Pars=case application:get_env(port) of
	     {ok,Port} ->
		 [Port]++ case application:get_env(opts) of
			      {ok,Opts} -> [Opts];
			      _Nothing -> []
			  end;
	     _Nothing ->
		 []
	 end,
    Server=fast_agi_server,
    S = {Server,{Server,start_link,Pars},
	 permanent,2000,worker,[Server]},
    {ok,{{one_for_all,10,1}, [S]}}.

%%====================================================================
%% Internal functions
%%====================================================================

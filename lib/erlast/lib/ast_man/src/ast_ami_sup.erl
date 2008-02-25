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
%%% File    : ast_man_sup.erl
%%% Author  : Anders Nygren <anders@telteq.com.mx>
%%% Description : 
%%%
%%% Created : 29 Mar 2006 by Anders Nygren <anders@telteq.com.mx>
%%%-------------------------------------------------------------------
%% @private

-module(ast_ami_sup).

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
start_link(_StartArgs) ->
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
	Ip = get_env(ip, {127,0,0,1}),
	Port = get_env(port, 5038),
	User = get_env(user, "admin"),
	Secret = get_env(passwd, "secret"),
	
	%AstEvt = {ast_evt,{ast_man_events,start_link,[]},
	%	permanent,2000,worker,[ast_man_events]},
	AstEvt = {ast_evt,{ast_ami_events,start_link,[]},
		permanent,2000,worker,[ast_ami_events]},
	AstMan = {ast_man,{ast_ami_client,start_link,[User, Secret]},
		permanent,2000,worker,[ast_ami_client]},
	AstDrv = {ast_drv,{ast_ami_drv,start_link,[Ip, Port]},
		permanent,2000,worker,[ast_ami_drv]},
	{ok,{{one_for_all,100,1000}, [AstDrv,AstEvt,AstMan]}}.

%%====================================================================
%% Internal functions
%%====================================================================

get_env(Key, Default) ->
	case application:get_env(Key) of
		{ok, Value} -> Value;
		_Undef -> Default
	end.

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
%%% @doc gen_socket_server callback module for Asterisk Manager
%%% Interface servers.
%%% @end 
%%% Created :  8 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_ami_server).

%% gen_socket_server callbacks
-export([init/2,
	 handle_data/2,
	 handle_closed/1,
	 handle_info/2,
	 handle_error/2,
	 terminate/2
	]).

-record(state,{handler,user,passwd}).

%%-------------------------------------------------------------------
%% @spec init(Conf,Conn) -> {ok,State} | {stop,Reason}
%% @doc 
%% @end
%%-------------------------------------------------------------------
init({Handler,UserId,Passwd}=Conf,Conn) ->
    io:format("~p:init(~p,~p)~n",[?MODULE,Conf,Conn]),
    S1=#state{handler=Handler,user=UserId,passwd=Passwd},
    S2=S1,%%login(S1),
    {ok,S2}.

%%-------------------------------------------------------------------
%% @spec handle_data(Data::list(),State) -> {ok,Cont::list(),State} | {stop,Reason}
%% @doc Received data callback. Called when new data is received from peer. 
%% If not all data is consumed it shall be retured in Cont.
%% @end
%%-------------------------------------------------------------------
handle_data(Data,State) ->
    io:format("~p:handle_data(~p) state=~p~n",[?MODULE,Data,State]),
    {Cont,S1}=case ast_ami_pdu:parse(Data) of
	     {nothing,C} ->
		 {C,State};
	     {Pdus,C} ->
		 {C,handle_pdus(Pdus,State)}
	 end,
    {ok,Cont,S1}.

%%-------------------------------------------------------------------
%% @spec handle_closed(State) -> {ok,State} | {stop,Reason}
%% @doc Socket closed callback.
%% @end
%%-------------------------------------------------------------------
handle_closed(State) ->
    {stop,normal}.

%%-------------------------------------------------------------------
%% @spec handle_info(Info,State) -> {ok,State} | {stop,Reason}
%% @doc Handle any other messages received.
%% @end
%%-------------------------------------------------------------------
handle_info({pdu,Pdu},State) ->
    gen_socket_server:send(Pdu),
    {ok,State};

handle_info(Info,State) ->
    {ok,State}.

%%-------------------------------------------------------------------
%% @spec handle_error(Reason,State) -> {ok,State} | {stop,Reason}
%% @doc Handle errors from socket.
%% @end
%%-------------------------------------------------------------------
handle_error(Reason,State) ->
    {ok,State}.

%%-------------------------------------------------------------------
%% @spec terminate(Reason,State) -> {ok,State} | {stop,Reason}
%% @doc Terminate process.
%% @end
%%-------------------------------------------------------------------
terminate(Reason,State) ->
    {ok,State}.

%%===================================================================
%% Internal functions
%%===================================================================
    
handle_pdus(Pdus,State) ->
    lists:foreach(fun (Pdu) -> handle_pdu(Pdu,State) end, Pdus).

handle_pdu(Pdu,State) ->
    (State#state.handler):request(Pdu).

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

%%  Action           Privilege        Synopsis
%%   ------           ---------        --------
%%   AbsoluteTimeout  call,all         Set Absolute Timeout
%%   AgentCallbackLo  agent,all        Sets an agent as logged in by callback
%%   AgentLogoff      agent,all        Sets an agent as no longer logged in
%%   Agents           agent,all        Lists agents and their status
%%   ChangeMonitor    call,all         Change monitoring filename of a channel
%%   Command          command,all      Execute Asterisk CLI Command
%%   DBGet            system,all       Get DB Entry
%%   DBPut            system,all       Put DB Entry
%%   Events           <none>           Control Event Flow
%%   ExtensionState   call,all         Check Extension Status
%%   Getvar           call,all         Gets a Channel Variable
%%   Hangup           call,all         Hangup Channel
%%   IAXnetstats      <none>           Show IAX Netstats
%%   IAXpeers         <none>           List IAX Peers
%%   ListCommands     <none>           List available manager commands
%%   Logoff           <none>           Logoff Manager
%%   MailboxCount     call,all         Check Mailbox Message Count
%%   MailboxStatus    call,all         Check Mailbox
%%   Monitor          call,all         Monitor a channel
%%   Originate        call,all         Originate Call
%%   ParkedCalls      <none>           List parked calls
%%   Ping             <none>           Keepalive command
%%   QueueAdd         agent,all        Add interface to queue.
%%   QueuePause       agent,all        Makes a queue member temporarily unavailable
%%   QueueRemove      agent,all        Remove interface from queue.
%%   Queues           <none>           Queues
%%   QueueStatus      <none>           Queue Status
%%   Redirect         call,all         Redirect (transfer) a call
%%   SetCDRUserField  call,all         Set the CDR UserField
%%   Setvar           call,all         Set Channel Variable
%%   SIPpeers         system,all       List SIP peers (text format)
%%   SIPshowpeer      system,all       Show SIP peer (text format)
%%   Status           call,all         Lists channel status
%%   StopMonitor      call,all         Stop monitoring a channel
%%   ZapDialOffhook   <none>           Dial over Zap channel while offhook
%%   ZapDNDoff        <none>           Toggle Zap channel Do Not Disturb status OFF
%%   ZapDNDon         <none>           Toggle Zap channel Do Not Disturb status ON
%%   ZapHangup        <none>           Hangup Zap Channel
%%   ZapShowChannels  <none>           Show status zapata channels
%%   ZapTransfer      <none>           Transfer Zap Channel



%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : ast_ami_client.erl
%%% @author anders <anders.nygren@gmail.com>
%%% @doc
%%% @end
%%% Created : 20 Feb 2006 by anders <anders@>
%%%-------------------------------------------------------------------
-module(ast_ami_client).

-behaviour(gen_server).

%% API
-export([start_link/2,
	 %subscribe/3,
 	 absolute_timeout/2,
 	 agent_callback_login/2,agent_callback_login/5,
 	 agent_logoff/1,agent_logoff/2,
 	 agents/0,
 	 change_monitor/2,
 	 command/1,
 	 db_del/2,
 	 db_get/2,
 	 db_put/3,
 	 events/1,
 	 extension_state/2,
 	 get_var/2,
 	 hangup/1,
 	 iax_netstats/0,
 	 iax_peers/0,
 	 list_commands/0,
 	 logoff/0,
	 mailbox_count/1,
 	 mailbox_status/1,
 	 monitor/4,
 	 originate/7,
 	 parked_calls/0,
 	 ping/0,
 	 queue_add/4,
 	 queue_pause/3,
 	 queue_remove/2,
 	 queues/0,
 	 queue_status/0,
 	 redirect/4,redirect/5,
 	 set_cdr_user_field/3,
 	 set_var/3,
	 sip_peers/0,
	 sip_showpeer/1,
	 status/0,status/1,
 	 stop_monitor/1,
 	 zap_dial_offhook/2,
 	 zap_dnd_off/1,
 	 zap_dnd_on/1,
 	 zap_hangup/1,
 	 zap_show_channels/0
	 %% 	 zap_transfer
	]).

-include("manager_api.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% ast_man_drv callbacks
-export([new_pdu/1]).

-define(SERVER,?MODULE).

-record(state, {queue,pending,list_resp=false,resp_acc=[],state}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @doc Start the manager server
start_link(User, Secret) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [User, Secret], []).

%% @spec subscribe(User,Secret,Events) -> ok
%% @doc Subscribe to events.
%subscribe(User,Secret,Events) ->
%    ast_man_events:subscribe(User,Secret,Events).

%% @doc Receive new pdu from asterisk.
%% @private
new_pdu(Msg) ->
    gen_server:cast(?SERVER,{pdu,Msg}).

%% @spec absolute_timeout(Channel::string(),Timeout::integer()) -> Result
%% @doc Set Absolute Timeout. Timeout in seconds.
absolute_timeout(Channel,Timeout) ->
    gen_server:call(?SERVER,{req,#req_absolute_timeout{channel=Channel,
						       timeout=Timeout}}).

%% @spec agent_callback_login(Agent::string(),Exten::integer()) -> Result
%% @doc Sets an agent as logged in by callback.
agent_callback_login(Agent,Exten) ->
    gen_server:call(?SERVER,{req,#req_agent_callback_login{agent=Agent,
							   exten=Exten}}).

%% @spec agent_callback_login(Agent::string(),Exten::string(),Context::string(),AckCall::boolean(),WrapUpTime::integer()) -> Result
%% @doc Sets an agent as logged in by callback.
agent_callback_login(Agent,Exten,Context,AckCall,WrapUpTime) ->
    gen_server:call(?SERVER,{req,#req_agent_callback_login{agent=Agent,
							   exten=Exten,
							   context=Context,
							   ackCall=AckCall,
							   wrapUpTime=WrapUpTime}}).

%% @spec agent_logoff(Agent::string()) -> Result
%% @doc Sets an agent as no longer logged in.
%% @equiv agent_logoff(Agent,true)
agent_logoff(Agent) ->
    agent_logoff(Agent,true).

%% @spec agent_logoff(Agent::string(),Soft::boolean()) -> Result
%% @doc Sets an agent as no longer logged in.
agent_logoff(Agent,Soft) ->
    gen_server:call(?SERVER,{req,#req_agent_logoff{agent=Agent,
						   soft=Soft}}).

%% @spec agents() -> Result
%% @doc Lists agents and their status.
agents() ->
    gen_server:call(?SERVER,{req,#req_agents{}}).

%% @spec change_monitor(Channel::string(),File::string()) -> Result
%% @doc Change monitoring filename of a channel.
change_monitor(Channel,File) ->
    gen_server:call(?SERVER,{req,#req_change_monitor{channel=Channel,
						     file=File}}).

%% @spec command(Command::string()) -> Result
%% @doc Execute Asterisk CLI Command.
command(Command) ->
    gen_server:call(?SERVER,{req,#req_command{command=Command}}).

%% @spec db_del(Family::string(),Key::string()) -> Result
%% @doc Delete DB Entry.
db_del(Family,Key) ->
    gen_server:call(?SERVER,{req,#req_db_del{family=Family,key=Key}}).

%% @spec db_get(Family::string(),Key::string()) -> Result
%% @doc Get DB Entry.
db_get(Family,Key) ->
    gen_server:call(?SERVER,{req,#req_db_get{family=Family,key=Key}}).

%% @spec db_put(Family::string(),Key::string(),Value::string()) -> Result
%% @doc Put DB Entry.
db_put(Family,Key,Value) ->
    gen_server:call(?SERVER,{req,#req_db_put{family=Family,key=Key,value=Value}}).

%% @spec events(EventMask) -> Result
%% @doc Control Event Flow.
events(EventMask) ->
    gen_server:call(?SERVER,{req,#req_events{eventMask=EventMask}}).

%% @spec extension_state(Extension::string(),Context::string()) -> Result
%% @doc Check Extension Status.
extension_state(Extension,Context) ->
    gen_server:call(?SERVER,{req,#req_extension_state{extension=Extension,
						      context=Context}}).

%% @spec get_var(Channel::string(),Var::string()) -> Result
%% @doc Gets a Channel Variable.
get_var(Channel,Var) ->
    gen_server:call(?SERVER,{req,#req_get_var{channel=Channel,var=Var}}).

%% @spec hangup(Channel::string()) -> Result
%% @doc Hangup Channel.
hangup(Channel) ->
    gen_server:call(?SERVER,{req,#req_hangup{channel=Channel}}).

%% @spec iax_netstats() -> Result
%% @doc Show IAX Netstats.
iax_netstats() ->
    gen_server:call(?SERVER,{req,#req_iax_netstats{}}).

%% @spec iax_peers() -> Result
%% @doc List IAX Peers.
iax_peers() ->
    gen_server:call(?SERVER,{req,#req_iax_peers{}}).

%% @spec list_commands() -> Result
%% @doc List available manager commands.
list_commands() ->
    gen_server:call(?SERVER,{req,#req_list_commands{}}).

%% @spec logoff() -> Result
%% @doc Logoff Manager.
logoff() ->
    gen_server:call(?SERVER,{req,#req_logoff{}}).

%% @spec mailbox_count(Mbox::string()) -> Result
%% @doc Check Mailbox Message Count.
mailbox_count(Mbox) ->
    gen_server:call(?SERVER,{req,#req_mailbox_count{mailbox=Mbox}}).

%% @spec mailbox_status(Mailbox::string()) -> Result
%% @doc Check Mailbox.
mailbox_status(Mailbox) ->
    gen_server:call(?SERVER,{req,#req_mailbox_status{mailbox=Mailbox}}).

%% @spec monitor(Channel::string(),File::string(),Format::string(),Mix::boolean()) -> Result
%% @doc Monitor a channel.
monitor(Channel,File,Format,Mix) ->
    gen_server:call(?SERVER,{req,#req_monitor{channel=Channel,
					      file=File,
					      format=Format,
					      mix=Mix}}).

%% @spec originate(Channel::string(), Dest, Timeout::integer(), CID::string(), Vars, Acct::string(), Async::boolean()) -> Result
%% Dest = {Appl::string(), Data::string()} | {Context::string(), Extension::string(), Prio::string()}
%% Vars = [{Name::string(),Value::string()}]
%% @doc Originate Call.
originate(Channel, Dest, Timeout, CID, Vars, Acct, Async) ->
    gen_server:call(?SERVER,{req,#req_originate{channel=Channel,
						dest=Dest,
						timeout=Timeout,
						cid=CID,
						vars=Vars,
						account=Acct,
						async=Async}},30000).

%% @spec parked_calls() -> Result
%% @doc List parked calls.
parked_calls() ->
    gen_server:call(?SERVER,{req,#req_parked_calls{}}).

%% @spec ping() -> Result
%% @doc Keepalive command.
ping() ->
    gen_server:call(?SERVER,{req,#req_ping{}}).

%% @spec queue_add(Interface,Queue,Penalty,Pause) -> Result
%% @doc Add interface to queue.
queue_add(Interface,Queue,Penalty,Pause) ->
    gen_server:call(?SERVER,{req,#req_queue_add{interface=Interface,
						queue=Queue,
						penalty=Penalty,
						pause=Pause}}).

%% @spec queue_pause(Interface,Queue,Pause) -> Result
%% @doc Makes a queue member temporarily unavailable.
queue_pause(Interface,Queue,Pause) ->
    gen_server:call(?SERVER,{req,#req_queue_pause{interface=Interface,
						  queue=Queue,
						  pause=Pause}}).

%% @spec queue_remove(Interface,Queue) -> Result
%% @doc Remove interface from queue.
queue_remove(Interface,Queue) ->
    gen_server:call(?SERVER,{req,#req_queue_remove{interface=Interface,
						   queue=Queue}}).

%% @spec queues() -> Result
%% @doc Queues.
queues() ->
    gen_server:call(?SERVER,{req,#req_queues{}}).

%% @spec queue_status() -> Result
%% @doc Queue Status.
queue_status() ->
    gen_server:call(?SERVER,{req,#req_queue_status{}}).

%% @spec redirect(Channel::string(),Exten::string(),Context::string(),Priority::string()) -> Result
%% @doc Redirect (transfer) a call.
redirect(Channel,Exten,Context,Priority) ->
    gen_server:call(?SERVER,{req,#req_redirect{channel=Channel,
					       exten=Exten,
					       context=Context,
					       priority=Priority}}).

%% @spec redirect(Channel::string(),ExtraChannel::string(),Exten::string(),Context::string(),Priority::string()) -> Result
%% @doc Redirect (transfer) a call.
redirect(Channel,ExtraChannel,Exten,Context,Priority) ->
    gen_server:call(?SERVER,{req,#req_redirect{channel=Channel,
					       extrachannel=ExtraChannel,
					       exten=Exten,
					       context=Context,
					       priority=Priority}}).

%% @spec set_cdr_user_field(Channel::string(),Userfield::string(),Append::boolean()) -> Result
%% @doc Set the CDR UserField.
set_cdr_user_field(Channel,Userfield,Append) ->
    gen_server:call(?SERVER,{req,#req_set_cdr_user_field{channel=Channel,
							 userfield=Userfield,
							 append=Append}}).

%% @spec set_var(Channel,Variable,Value) -> Result
%% @doc Set Channel Variable.
set_var(Channel,Variable,Value) ->
    gen_server:call(?SERVER,{req,#req_set_var{channel=Channel,
					      variable=Variable,
					      value=Value}}).

%% @spec sip_showpeer(Peer) -> Result
%% @doc Show SIP peer (text format).
sip_showpeer(Peer) ->
    gen_server:call(?SERVER,{req,#req_sip_showpeer{peer=Peer}}).

%% @spec sip_peers() -> Result
%% @doc List SIP peers (text format).
sip_peers() ->
    gen_server:call(?SERVER,{req,#req_sip_peers{}}).

%% @spec status() -> Result
%% @doc Lists channel status for all channels.
status() ->
    gen_server:call(?SERVER,{req,#req_status{}}).

%% @spec status(Channel) -> Result
%% @doc Lists channel status.
status(Channel) ->
    gen_server:call(?SERVER,{req,#req_status{channel=Channel}}).

%% @spec stop_monitor(Channel) -> Result
%% @doc Stop monitoring a channel.
stop_monitor(Channel) ->
    gen_server:call(?SERVER,{req,#req_stop_monitor{channel=Channel}}).

%% @spec zap_dial_offhook(Channel,Number) -> Result
%% @doc Dial over Zap channel while offhook.
zap_dial_offhook(Channel,Number) ->
    gen_server:call(?SERVER,{req,#req_zap_dial_offhook{channel=Channel,
						       number=Number}}).

%% @spec zap_dnd_off(Channel) -> Result
%% @doc Toggle Zap channel Do Not Disturb status OFF.
zap_dnd_off(Channel) ->
    gen_server:call(?SERVER,{req,#req_zap_dnd_off{channel=Channel}}).

%% @spec zap_dnd_on(Channel) -> Result
%% @doc Toggle Zap channel Do Not Disturb status ON.
zap_dnd_on(Channel) ->
    gen_server:call(?SERVER,{req,#req_zap_dnd_on{channel=Channel}}).

%% @spec zap_hangup(Channel) -> Result
%% @doc Hangup Zap Channel.
zap_hangup(Channel) ->
    gen_server:call(?SERVER,{req,#req_zap_hangup{channel=Channel}}).

%% @spec zap_show_channels() -> Result
%% @doc Show status zapata channels.
zap_show_channels() ->
    gen_server:call(?SERVER,{req,#req_zap_show_channels{}}).

%% %@spec zap_transfer() -> Result
%% %@doc Transfer Zap Channel.
%% zap_transfer() ->
%% gen_server:call(?SERVER,{req,#req_zap_transfer{}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @private
%%--------------------------------------------------------------------
init([User, Passwd]) ->
    AId=login(User, Passwd),
    {ok, #state{queue=queue:new(),pending={AId,self,login},state=login}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @private
%%--------------------------------------------------------------------
handle_call({req,Req}, From, State) when State#state.state==free ->
    Pdu=ast_ami_pdu:encode(Req),
    AId=ast_ami_drv:send(Pdu),
    {noreply, State#state{pending={AId,From,Req}}};

handle_call({req,Request}, From, State) ->
    Q=State#state.queue,
    Q1=queue:in({Request,From},Q),
    {noreply, State#state{queue=Q1}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @private
%%--------------------------------------------------------------------
handle_cast({pdu,Msg}, State) when State#state.state==login ->
    {noreply, State#state{state=free}};

handle_cast({pdu,Msg}, State) ->
    M1 = parse_msg(Msg),
    S1=handle_pdu(msg_type(M1),State),
    {noreply, S1};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
login(User,Secret) ->
    Pdu=ast_ami_pdu:encode(#req_login{user=User,secret=Secret}),
    ast_ami_drv:send(Pdu).


%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
parse_msg([ET|Msg]) ->
    [ET | [split_line(L) || L <- Msg]].

split_line(L) ->
    Pos=string:str(L,": "),
    Lbl=string:substr(L,1,Pos-1),
    Value=string:substr(L,Pos+2),
    {Lbl,Value}.


%% @doc There are 2 types of messages, event and response.
%% They are identified by a header with "Event" or "Response".
%% But there is a small complication some events are really part of
%% a response, those can be identified by the presence of an "ActionId"
%% line, (this requires that "ActionId" was set in the request).
%% @end.
msg_type(Msg) ->
    case check_event(Msg) of
	false ->
	    check_result(Msg);
	Result ->
	    Result
    end.

check_event(Msg) ->
    check_event1(lists:keysearch("Event",1,Msg),Msg).

check_event1({value,{"Event",Type}},Msg) ->
    case lists:keysearch("ActionID",1,Msg) of
	{value,{"ActionID",AId}} ->
	    {response,AId,undefined,Msg};
	false  ->
	    {event,Type,Msg}
    end;

check_event1(false,_Msg) ->
    false.

check_result(Msg) ->
    {value,{"Response",Result}}=lists:keysearch("Response",1,Msg),
    {value,{"ActionID",AId}}=lists:keysearch("ActionID",1,Msg),
    {response,list_to_integer(AId),Result,Msg}.

handle_pdu({response,AId,Res,Pdu},State) ->
    handle_response(State#state.pending,Res,Pdu,State);

handle_pdu({event,_Event,_Pdu}=M,State) ->
    ast_ami_events:event(M),
    State.

handle_response({_AId,_From,login},_Res,_Pdu,State) ->
    State#state{pending=undefined,state=free};

%% handle_response({AId,From,Cmd},undefined,Pdu,State) ->
%%     io:format("~p~n",[Pdu]),
%%     State;

handle_response({AId,From,Cmd},Res,Pdu,State) when State#state.list_resp==false ->
    io:format("hr1~n",[]),
    case lists:keysearch("Message",1,Pdu) of
	{value,{"Message",Txt}} ->
	    case lists:member(Txt,[
				   "Agents will follow",
				   "Channel status will follow",
				   "Originate successfully queued",
				   "Queue status will follow",
				   "Parked calls will follow",
				   "Peer status list will follow",
				   "Result will follow"
				  ]) of
		true ->
		    State#state{list_resp=true,
				resp_acc=[Pdu|State#state.resp_acc]};
		false ->
		    gen_server:reply(From,{Cmd,Res,Pdu}),
		    State#state{pending=undefined,state=free}
	    end;
	false ->
	    gen_server:reply(From,{Cmd,Res,Pdu}),
	    State#state{pending=undefined,state=free}
    end;

%% handle_response({AId,From,Cmd},Res,Pdu,State) when State#state.list_resp==false ->
%%     case lists:keysearch("Message",1,Pdu) of
%% 	{value,{"Message",Txt}} ->
%% 	    gen_server:reply(From,{Cmd,Res,Pdu}),
%% 	    State#state{pending=undefined,state=free};
%% %% 	    State#state{list_resp=true,
%% %% 			resp_acc=[Pdu|State#state.resp_acc]};
%% 	false ->
%% 	    gen_server:reply(From,{Cmd,Res,Pdu}),
%% 	    State#state{pending=undefined,state=free}
%%     end;

handle_response({AId,From,Cmd},Res,Pdu,State) ->
    case lists:keysearch("Event",1,Pdu) of
	{value,{"Event","AgentsComplete"}} ->
	    resp_done(From,Cmd,State);
	{value,{"Event","DBGetResponse"}} ->
	    resp_done(From,Cmd,State#state{resp_acc=[Pdu|State#state.resp_acc]});
	{value,{"Event","OriginateSuccess"}} ->
	    resp_done(From,Cmd,State#state{resp_acc=[Pdu|State#state.resp_acc]});
	{value,{"Event","ParkedCallsComplete"}} ->
	    resp_done(From,Cmd,State);
	{value,{"Event","PeerlistComplete"}} ->
	    resp_done(From,Cmd,State);
	{value,{"Event","QueueStatusComplete"}} ->
	    resp_done(From,Cmd,State);
	{value,{"Event","StatusComplete"}} ->
	    resp_done(From,Cmd,State);
	{value,{"Event",Evt}} ->
	    State#state{list_resp=true,
			resp_acc=[Pdu|State#state.resp_acc]}
    end.    

resp_done(From,Cmd,State) ->
    Resp=lists:reverse(State#state.resp_acc),
    gen_server:reply(From,{Cmd,success,Resp}),
    State#state{pending=undefined,state=free,
		resp_acc=[],list_resp=false}.

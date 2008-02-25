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
%%% File    : ast_manager.erl
%%% Author  : anders <anders@>
%%% Description : 
%%%
%%% Created : 20 Feb 2006 by anders <anders@>
%%%-------------------------------------------------------------------
-module(ast_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 subscribe/3,
 	 absolute_timeout/2,
 	 agent_callback_login/2,agent_callback_login/5,
 	 agent_logoff/1,agent_logoff/2,
 	 agents/0,
 	 change_monitor/2,
 	 command/1,
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
	 mailboxcount/1,
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
 	 redirect/5,
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{debug,[trace]}]).

%% @doc Subscribe to events.
subscribe(User,Secret,Events) ->
    ast_man_events:subscribe(User,Secret,Events).

%% @doc Receive new pdu from asterisk.
%% @private
new_pdu(Msg) ->
    gen_server:cast(?SERVER,{pdu,Msg}).

%%@doc Set Absolute Timeout.
absolute_timeout(Channel,Timeout) ->
    gen_server:call(?SERVER,{req,#absolute_timeout{channel=Channel,
						   timeout=Timeout}}).

%% @doc Sets an agent as logged in by callback.
agent_callback_login(Agent,Exten) ->
    gen_server:call(?SERVER,{req,#agent_callback_login{agent=Agent,
						       exten=Exten}}).

%% @doc Sets an agent as logged in by callback.
agent_callback_login(Agent,Exten,Context,AckCall,WrapUpTime) ->
    gen_server:call(?SERVER,{req,#agent_callback_login{agent=Agent,
						       exten=Exten,
						       context=Context,
						       ackCall=AckCall,
						       wrapUpTime=WrapUpTime}}).

%% @doc Sets an agent as no longer logged in.
%% @equiv agent_logoff(Agent,true)
agent_logoff(Agent) ->
    agent_logoff(Agent,true).

%% @doc Sets an agent as no longer logged in.
agent_logoff(Agent,Soft) ->
    gen_server:call(?SERVER,{req,#agent_logoff{agent=Agent,
					       soft=Soft}}).

%% @doc Lists agents and their status.
agents() ->
    gen_server:call(?SERVER,{req,#agents{}}).

%% @doc Change monitoring filename of a channel.
change_monitor(Channel,File) ->
    gen_server:call(?SERVER,{req,#change_monitor{channel=Channel,
						  file=File}}).

%% @doc Execute Asterisk CLI Command.
command(Command) ->
    gen_server:call(?SERVER,{req,#command{command=Command}}).

%% @doc Get DB Entry.
db_get(Family,Key) ->
    gen_server:call(?SERVER,{req,#db_get{family=Family,key=Key}}).

%% @doc Put DB Entry.
db_put(Family,Key,Value) ->
    gen_server:call(?SERVER,{req,#db_put{family=Family,key=Key,value=Value}}).

%% @doc Control Event Flow.
events(EventMask) ->
    gen_server:call(?SERVER,{req,#events{eventMask=EventMask}}).

%% @doc Check Extension Status.
extension_state(Extension,Context) ->
    gen_server:call(?SERVER,{req,#extension_state{extension=Extension,
						  context=Context}}).

%% @doc Gets a Channel Variable.
get_var(Channel,Var) ->
    gen_server:call(?SERVER,{req,#get_var{channel=Channel,var=Var}}).

%% @doc Hangup Channel.
hangup(Channel) ->
    gen_server:call(?SERVER,{req,#hangup{channel=Channel}}).

%% @doc Show IAX Netstats.
iax_netstats() ->
    gen_server:call(?SERVER,{req,#iax_netstats{}}).

%% @doc List IAX Peers.
iax_peers() ->
    gen_server:call(?SERVER,{req,#iax_peers{}}).

%% @doc List available manager commands.
list_commands() ->
    gen_server:call(?SERVER,{req,#list_commands{}}).

%% @doc Logoff Manager.
logoff() ->
    gen_server:call(?SERVER,{req,#logoff{}}).

%% @doc Check Mailbox Message Count.
mailboxcount(Mbox) ->
    gen_server:call(?SERVER,{req,#mailboxcount{mailbox=Mbox}}).

%% @doc Check Mailbox.
mailbox_status(Mailbox) ->
    gen_server:call(?SERVER,{req,#mailbox_status{mailbox=Mailbox}}).

%% @doc Monitor a channel.
monitor(Channel,File,Format,Mix) ->
    gen_server:call(?SERVER,{req,#monitor{channel=Channel,
					  file=File,
					  format=Format,
					  mix=Mix}}).

%% %@doc Originate Call.
originate(Channel, Dest, Timeout, CID, Vars, Acct, Async) ->
    gen_server:call(?SERVER,{req,#originate{channel=Channel,
					    dest=Dest,
					    timeout=Timeout,
					    cid=CID,
					    vars=Vars,
					    account=Acct,
					    async=Async}},10000).

%% @doc List parked calls.
parked_calls() ->
    gen_server:call(?SERVER,{req,#parked_calls{}}).

%% @doc Keepalive command.
ping() ->
    gen_server:call(?SERVER,{req,#ping{}}).

%% @doc Add interface to queue.
queue_add(Interface,Queue,Penalty,Pause) ->
    gen_server:call(?SERVER,{req,#queue_add{interface=Interface,
					    queue=Queue,
					    penalty=Penalty,
					    pause=Pause}}).

%% @doc Makes a queue member temporarily unavailable.
queue_pause(Interface,Queue,Pause) ->
    gen_server:call(?SERVER,{req,#queue_pause{interface=Interface,
					      queue=Queue,
					      pause=Pause}}).

%% @doc Remove interface from queue.
queue_remove(Interface,Queue) ->
    gen_server:call(?SERVER,{req,#queue_remove{interface=Interface,
					       queue=Queue}}).

%% @doc Queues.
queues() ->
    gen_server:call(?SERVER,{req,#queues{}}).

%% @doc Queue Status.
queue_status() ->
    gen_server:call(?SERVER,{req,#queue_status{}}).

%% @doc Redirect (transfer) a call.
redirect(Channel,ExtraChannel,Exten,Context,Priority) ->
    gen_server:call(?SERVER,{req,#redirect{channel=Channel,
					   extrachannel=ExtraChannel,
					   exten=Exten,
					   context=Context,
					   priority=Priority}}).

%% @doc Set the CDR UserField.
set_cdr_user_field(Channel,Userfield,Append) ->
    gen_server:call(?SERVER,{req,#set_cdr_user_field{channel=Channel,
						     userfield=Userfield,
						     append=Append}}).

%% @doc Set Channel Variable.
set_var(Channel,Variable,Value) ->
    gen_server:call(?SERVER,{req,#set_var{channel=Channel,
					  variable=Variable,
					  value=Value}}).

%% @doc Show SIP peer (text format).
sip_showpeer(Peer) ->
    gen_server:call(?SERVER,{req,#sip_showpeer{peer=Peer}}).

%% @doc List SIP peers (text format).
sip_peers() ->
    gen_server:call(?SERVER,{req,#sip_peers{}}).

%% @doc Lists channel status for all channels.
status() ->
    gen_server:call(?SERVER,{req,#status{}}).

%% @doc Lists channel status.
status(Channel) ->
    gen_server:call(?SERVER,{req,#status{channel=Channel}}).

%% @doc Stop monitoring a channel.
stop_monitor(Channel) ->
    gen_server:call(?SERVER,{req,#stop_monitor{channel=Channel}}).

%% @doc Dial over Zap channel while offhook.
zap_dial_offhook(Channel,Number) ->
    gen_server:call(?SERVER,{req,#zap_dial_offhook{channel=Channel,
						   number=Number}}).

%% @doc Toggle Zap channel Do Not Disturb status OFF.
zap_dnd_off(Channel) ->
    gen_server:call(?SERVER,{req,#zap_dnd_off{channel=Channel}}).

%% @doc Toggle Zap channel Do Not Disturb status ON.
zap_dnd_on(Channel) ->
    gen_server:call(?SERVER,{req,#zap_dnd_on{channel=Channel}}).

%% @doc Hangup Zap Channel.
zap_hangup(Channel) ->
    gen_server:call(?SERVER,{req,#zap_hangup{channel=Channel}}).

%% @doc Show status zapata channels.
zap_show_channels() ->
    gen_server:call(?SERVER,{req,#zap_show_channels{}}).

%% %@doc Transfer Zap Channel.
%% zap_transfer() ->
%% gen_server:call(?SERVER,{req,#zap_transfer{}}).

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
init([]) ->
    AId=login("anders","secret"),
    {ok, #state{queue=queue:new(),pending={AId,self,login},state=free}}.
%%    {ok, #state{queue=queue:new(),pending={AId,self,login},state=login}}.

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
    Pdu=make_pdu(Req),
    AId=ast_man_drv:send(Pdu),
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
handle_cast({pdu,Msg}, State) ->
    S1=handle_pdu(msg_type(Msg),State),
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
    Pdu=make_pdu(#login{user=User,secret=Secret}),
    ast_man_drv:send(Pdu).

make_pdu(#absolute_timeout{channel=Channel,timeout=Timeout} ) ->
    ["Action: AbsoluteTimeout\r\n"
     "Channel: ",Channel,"\r\n"
     "Timeout: ",Timeout,"\r\n"];

make_pdu(#agent_callback_login{agent=Agent,
			       exten=Exten,
			       context=Context,
			       ackCall=AckCall,
			       wrapUpTime=WrapUpTime} ) ->
    ["Action: AgentCallBackLogin \r\n"
     "Agent: ",Agent,"\r\n"
     "Exten: ",Exten,"\r\n"
     "Context: ",Context,"\r\n"
     "AckCall: ",AckCall,"\r\n"
     "WrapUpTime: ",WrapUpTime,"\r\n"];

make_pdu( #agent_logoff{agent=Agent,soft=Soft}) ->
    ["Action: AgentLogoff\r\n"
     "Agent: ",Agent,"\r\n"
     "soft: ",Soft,"\r\n"];

make_pdu(#agents{} ) ->
    "Action: Agents\r\n";

make_pdu(#change_monitor{channel=Channel,file=File} ) ->
    ["Action: ChangeMonitor\r\n"
     "Channel: ",Channel,"\r\n"
     "File: ",File,"\r\n"];

make_pdu(#command{command=Command} ) ->
    ["Action: Command\r\n"
     "Command: ",Command,"\r\n"];

make_pdu(#db_get{family=Family,key=Key} ) ->
    ["Action: DBGet\r\n"
     "Family: ",Family,"\r\n"
     "Key: ",Key ,"\r\n"];

make_pdu(#db_put{family=Family,key=Key,value=Value} ) ->
    ["Action: DBPut\r\n"
     "Family: ",Family,"\r\n"
     "Key: ",Key,"\r\n"
     "Value: ",Value,"\r\n"];

make_pdu(#events{eventMask=EventMask} ) ->
    ["Action: Events\r\n"
     "EventMask: ",EventMask,"\r\n"];

make_pdu(#extension_state{extension=Extension,context=Context} ) ->
    ["Action: ExtensionState\r\n"
     "Extension: ",Extension,"\r\n"
     "Context: ",Context,"\r\n"];

make_pdu(#get_var{channel=Channel,var=Var}) ->
    ["Action: GetVar\r\n"
     "Channel: ",Channel,"\r\n"
     "Var: ",Var,"\r\n"];

make_pdu(#hangup{channel=Channel}) ->
    ["Action: Hangup\r\n"
     "Channel: ",Channel,"\r\n"];

make_pdu(#iax_netstats{} ) ->
    "Action: IAXnetstats\r\n";

make_pdu(#iax_peers{} ) ->
    "Action: IAXPeers\r\n";

make_pdu(#list_commands{} ) ->
    "Action: ListCommands\r\n";

make_pdu(#login{user=User,secret=Secret}) ->
    ["Action: login\r\n"
     "Username: ",User,"\r\n"
     "Secret: ",Secret,"\r\n"];

make_pdu(#mailboxcount{mailbox=Mbox}) ->
    ["Action: MailboxCount\r\n",
     "Mailbox: ",Mbox,"\r\n"];

make_pdu(#mailbox_status{mailbox=Mailbox}) ->
    ["Action: MailboxStatus\r\n"
     "Mailbox: ",Mailbox,"\r\n"];

make_pdu(#monitor{channel=Channel,file=File,format=Format,mix=Mix}) ->
    ["Action: Monitor\r\n"
     "Channel: ",Channel,"\r\n"
     "File: ",File,"\r\n"
     "Format: ",Format,"\r\n"
     "Mix: ",Mix,"\r\n"];

make_pdu(#originate{channel=Channel,dest={Appl,Data},timeout=Timeout,cid=CID,vars=Vars,account=Acct,async=Async}) ->
    ["Action: Originate\r\n"
     "Channel: ",Channel,"\r\n"
     "Application: ", Appl, "\r\n"
     "Data: ", Data, "\r\n"
     "Timeout: ", integer_to_list(Timeout), "\r\n"
     "CallerId: ", CID, "\r\n"
     "Account: ", Acct, "\r\n"
     "Async: ", atom_to_list(Async), "\r\n"];

make_pdu(#originate{channel=Channel,dest={Context,Exten,Prio},timeout=Timeout,cid=CID,vars=Vars,account=Acct,async=Async}) ->
    ["Action: Originate\r\n"
     "Channel: ",Channel,"\r\n"
     "Context: ", Context, "\r\n"
     "Extension: ", Exten, "\r\n"
     "Priority: ", Prio, "\r\n"
     "Timeout: ", integer_to_list(Timeout), "\r\n"
     "CallerId: ", CID, "\r\n"
     "Account: ", Acct, "\r\n"
     "Async: ", atom_to_list(Async), "\r\n"];

make_pdu(#parked_calls{}) ->
    "Action: ParkedCalls\r\n";
    
make_pdu(#ping{}) ->
    "Action: Ping\r\n";

make_pdu(#queue_add{interface=Interface,
		    queue=Queue,
		    penalty=Penalty,
		    pause=Pause}) ->
    ["Action: QueueAdd\r\n"
     "Interface: ",Interface,"\r\n"
     "Queue: ",Queue,"\r\n"
     "Penalty: ",Penalty,"\r\n"
     "Pause: ",Pause,"\r\n"  ];

make_pdu(#queue_pause{interface=Interface,
		      queue=Queue,
		      pause=Pause}) ->
    ["Action: QueuePause\r\n"
     "Interface: ",Interface,"\r\n"
     "Queue: ",Queue,"\r\n"
     "Pause: ",Pause,"\r\n"  ];

make_pdu(#queue_remove{interface=Interface,
		       queue=Queue}) ->
    ["Action: QueueRemove\r\n"
     "Interface: ",Interface,"\r\n"
     "Queue: ",Queue, "\r\n"  ];

make_pdu(#queues{}) ->
    ["Action: Queues\r\n"
     ];

make_pdu(#queue_status{}) ->
    ["Action: QueueStatus\r\n"
     ];

make_pdu(#redirect{channel=Channel,
		   extrachannel=ExtraChannel,
		   exten=Exten,
		   context=Context,
		   priority=Priority}) ->
    ["Action: Redirect\r\n"
     "Channel: ",Channel,"\r\n"
     "Extrachannel: ",ExtraChannel,"\r\n"
     "Exten: ",Exten,"\r\n"
     "Context: ",Context,"\r\n"
     "Priority: ",Priority , "\r\n" ];

make_pdu(#set_cdr_user_field{channel=Channel,
			     userfield=Userfield,
			     append=Append}) ->
    ["Action: SetCDRUserField\r\n"
     "Channel: ",Channel,"\r\n"
     "Userfield: ",Userfield,"\r\n"
     "Append: ",Append, "\r\n"   ];

make_pdu(#set_var{channel=Channel,
		  variable=Variable,
		  value=Value}) ->
    ["Action: SetVar\r\n"
     "Channel: ",Channel,"\r\n"
     "Variable: ",Variable,"\r\n"
     "Value: ",Value, "\r\n"   ];

make_pdu(#sip_showpeer{peer=Peer}) ->
    ["Action: SIPshowpeer\r\n",
     "Peer: ",Peer,"\r\n"];

make_pdu(#sip_peers{}) ->
    "Action: SIPpeers\r\n";

make_pdu(#status{}) ->
    "Action: Status\r\n";

make_pdu(#stop_monitor{channel=Channel}) ->
    ["Action: StopMonitor\r\n"
     "Channel: ",Channel,"\r\n"];

make_pdu(#zap_dial_offhook{channel=Channel,
			   number=Number}) ->
    ["Action: ZapDialOffhook\r\n"
     "Channel: ",Channel,"\r\n"
     "Number: ",Number,"\r\n"];

make_pdu(#zap_dnd_off{channel=Channel}) ->
    ["Action: ZapDNDOff\r\n"
     "Channel: ",Channel,"\r\n"];

make_pdu(#zap_dnd_on{channel=Channel}) ->
    ["Action: ZapDNDOn\r\n"
     "Channel: ",Channel,"\r\n"];

make_pdu(#zap_hangup{channel=Channel}) ->
    ["Action: ZapHangup\r\n"
     "Channel: ",Channel,"\r\n"];

make_pdu(#zap_show_channels{}) ->
    "Action: ZapShowChannels\r\n".

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------

%% @doc There are 3 types of messages, alarm, event and response.
%% They are identified by a header with "Alarm","Event" or "Response".
%% But there is a small complication some events are really part of
%% a response, those can be identified by the presence of an "ActionId"
%% line, (this requires that "ActionId" was set in the request.
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
    ast_man_events:event(M),
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
	    case lists:member(Txt,["Peer status list will follow",
				   "Originate successfully queued" ]) of
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

handle_response({AId,From,Cmd},Res,Pdu,State) ->
io:format("hr2~n",[]),
    case lists:keysearch("Event",1,Pdu) of
	{value,{"Event","OriginateSuccess"}} ->
	    resp_done(From,Cmd,State#state{resp_acc=[Pdu|State#state.resp_acc]});
	{value,{"Event","PeerlistComplete"}} ->
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

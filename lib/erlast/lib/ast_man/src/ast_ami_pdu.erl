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
%%% @doc Pack and unpack pdus.
%%% @end 
%%% Created : 10 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_ami_pdu).

%% @headerfile "manager_api.hrl"
-include("manager_api.hrl").

%% API
-export([encode/1,
	 parse/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec encode(Cmd::ami_cmd()) -> io_list()
%% @doc Create a binary pdu from a command record.
%% @end
%%--------------------------------------------------------------------
encode(#req_absolute_timeout{channel=Channel,timeout=Timeout} ) ->
    pack_pdu([{"Action","AbsoluteTimeout"},
	      {"Channel",Channel},
	      {"Timeout",Timeout}]);

encode(#req_agent_callback_login{agent=Agent,
				 exten=Exten,
				 context=Context,
				 ackCall=AckCall,
				 wrapUpTime=WrapUpTime} ) ->
    pack_pdu([{"Action","AgentCallBackLogin"},
	      {"Agent",Agent},
	      {"Exten",Exten},
	      {"Context",Context},
	      {"AckCall",AckCall},
	      {"WrapUpTime",WrapUpTime}]);

encode( #req_agent_logoff{agent=Agent,soft=Soft}) ->
    pack_pdu([{"Action","AgentLogoff"},
	      {"Agent",Agent},
	      {"soft",Soft}]);

encode(#req_agents{} ) ->
    pack_pdu([{"Action","Agents"}]);

encode(#req_challenge{authtype=AuthType}) ->
    pack_pdu([{"Action","Challenge"},
	      {"AuthType",AuthType}]);

encode(#req_change_monitor{channel=Channel,file=File} ) ->
    pack_pdu([{"Action","ChangeMonitor"},
	      {"Channel",Channel},
	      {"File",File}]);

encode(#req_command{command=Command} ) ->
    pack_pdu([{"Action","Command"},
	      {"Command",Command}]);

encode(#req_db_del{family=Family,key=Key} ) ->
    pack_pdu([{"Action","DBDel"},
	      {"Family",Family},
	      {"Key",Key }]);

encode(#req_db_get{family=Family,key=Key} ) ->
    pack_pdu([{"Action","DBGet"},
	      {"Family",Family},
	      {"Key",Key }]);

encode(#req_db_put{family=Family,key=Key,value=Value} ) ->
    pack_pdu([{"Action","DBPut"},
	      {"Family",Family},
	      {"Key",Key},
	      {"Value",Value}]);

encode(#req_events{eventMask=EventMask} ) ->
    pack_pdu([{"Action","Events"},
	      {"EventMask",EventMask}]);

encode(#req_extension_state{extension=Extension,context=Context} ) ->
    pack_pdu([{"Action","ExtensionState"},
	      {"Extension",Extension},
	      {"Context",Context}]);

encode(#req_get_var{channel=Channel,var=Var}) ->
    pack_pdu([{"Action","GetVar"},
	      {"Channel",Channel},
	      {"Var",Var}]);

encode(#req_hangup{channel=Channel}) ->
    pack_pdu([{"Action","Hangup"},
	      {"Channel",Channel}]);

encode(#req_iax_netstats{} ) ->
    pack_pdu([{"Action","IAXnetstats"}]);

encode(#req_iax_peers{} ) ->
    pack_pdu([{"Action","IAXPeers"}]);

encode(#req_list_commands{} ) ->
    pack_pdu([{"Action","ListCommands"}]);

encode(#req_login{user=User,secret=Secret}) ->
    pack_pdu([{"Action","login"},
	      {"Username",User},
	      {"Secret",Secret}]);

encode(#req_mailbox_count{mailbox=Mbox}) ->
    pack_pdu([{"Action","MailboxCount"},
	      {"Mailbox",Mbox}]);

encode(#req_mailbox_status{mailbox=Mailbox}) ->
    pack_pdu([{"Action","MailboxStatus"},
	      {"Mailbox",Mailbox}]);

encode(#req_monitor{channel=Channel,file=File,format=Format,mix=Mix}) ->
    pack_pdu([{"Action","Monitor"},
	      {"Channel",Channel},
	      {"File",File},
	      {"Format",Format},
	      {"Mix",Mix}]);

encode(#req_originate{channel=Channel,dest={Appl,Data},timeout=Timeout,cid=CID,vars=Vars,account=Acct,async=Async}) ->
    pack_pdu([{"Action","Originate"},
	      {"Channel",Channel},
	      {"Application", Appl},
	      {"Data", Data},
	      {"Timeout", integer_to_list(Timeout)},
	      {"CallerId", CID},
	      {"Account", Acct},
	      {"Async", atom_to_list(Async)}]);

encode(#req_originate{channel=Channel,dest={Context,Exten,Prio},timeout=Timeout,cid=CID,vars=Vars,account=Acct,async=Async}) ->
    pack_pdu([{"Action","Originate"},
	      {"Channel",Channel},
	      {"Context", Context},
	      {"Extension", Exten},
	      {"Priority", Prio},
	      {"Timeout", integer_to_list(Timeout)},
	      {"CallerId", CID},
	      {"Account", Acct},
	      {"Async", atom_to_list(Async)}]);

encode(#req_parked_calls{}) ->
    pack_pdu([{"Action","ParkedCalls"}]);

encode(#req_ping{}) ->
    pack_pdu([{"Action","Ping"}]);

encode(#req_queue_add{interface=Interface,
		      queue=Queue,
		      penalty=Penalty,
		      pause=Pause}) ->
    pack_pdu([{"Action","QueueAdd"},
	      {"Interface",Interface},
	      {"Queue",Queue},
	      {"Penalty",Penalty},
	      {"Pause",Pause}]);

encode(#req_queue_pause{interface=Interface,
			queue=Queue,
			pause=Pause}) ->
    pack_pdu([{"Action","QueuePause"},
	      {"Interface",Interface},
	      {"Queue",Queue},
	      {"Pause",Pause}]);

encode(#req_queue_remove{interface=Interface,
			 queue=Queue}) ->
    pack_pdu([{"Action","QueueRemove"},
	      {"Interface",Interface},
	      {"Queue",Queue}]);

encode(#req_queues{}) ->
    pack_pdu([{"Action","Queues"}]);

encode(#req_queue_status{}) ->
    pack_pdu([{"Action","QueueStatus"}]);


encode(#req_redirect{channel=Channel,
		     extrachannel=ExtraChannel,
		     exten=Exten,
		     context=Context,
		     priority=Priority}) ->
    pack_pdu([{"Action","Redirect"},
	      {"Channel",Channel},
	      {"Extrachannel",ExtraChannel},
	      {"Exten",Exten},
	      {"Context",Context},
	      {"Priority",Priority}]);

encode(#req_set_cdr_user_field{channel=Channel,
			       userfield=Userfield,
			       append=Append}) ->
    pack_pdu([{"Action","SetCDRUserField"},
	      {"Channel",Channel},
	      {"Userfield",Userfield},
	      {"Append",Append}]);

encode(#req_set_var{channel=Channel,
		    variable=Variable,
		    value=Value}) ->
    pack_pdu([{"Action","SetVar"},
	      {"Channel",Channel},
	      {"Variable",Variable},
	      {"Value",Value}]);

encode(#req_sip_showpeer{peer=Peer}) ->
    pack_pdu([{"Action","SIPshowpeer"},
	      {"Peer",Peer}]);

encode(#req_sip_peers{}) ->
    pack_pdu([{"Action","SIPpeers"}]);

encode(#req_status{}) ->
    pack_pdu([{"Action","Status"}]);

encode(#req_stop_monitor{channel=Channel}) ->
    pack_pdu([{"Action","StopMonitor"},
	      {"Channel",Channel}]);

encode(#req_zap_dial_offhook{channel=Channel,
			     number=Number}) ->
    pack_pdu([{"Action","ZapDialOffhook"},
	      {"Channel",Channel},
	      {"Number",Number}]);

encode(#req_zap_dnd_off{channel=Channel}) ->
    pack_pdu([{"Action","ZapDNDOff"},
	      {"Channel",Channel}]);

encode(#req_zap_dnd_on{channel=Channel}) ->
    pack_pdu([{"Action","ZapDNDOn"},
	      {"Channel",Channel}]);

encode(#req_zap_hangup{channel=Channel}) ->
    pack_pdu([{"Action","ZapHangup"},
	      {"Channel",Channel}]);

encode(#req_zap_show_channels{}) ->
    pack_pdu([{"Action","ZapShowChannels"}]).

%%--------------------------------------------------------------------
%% @spec parse_action(Action::string(),Pars::list()) -> :ami_cmd()
%% @doc Create an action request record from a .
%% @end
%%--------------------------------------------------------------------
parse_action("AbsoluteTimeout",Pdu) ->
    #req_absolute_timeout{channel=get_pdu_var("Channel",Pdu),
			  timeout=get_pdu_var("Timeout",Pdu)};
parse_action("AgentCallbackLogin",Pdu) ->
    #req_agent_callback_login{agent=get_pdu_var("Agent",Pdu),
			      exten=get_pdu_var("Exten",Pdu),
			      context=get_pdu_var("Context",Pdu),
			      ackCall=get_pdu_var("AckCall",Pdu),
			      wrapUpTime=get_pdu_var("WrapUpTime",Pdu)};
parse_action("AgentLogoff",Pdu) ->
    #req_agent_logoff{agent=get_pdu_var("Agent",Pdu),
		      soft=get_pdu_var("Soft",Pdu)};

parse_action("Agents",_Pdu) ->
    #req_agents{};

parse_action("ChangeMonitor",Pdu) ->
    #req_change_monitor{channel=get_pdu_var("Channel",Pdu),
			file=get_pdu_var("File",Pdu)};

parse_action("Command",Pdu) ->
    #req_command{command=get_pdu_var("Command",Pdu)};

parse_action("DBGet",Pdu) ->
    #req_db_get{family=get_pdu_var("Family",Pdu),
		key=get_pdu_var("Key",Pdu)};

parse_action("DBPut",Pdu) ->
    #req_db_put{family=get_pdu_var("Family",Pdu),
		key=get_pdu_var("Key",Pdu),
		value=get_pdu_var("Value",Pdu)};

parse_action("Events",Pdu) ->
    #req_events{eventMask=get_pdu_var("EventMask",Pdu)};

parse_action("ExtensionState",Pdu) ->
    #req_extension_state{extension=get_pdu_var("Extension",Pdu),
			 context=get_pdu_var("Context",Pdu)};

parse_action("GetVar",Pdu) ->
    #req_get_var{channel=get_pdu_var("Channel",Pdu),
		 var=get_pdu_var("Var",Pdu)};

parse_action("Hangup",Pdu) ->
    #req_hangup{channel=get_pdu_var("Channel",Pdu)};

parse_action("IAXNetStats",_Pdu) ->
    #req_iax_netstats{};

parse_action("IAXPeers",_Pdu) ->
    #req_iax_peers{};

parse_action("ListCommands",_Pdu) ->
    #req_list_commands{};

parse_action("Login",Pdu) ->
    #req_login{user=get_pdu_var("User",Pdu),
	       secret=get_pdu_var("Secret",Pdu)};

parse_action("Logoff",_Pdu) ->
    #req_logoff{};

parse_action("MailboxCount",Pdu) ->
    #req_mailbox_count{mailbox=get_pdu_var("Mailbox",Pdu),
		       new=get_pdu_var("NewMessages",Pdu),
		       old=get_pdu_var("OldMessages",Pdu)};

parse_action("MailboxStatus",Pdu) ->
    #req_mailbox_status{mailbox=get_pdu_var("Mailbox",Pdu),
			waiting=get_pdu_var("Waiting",Pdu)};

parse_action("Monitor",Pdu) ->
    #req_monitor{channel=get_pdu_var("Channel",Pdu),
		 file=get_pdu_var("File",Pdu),
		 format=get_pdu_var("Format",Pdu),
		 mix=get_pdu_var("Mix",Pdu)};

parse_action("Originate",Pdu) ->
    #req_originate{channel=get_pdu_var("Channel",Pdu)};

parse_action("ParkedCalls",_Pdu) ->
    #req_parked_calls{};

parse_action("Ping",_Pdu) ->
    #req_ping{};

parse_action("QueueAdd",Pdu) ->
    #req_queue_add{interface=get_pdu_var("Interface",Pdu),
		   queue=get_pdu_var("Queue",Pdu),
		   penalty=get_pdu_var("Penalty",Pdu),
		   pause=get_pdu_var("Pause",Pdu)};

parse_action("QueuePause",Pdu) ->
    #req_queue_pause{interface=get_pdu_var("Interface",Pdu),
		     queue=get_pdu_var("Queue",Pdu),
		     penalty=get_pdu_var("Penalty",Pdu),
		     pause=get_pdu_var("Pause",Pdu)};

parse_action("QueueRemoveInterface",Pdu) ->
    #req_queue_remove{interface=get_pdu_var("Interface",Pdu),
		      queue=get_pdu_var("Queue",Pdu)};

parse_action("Queues",_Pdu) ->
    #req_queues{};

parse_action("QueueStatus",_Pdu) ->
    #req_queue_status{};

parse_action("Redirect",Pdu) ->
    #req_redirect{channel=get_pdu_var("Channel",Pdu),
		  extrachannel=get_pdu_var("ExtraChannel",Pdu),
		  exten=get_pdu_var("Exten",Pdu),
		  context=get_pdu_var("",Pdu),
		  priority=get_pdu_var("",Pdu)};

parse_action("set_cdr_user_field",Pdu) ->
    #req_set_cdr_user_field{channel=get_pdu_var("Channel",Pdu),
			    userfield=get_pdu_var("UserField",Pdu),
			    append=get_pdu_var("Append",Pdu)};

parse_action("SetVar",Pdu) ->
    #req_set_var{channel=get_pdu_var("Channel",Pdu),
		 variable=get_pdu_var("Variable",Pdu),
		 value=get_pdu_var("Value",Pdu)};

parse_action("SIPshowpeer",Pdu) ->
    #req_sip_showpeer{peer=get_pdu_var("Peer",Pdu)};

parse_action("SIPpeers",_Pdu) ->
    #req_sip_peers{};

parse_action("Status",Pdu) ->
    #req_status{channel=get_pdu_var("Channel",Pdu)};

parse_action("StopMonitor",Pdu) ->
    #req_stop_monitor{channel=get_pdu_var("Channel",Pdu)};

parse_action("ZapDialOffhook",Pdu) ->
    #req_zap_dial_offhook{channel=get_pdu_var("Channel",Pdu),
			  number=get_pdu_var("Number",Pdu)};

parse_action("ZapDndOff",Pdu) ->
    #req_zap_dnd_off{channel=get_pdu_var("Channel",Pdu)};

parse_action("ZapDndOn",Pdu) ->
    #req_zap_dnd_on{channel=get_pdu_var("Channel",Pdu)};

parse_action("ZapHangup",Pdu) ->
    #req_zap_hangup{channel=get_pdu_var("Channel",Pdu)};

parse_action("ZapShowChannels",_Pdu) ->
    #req_zap_show_channels{};

parse_action("ZapTransfer",_Pdu) ->
    #req_zap_transfer{}.

%%--------------------------------------------------------------------
%% @spec parse(Data::string()) -> {[Pdu::pdu()],Cont::string()}
%% @doc Parse a string() containing 0 or more pdus.
%% @end
%%--------------------------------------------------------------------
parse(Data) ->
    {Pdus,Cont}=parse(Data,[]).

%%====================================================================
%% Internal functions
%%====================================================================
parse(Data,Acc) ->
    case split(Data) of
	{nothing,Rows} ->
	    {lists:reverse(Acc),Rows};
	{Msg,More} ->
	    parse(More,[Msg|More])
    end.

split(Data) ->
    case string:str(Data,"\r\n\r\n") of
	Pos when Pos>0 ->
	    Msg=string:substr(Data,1,Pos-1),
	    Msg1=string:tokens(Msg,"\r\n"),
	    Msg2=[split_line(L)|| L<- Msg1],
	    More=string:substr(Data,Pos+4),
	    {Msg2,More};
	_NotFound ->
	    {nothing,Data}
    end.
%%

split_line(L) ->
    Pos=string:str(L,": "),
    Lbl=string:substr(L,1,Pos-1),
    Value=string:substr(L,Pos+2),
    {Lbl,Value}.

get_pdu_var(Lbl,Pdu) ->
    case lists:keysearch(Lbl,1,Pdu) of
	{value,{Lbl,Val}} ->
	    Val;
	false ->
	    undefined
    end.

pdu_type(Pdu) ->
    pdu_type_action(Pdu,get_pdu_var("Action",Pdu)).

pdu_type_action(Pdu,undefined) ->
    pdu_type_event(Pdu,get_pdu_var("Event",Pdu));
pdu_type_action(Pdu,Action) ->
    {action,Action,Pdu}.

pdu_type_event(Pdu,undefined) ->
    pdu_type_alarm(Pdu,get_pdu_var("Alarm",Pdu));
pdu_type_event(Pdu,Event) ->
    {event,Event,Pdu}.

pdu_type_alarm(Pdu,undefined)->
    exit({unknown_pdu_type,Pdu});
pdu_type_alarm(Pdu,Alarm) ->
    {alarm,Alarm,Pdu}.

pack_pdu(Ls) ->
    [pack_l(L) || L <- Ls].

pack_l({L,V}) when is_integer(V) ->
    [list_to_binary(L),<<": ">>,
     list_to_binary(integer_to_list(V)),
     <<"\r\n">>];
pack_l({L,V}) when is_atom(V) ->
    [list_to_binary(L),<<": ">>,list_to_binary(atom_to_list(V)),<<"\r\n">>];
pack_l({L,V}) when is_list(V) ->
    [list_to_binary(L),<<": ">>,list_to_binary(V),<<"\r\n">>].



parse_event(Alarm,Pdu) ->
    ok.

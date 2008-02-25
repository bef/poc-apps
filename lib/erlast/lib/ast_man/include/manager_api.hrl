%% @type ami_cmd() = absolute_timeout()| agent_callback_login()| agent_logoff() | agents() | change_monitor() | command() | db_get() | db_put() | events() | extension_state() | get_var() | hangup() | iax_netstats() | iax_peers() | list_commands() | login() | logoff() | mailboxcount() | mailbox_status() | monitor() | originate()  | parked_calls() | ping() | queue_add() | queue_pause() | queue_remove() | queues() | queue_status() | redirect() | set_cdr_user_field() | set_var() | sip_showpeer() | sip_peers() | status() | stop_monitor() | zap_dial_offhook() | zap_dnd_off() | zap_dnd_on() | zap_hangup() | zap_show_channels() | zap_transfer()

%% @type absolute_timeout() = #absolute_timeout{channel=string(),timeout=integer()}.
-record(req_absolute_timeout,{channel,timeout}).

%% @type agent_callback_login() = #agent_callback_login{agent=string(),exten=string(),context=string(),ackCall=string(),wrapUpTime=string()}.
-record(req_agent_callback_login,{agent,exten,context,ackCall,wrapUpTime}).

%% @type agent_logoff() = #agent_logoff{agent=string(),soft=boolean()}.
-record(req_agent_logoff,{agent,soft}).

%% @type agents() = #agents{}.
-record(req_agents,{}).

%% @type challenge() = #challenge{authtype}.
-record(req_challenge,{authtype="MD5"}).

%% @type change_monitor() = #change_monitor{channel=string(),file=string()}.
-record(req_change_monitor,{channel,file}).

%% @type command() = #command{command=string()}.
-record(req_command,{command}).

%% @type db_del() = #db_del{family=string(),key=string()}.
-record(req_db_del,{family,key}).

%% @type db_get() = #db_get{family=string(),key=string()}.
-record(req_db_get,{family,key}).

%% @type db_put() = #db_put{family=string(),key=string(),value=string()}.
-record(req_db_put,{family,key,value}).

%% @type events() = #events{eventMask=string()}.
-record(req_events,{eventMask}).

%% @type extension_state() = #extension_state{extension=string(),context=string()}.
-record(req_extension_state,{extension,context}).

%% @type get_var() = #get_var{channel=string(),var=string()}.
-record(req_get_var,{channel,var}).

%% @type hangup() = #hangup{channel=string()}.
-record(req_hangup,{channel}).

%% @type iax_netstats() = #iax_netstats{}.
-record(req_iax_netstats,{}).

%% @type iax_peers() = #iax_peers{}.
-record(req_iax_peers,{}).

%% @type list_commands() = #list_commands{}.
-record(req_list_commands,{}).

%% @type login() = #login{user=string(),secret=string()}.
-record(req_login,{user,secret}).

%% @type logoff() = #logoff{}.
-record(req_logoff,{}).

%% @type mailbox_count() = #mailbox_count{mailbox=string(),new::integer(),old::integer()}.
-record(req_mailbox_count,{mailbox,new,old}).

%% @type mailbox_status() = #mailbox_status{mailbox=string(),waiting::integer()}.
-record(req_mailbox_status,{mailbox,waiting}).

%% @type monitor() = #monitor{channel=string(),file=string(),format=string(),mix=string()}.
-record(req_monitor,{channel,file,format,mix}).

%% @type originate() = #originate{channel=string()}.
-record(req_originate,{channel,dest,timeout,cid,vars=[],account,async}).

%% @type parked_calls() = #parked_calls{}.
-record(req_parked_calls,{}).

%% @type ping() = #ping{}.
-record(req_ping,{}).

%% @type queue_add() = #queue_add{interface=string(),queue=string(),penalty=string(),pause=string()}.
-record(req_queue_add,{interface,queue,penalty,pause}).

%% @type queue_pause() = #queue_pause{interface=string(),queue=string(),penalty=string(),pause=string()}.
-record(req_queue_pause,{interface,queue,penalty,pause}).

%% @type queue_remove() = #queue_remove{interface=string(),queue=string()}.
-record(req_queue_remove,{interface,queue}).

%% @type queues() = #queues{}.
-record(req_queues,{}).

%% @type queue_status() = #queue_status{queue=string(),member=string()}.
-record(req_queue_status,{queue,member}).

%% @type redirect() = #redirect{channel=string(),extrachannel=string(),exten=string(),context=string(),priority=string()}.
-record(req_redirect,{channel,extrachannel,exten,context,priority}).

%% @type set_cdr_user_field() = #set_cdr_user_field{channel=string(),userfield=string(),append=string()}.
-record(req_set_cdr_user_field,{channel,userfield,append}).

%% @type set_var() = #set_var{channel=string(),variable=string(),value=string()}.
-record(req_set_var,{channel,variable,value}).

%% @type sip_showpeer() = #sip_showpeer{peer=string()}.
-record(req_sip_showpeer,{peer}).

%% @type sip_peers() = #sip_peers{}.
-record(req_sip_peers,{}).

%% @type status() = #status{channel=string()}.
-record(req_status,{channel}).

%% @type stop_monitor() = #stop_monitor{channel=string()}.
-record(req_stop_monitor,{channel}).

%% @type zap_dial_offhook() = #zap_dial_offhook{channel=string(),number=string()}.
-record(req_zap_dial_offhook,{channel,number}).

%% @type zap_dnd_off() = #zap_dnd_off{channel=string()}.
-record(req_zap_dnd_off,{channel}).

%% @type zap_dnd_on() = #zap_dnd_on{channel=string()}.
-record(req_zap_dnd_on,{channel}).

%% @type zap_hangup() = #zap_hangup{channel=string()}.
-record(req_zap_hangup,{channel}).

%% @type zap_show_channels() = #zap_show_channels{}.
-record(req_zap_show_channels,{}).

%% @type zap_transfer() = #zap_transfer{}.
-record(req_zap_transfer,{}).

%% Responses

-record(resp_agents,{agent,name,status,channel,loggedtime,talkingto,aid,timestamp}).
-record(resp_agentscomplete,{aid,timestamp}).

%-record(resp_dbgetresponse,{,timestamp}).  %% TODO

%-record(resp_parkedcall,{,timestamp}).  %% TODO

%-record(resp_parkedcallscomplete,{,timestamp}).  %% TODO

-record(resp_peerentry,{aid, channeltype, objectname,chanobjecttype, 
			ip_address, ip_port, dynamic, nat, video, 
			acl, status, realtime, timestamp}).

-record(resp_peerlistcomplete,{aid, listitems, timestamp}).

-record(resp_queueentry,{aid, queue, position, channel, callerid, 
			 calleridname, wait, timestamp}).

-record(resp_queuemember,{queue, location, membership, penalty, callstaken, 
			 lastcall, status, paused, aid, timestamp}).

-record(resp_queueparams,{queue, max, calls, holdtime, completed, 
			  abandoned, sevicelevel, servicelevelperf,
			  weight, aid, timestamp}).

-record(resp_queuestatuscomplete,{aid, timestamp}).

-record(resp_statuscomplete,{aid,timestamp}).

-record(resp_zapshowchannelscomplete,{aid,timestamp}).

-record(resp_zapshowchannels,{channel, signalling, context, dnd, alarm, aid, 
			      timestamp}).

%% AMI Events
-record(evt_agentcallbacklogin,{agent,channel,logintime,timestamp}).

-record(evt_agentcallbacklogoff,{agent,reason,loginchan,logintime,timestamp}).

-record(evt_agentcalled,{agent,channel,callerid,calleridname,context,extension,priority,queuevars,timestamp}).

-record(evt_agentcomplete,{queue,uniqueid,channel,member,membername,holdtime,talktime,reason,queuevars,timestamp}).

-record(evt_agentconnect,{queue,uniqueid,channel,member,membername,holdtime,bridgedchannel,queuevars,timestamp}).

-record(evt_agentdump,{queue,uniqueid,channel,member,membername,queuevars,timestamp}).
-record(evt_agentlogin,{agent,channel,uniqueid,timestamp}).

-record(evt_agentlogoff,{agent,logintime,uniqueid,timestamp}).

-record(evt_alarm,{alarm,channel,timestamp}).

-record(evt_alarmclear,{channel,timestamp}).

-record(evt_cdr,{accountcode, source, destination, destinationcontext,
		 callerid, channel, destinationchannel, lastapplication,
		 lastdata, starttime, answertime, endtime, duration,
		 billableseconds, disposition, amaflags, uniqueid,
		 userfield,timestamp}).

-record(evt_channelreload,{channel, reaload_reason, registry_count,
			   peer_count, user_count, timestamp}).

%-record(evt_connect,{,timestamp}).  %% TODO

-record(evt_dial,{source, destination, callerid, calleridname, 
		  uniqueid1, uniqueid2, timestamp}).

%-record(evt_disconnect,{,timestamp}).  %% TODO

-record(evt_dndstate,{channel, state, timestamp}).

-record(evt_extensionstatus,{extension, context, status, timestamp}).

-record(evt_hangup,{channel,cause, cause_txt, uniqueid, timestamp}).

-record(evt_hold,{channel, uniqueid, timestamp}).

-record(evt_jabberevent,{account, packet, timestamp}).

-record(evt_join,{channel, callerid, calleridname, queue, position,
		  count, uniqueid, timestamp}).

-record(evt_leave,{channel, queue, count, uniqueid, timestamp}).

-record(evt_link,{channel1, channel2, callerid1, callerid2,
		  uniqueid1, uniqueid2, timestamp}).

-record(evt_logchannel,{channel, enabled, timestamp}).

-record(evt_meetmejoin,{channel, meetme, usernum, uniqueid, timestamp}).

-record(evt_meetmeleave,{channel, meetme, usernum, calleridnum, calleridname,
			 uniqueid, timestamp}).

-record(evt_meetmemute,{channel, meetme, usernum, status, uniqueid, timestamp}).

%-record(evt_meetmetalking,{channel, meetme, usernum, status, ,uniqueid, timestamp}).

-record(evt_messagewaiting,{mailbox, waiting, new, old, timestamp}).

-record(evt_newcallerid,{channel, callerid, calleridname, 
			 cid_callingpres, uniqueid, timestamp}).

-record(evt_newchannel,{channel, state, callerid, calleridname, 
			uniqueid, timestamp}).

-record(evt_newexten,{channel, context, extension, priority, 
		      application, data, uniqueid, timestamp}).

-record(evt_newstate,{channel, state, callerid, calleridname, uniqueid, timestamp}).

-record(evt_originateresponse,{channel, context, exten, reason, callerid, 
			       calleridnum, calleridname ,uniqueid, timestamp}). %% Check this.

-record(evt_parkedcallgiveup,{extension, channel, callerid, calleridname, 
			      timestamp}).

-record(evt_parkedcalltimeout,{extension, channel, callerid, calleridname, 
			       timestamp}).

-record(evt_peerstatus,{peer, status, cause, timestamp}).

-record(evt_queuecallerabandon,{queue, position, originalposition, 
				holdtime, uniqueid, timestamp}).

-record(evt_queuememberadded,{queue, location, membername, membership, 
			      penalty, callstaken, lastcall, status,
			      paused, timestamp}).

-record(evt_queuememberpaused,{queue, location, membername, paused, timestamp}).

-record(evt_queuememberremoved,{queue, location, membername, timestamp}).

-record(evt_queuememberstatus,{queue, location, membername, membership,
			       penalty, callstaken, lastcall, status,
			       paused, timestamp}).

-record(evt_registry,{channeldriver, domain, status, timestamp}).

-record(evt_reload,{message, timestamp}).

-record(evt_rename,{old, new, uniqueid, timestamp}).

-record(evt_shutdown,{shutdown, restart, timestamp}).

-record(evt_status_call,{channel, callerid, calleridname, account, state,
			 context, extension, priority, seconds, aid,
			 uniqueid, timestamp}).
-record(evt_status_conn,{channel, callerid, calleridname, account, state,
			 link, aid, uniqueid, timestamp}).

-record(evt_unhold,{channel, uniqueid, timestamp}).

-record(evt_unlink,{channel1, channel2, callerid1, callerid2,
		    uniqueid1, uniqueid2, timestamp}).

-record(evt_unparkedcall,{extension, channel, from, callerid, calleridname, 
			  timestamp}).

-record(evt_userevent,{parlist,timestamp}).

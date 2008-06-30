-module(erlami). %% client interface
-export([
	server/0, driver/0,
	start/0,
	connect/0, connect/2,
	start_connect/0, start_connect/2,
	action/1,
	pdu/1,
	parse/2
]).

-define(SERVER, erlami).
-define(DRIVER, erlami_drv).

%% @doc get the default server name
%% @spec server() -> atom()
server() -> ?SERVER.

%% @doc get the default driver name
%% @spec driver() -> atom()
driver() -> ?DRIVER.

%% @doc start_link a local erlami_server named {local, server()}
%% @spec start() -> term()
start() ->
	erlami_server:start_link({local, ?SERVER}).

%% @doc connect using the host/port provided by the application's configuration
%% @spec connect() -> ok
connect() ->
	{ok, Host} = application:get_env(erlami, host),
	{ok, Port} = application:get_env(erlami, port),
	connect(Host, Port).

%% @doc connect to host/port
%% @spec connect(Host::string(), Port::string()) -> ok
connect(Host, Port) ->
	erlami_server:connect(?SERVER, ?DRIVER, Host, Port).

%% @doc like start/0 and connect/0
%% @see start/0
%% @see connect/0
start_connect() ->
	start(),
	connect().

%% @doc like start/0 and connect/2
%% @see start/0
%% @see connect/2
start_connect(Host, Port) ->
	start(),
	connect(Host, Port).

%% @doc send PDU synchronously
%% @spec action(Pdu::tuple()) -> {Response::string(), tuplelist()}
action(Pdu) when is_list(Pdu) ->
	erlami_server:send_pdu(?SERVER, ?DRIVER, Pdu);
action(Pdu) when is_tuple(Pdu) ->
	action(pdu(Pdu)).

%% @doc assemble protocol description unit (PDU)
%% @spec pdu(tuple()) -> tuplelist()
pdu({'AbsoluteTimeout', Channel, Timeout}) ->
	[{"Action", "AbsoluteTimeout"},
	{"Channel", Channel},
	{"Timeout", Timeout}];

pdu({'ChangeMonitor', Channel, File}) ->
	[{"Action", "ChangeMonitor"},
	{"Channel", Channel},
	{"File", File}];

pdu({'Command', Command}) ->
	[{"Action", "Command"},
	{"Command", Command}];

%pdu({'ConferenceEnd'}) ->
%	[{"Action", "ConferenceEnd"}];

pdu({'ConferenceList'}) ->
	[{"Action", "ConferenceList"}];

%pdu({'DBGet'}) ->
%	[{"Action", "DBGet"}];

%pdu({'DBPut'}) ->
%	[{"Action", "DBPut"}];

pdu({'Events', EventMask}) ->
	[{"Action", "Events"},
	{"EventMask", EventMask}];

pdu({'ExtensionState', Exten, Context}) ->
	[{"Action", "ExtensionState"},
	{"Exten", Exten},
	{"Context", Context}];

pdu({'GetConfig', Filename}) ->
	[{"Action", "GetConfig"},
	{"Filename", Filename}];

pdu({'Getvar', Channel, Variable}) ->
	[{"Action", "Getvar"},
	{"Channel", Channel},
	{"Variable", Variable}];

pdu({'Getvar', Variable}) ->
	[{"Action", "Getvar"},
	{"Variable", Variable}];

pdu({'Hangup', Channel}) ->
	[{"Action", "Hangup"},
	{"Channel", Channel}];

pdu({'IAXnetstats'}) ->
	[{"Action", "IAXnetstats"}];

pdu({'IAXpeers'}) ->
	[{"Action", "IAXpeers"}];

pdu({'ListCommands'}) ->
	[{"Action", "ListCommands"}];

pdu({'Login', Username, Secret}) ->
	[{"Action", "Login"},
	{"Username", Username},
	{"Secret", Secret}];

pdu({'Logoff'}) ->
	[{"Action", "Logoff"}];

pdu({'MailboxCount', Mailbox}) ->
	[{"Action", "MailboxCount"},
	{"Mailbox", Mailbox}];

pdu({'MailboxStatus', Mailbox}) ->
	[{"Action", "MailboxStatus"},
	{"Mailbox", Mailbox}];

pdu({'Monitor', Channel}) ->
	[{"Action", "Monitor"},
	{"Channel", Channel}];

pdu({'Monitor', Channel, File, Format, Mix}) ->
	[{"Action", "Monitor"},
	{"Channel", Channel},
	{"File", File},
	{"Format", Format},
	{"Mix", Mix}];

pdu({'Originate', Channel, Exten, Context, Priority, Timeout, CallerID, Async}) ->
	[{"Action", "Originate"},
	{"Channel", Channel},
	{"Exten", Exten},
	{"Context", Context},
	{"Priority", Priority},
	{"Timeout", Timeout},
	{"CallerID", CallerID},
	{"Async", Async}];

pdu({'Originate', Channel, Application, Data, Timeout, CallerID, Async}) ->
	[{"Action", "Originate"},
	{"Channel", Channel},
	{"Application", Application},
	{"Data", Data},
	{"Timeout", Timeout},
	{"CallerID", CallerID},
	{"Async", Async}];

pdu({'Park', Channel, Channel2}) ->
	[{"Action", "Park"},
	{"Channel", Channel},
	{"Channel2", Channel2}];

pdu({'ParkedCalls'}) ->
	[{"Action", "ParkedCalls"}];

pdu({'PauseMonitor', Channel}) ->
	[{"Action", "PauseMonitor"},
	{"Channel", Channel}];

pdu({'Ping'}) ->
	[{"Action", "Ping"}];

pdu({'PlayDTMF', Channel, Digit}) ->
	[{"Action", "PlayDTMF"},
	{"Channel", Channel},
	{"Digit", Digit}];

pdu({'Redirect', Channel, Exten, Context, Priority}) ->
	[{"Action", "Redirect"},
	{"Channel", Channel},
	{"Exten", Exten},
	{"Context", Context},
	{"Priority", Priority}];

pdu({'Redirect', Channel, ExtraChannel, Exten, Context, Priority}) ->
	[{"Action", "Redirect"},
	{"Channel", Channel},
	{"ExtraChannel", ExtraChannel},
	{"Exten", Exten},
	{"Context", Context},
	{"Priority", Priority}];

%pdu({'SetCDRUserField'}) ->
%	[{"Action", "SetCDRUserField"}];

pdu({'Setvar', Channel, Variable, Value}) ->
	[{"Action", "Setvar"},
	{"Channel", Channel},
	{"Variable", Variable},
	{"Value", Value}];

pdu({'Setvar', Variable, Value}) ->
	[{"Action", "Setvar"},
	{"Variable", Variable},
	{"Value", Value}];

pdu({'SIPpeers'}) ->
	[{"Action", "SIPpeers"}];

pdu({'SIPshowpeer', Peer}) ->
	[{"Action", "SIPshowpeer"},
	{"Peer", Peer}];

pdu({'Status'}) ->
	[{"Action", "Status"}];

pdu({'StopMonitor', Channel}) ->
	[{"Action", "StopMonitor"},
	{"Channel", Channel}];

pdu({'UnpauseMonitor', Channel}) ->
	[{"Action", "UnpauseMonitor"},
	{"Channel", Channel}];

%pdu({'UpdateConfig', ...}) ->...

pdu({'UserEvent', UserEvent}) ->
	[{"Action", "UserEvent"},
	{"UserEvent", UserEvent}];

pdu({'UserEvent', UserEvent, Vars}) when is_list(Vars) ->
	[{"Action", "UserEvent"},
	{"UserEvent", UserEvent}] ++ Vars;

pdu({'WaitEvent', Timeout}) ->
	[{"Action", "WaitEvent"},
	{"Timeout", Timeout}];

pdu({'WaitEvent'}) ->
	[{"Action", "WaitEvent"}].

%% @doc extract specific keys from a PDU
%% @spec parse(Keys::list(), Pdu::tuplelist()) -> [string()|undefined]
parse(Keys, Pdu) ->
	[get_value(K, Pdu) || K <- Keys].

%% internal functions

get_value(Key, L) ->
	case lists:keysearch(Key, 1, L) of
		{value, {_, V}} -> V;
		_ -> undefined
	end.

-module(ast_ami_events).
-compile(export_all).

-define(SERVER,?MODULE).

start_link() ->
	gen_event:start_link({local, ?SERVER}).

event(Event) ->
    gen_event:notify(?SERVER, Event).

subscribe() ->
	subscribe(self()).

subscribe(Handler) ->
	gen_event:add_handler(?SERVER, Handler, []).
-module(eventserver).
-compile(export_all).

%% PUNCTUATION
%% scoping: receive, case requires end clauses
%% functions just need a . at the end of the defination
%% https://stackoverflow.com/questions/1110601/in-erlang-when-do-i-use-or-or
%% 'after delay' has a limit of about 49 days in erlang, many ways to work around it

-record(serverstate, {currentsubs, currentevents}).
-record(eventdesc, {name="", desc="", pid, timeout=10000}).
-record(eventstate, {serverpid, name="", delay=0}).

%% chain overloaded/guard functions with ; (so you go from most specific to the most general function
server_notify(Subs, Msg, MPid) when length(Subs) =:= 0 ->
	emptylist;	
server_notify(Subs, Msg, MPid) ->
	[ First | Remainder ] = Subs,
	First ! { Msg, MPid },
	io:format("~p~n", [First]),
	server_notify(Remainder, Msg, MPid)
.

%% http://erlang.org/doc/man/orddict.html
	
server_main(ServerState) ->
	%%ProcX = spawn(eventserver, timer, self()),
	%%ProcY = spawn(eventserver, timer, self()),
	%%ProcZ = spawn(eventserver, timer, self()),
	receive
		{Pid, subscribe} -> 
			Handle = erlang:monitor(process, Pid), 
			NewSubs = orddict:store(Handle, Pid, ServerState#serverstate.currentsubs),
			server_notify(ServerState, subscribe, Pid), 
			server_main(ServerState#serverstate{currentsubs=NewSubs});
		{Pid, terminate} -> exit(terminate);
		{Pid, add} -> 
			io:format("event added~n"), 
			EventPid = spawn(eventserver, eventloop, #eventstate{self(), name="asd", delay=3000}),
			NewEvents = orddict:store(Name, #event{name=Name, description=Description,pid=EventPid,timeout=TimeOut},S#state.events),
			Pid ! {MsgRef, ok},
			server_main(ServerState#serverstate{events=NewEvents}); %%spawn(eventserver, timer, self()), server_main(ServerState);
		{Pid, cancel} -> io:format("event canceled~n"), server_main(ServerState);
		{Pid, eventdone} -> eventdone;
		{'DOWN', Ref, process, _Pid, _Reason} ->
			loop(ServerState#serverstate{clients=orddict:erase(Ref, S#state.clients)});
		codechange -> 
			?MODULE:loop(ServerState);  %%The inclusion of ?MODULE: does an external call to the module code, and external calls are always done on the newest version
		_ -> io:format("unknown msg~n"), server_main(ServerState)
	end
.
	

	
eventloop(State) ->
	erlang:monitor(process, State#eventstate.serverpid),
	receive
		{Pid, cancel} -> io:format("event cancelled!~n"), erlang:demonitor(State#eventstate.serverpid, [flush]), cancelled;
		{'DOWN', Ref, process, Pid, _Reason} -> parentdead;
		_ -> unknown_msg
	after State#eventstate.delay -> State#eventstate.serverpid ! {self(), eventdone}
	end
.
	
	
client_startup(ServerPid) ->
	ServerPid ! {self(), subscribe},
	%%monitor(ServerPid, )
	receive
		_ -> io:format("unknown msg~n")
	end
.
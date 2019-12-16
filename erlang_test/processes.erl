-module(processes).
-compile(export_all).

%% http://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency

%% Spawning processes: spawn(fun() -> io:format("~p~n",[2 + 2]) end).
%% Console will show Pid! example: <0.80.0>

%% Spawning a number of processes [spawn(fun() -> G(X) end) || X <- lists:seq(1,10)].

%% self(). returns pid of the process executing the code

%% exit(self()). to stop the process 

%% self() ! msg. to send a message to a process's mailbox

%% flush(). in the shell to print all messages and clear them. use in shell only

main() ->

%% receive is the mailbox's recv function, it will block until it receives a msg. can also use guards.
	receive
		do_flip ->
			io:format("flip!~n");
		fish ->
			io:format("fish!~n");
		_ ->
			io:format("anything else!~n")
	end
.

%% to use in shell, do:
%%NewProc = spawn(modulename, modulefunc, [args]).
%%NewProc ! { self(), fish }. to send a message accompanying your pid (so you can do flush(). to see the replies)

%% recv2 replies to the sender, and restarts the loop
recv2() ->
	receive
		{ From, do_flip } ->
			From ! "flip!", recv2();
		{ From, fish }  ->
			From ! "fish!"; %% this will end because recv2 is not called
		_ ->
			io:format("anything else!~n"), recv2()
	end
.

%% function with state manipulation due to messages; we spawn a process running the function and push messages to it
fridge2(FoodList) ->
	receive
		{From, {store, Food}} ->
			From ! {self(), ok},
			fridge2([Food|FoodList]);
			{From, {take, Food}} ->
		case lists:member(Food, FoodList) of
			true -> 
				From ! {self(), {ok, Food}},
				fridge2(lists:delete(Food, FoodList));
			false -> 
				From ! {self(), not_found},
				fridge2(FoodList)
	end;
	terminate ->
	ok
	end.
	
%% example message abstraction functions; store and take are acting like commands; note you will be stuck at the receive cmd; use ctrl-G(^G) to get out
store(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	end.
 
%% what if you are expecting a message but never ever receive it? you can use 'after <milliseconds>'
%% receive
%% 	Match -> Expression1
%% after Delay ->
%% 	Expression2
%% end.
store2(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 -> 
		timeout
	end.

%% calling receive until there are no more messages
flush() ->
	receive
		_ -> flush()
	after 0 ->
		ok
	end.
%% http://learnyousomeerlang.com/more-on-multiprocessing
%% note that when messages do not fit a pattern in the receive clause, they will stay in the mailbox. You might will to store them in some logging utility

%% Links. Linking to a child process will cause the parent to die if the child dies
chain(0) ->
	receive
		_ -> ok
	after 2000 ->
		exit("chain dies here")
	end;

chain(N) ->
	Pid = spawn(fun() -> chain(N-1) end),
	link(Pid),
	io:format("here!~n"),
	receive %% blocks until the last one calls exit, then this guy also exits due to link(Pid)
		_ -> ok
	after 4000 ->
		io:format("lasted 4000!~n")
end.

%% what's happening if you called link(spawn(linkmon, chain, [3])). in the shell
%% [shell] == [3] == [2] == [1] == [0]
%% [shell] == [3] == [2] == [1] == *dead*
%% [shell] == [3] == [2] == *dead*
%% [shell] == [3] == *dead*
%% [shell] == *dead*
%% *dead, error message shown*
%% [shell] <-- restarted

%% process_flag(trap_exit, true). Do this to trap the exit calls and ignore the child process dying; seems to be a workaround for link.

%% Monitors. Supervisory connection to child processes. 
%% erlang:monitor(process, spawn(fun() -> timer:sleep(500), io:format("stop") end)).
%% you'll get messages, something like: 
%% Shell got {'DOWN',#Ref<0.0.0.77>,process,<0.63.0>,normal}

%% can also do spawn_monitor(fun() -> receive _ -> exit(boom) end end). which spawns and monitors the process
%% demonitor() to stop monitoring


%% Process restarter 
start_critic2() ->
	spawn(?MODULE, restarter, []).
 
restarter() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, chain, 0), % alternatively, you can use monitor
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, shutdown} -> % manual termination, not a crash
			ok;
		{'EXIT', Pid, _} ->
			restarter()
end.


%% register(chain, Pid), gives a name/alias to the process Pid
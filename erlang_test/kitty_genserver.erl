-module(kitty_genserver).
-behaviour(gen_server). %%'inherits' behaviour from erlang:gen_server. Some functions will be undefined if you compile it, so have to provide your own
%%http://erlang.org/doc/design_principles/gen_server_concepts.html
-compile(export_all).
%%-export([behaviour_info/1]).

-record(cat, {name, color=green, description}).

behaviour_info(callbacks) -> [{init,1}, {some_fun,0}, {other,3}];
behaviour_info(_) -> undefined.

%%%%%%%%%%%% Client functions/Public API %%%%%%%%%%%%%%%
start_link() -> gen_server:start_link(?MODULE, [], []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
	gen_server:call(Pid, {order, Name, Color, Description}).
 
%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
	gen_server:cast(Pid, {return, Cat}).
 
%% Synchronous call
close_shop(Pid) ->
	gen_server:call(Pid, terminate).
	
%%%%%%%%% Server functions; think of these as virtual functions from gen_server following a certain format, ie. handle_call(-matching parameters-, _From, Cats) %%%%%%
%% http://learnyousomeerlang.com/clients-and-servers
%% Relationship between gen_server and yourmodule
%% gen_server 		YourModule
%% start/3-4 		init/1
%% start_link/3-4 	init/1
%% call/2-3 		handle_call/3
%% cast/2 			handle_cast/2
%% OR https://erldocs.com/18.0/stdlib/gen_server.html?i=0&search=gen_server#undefined

init([]) -> {ok, []}. %% no treatment of info here!
 
handle_call({order, Name, Color, Description}, _From, Cats) ->
	if Cats =:= [] ->
		{reply, make_cat(Name, Color, Description), Cats};
	Cats =/= [] ->
		{reply, hd(Cats), tl(Cats)} %% hd is function for head of list, tl is function for tail of list(check docs)
	end;
	
handle_call(terminate, _From, Cats) ->
	{stop, normal, ok, Cats}.
 
handle_cast({return, Cat = #cat{}}, Cats) ->
	{noreply, [Cat|Cats]}.
	
handle_info(Msg, Cats) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Cats}.
	
terminate(normal, Cats) ->
	[io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
	ok.
	
code_change(_OldVsn, State, _Extra) ->
	%% No change planned. The function is there for the behaviour,
	%% but will not be used. Only a version on the next
	{ok, State}.
	
%%% Private functions
make_cat(Name, Col, Desc) ->
	#cat{name=Name, color=Col, description=Desc}.
-module(functions).
-compile(export_all).

%%http://learnyousomeerlang.com/syntax-in-functions

%% OR represented by ;
%% rightage(X) when X > 16; X < 104 ->  ...

%% AND represented by ,
rightage(X) when X > 16, X < 104 -> 
	true;
rightage(_) ->
	false.
	
%% conditionals, 'true -> ' is the 'else' clause
omg(N) ->
	if N =:= 2 -> might_succeed;
		true -> always_does
	end.
	
dosomething(B) ->
	if B =:= lol -> ididlol;
	   B =:= lala -> ididlala;
	   true -> ididnothing
	end.
	
help_me(Animal) ->
	Talk = if Animal == cat  -> "meow";
	Animal == beef -> "mooo";
	Animal == dog  -> "bark";
	Animal == tree -> "bark";
	true -> "fgdadfgna"
	end,
	{Animal, "says " ++ Talk ++ "!"}.
	
%% case conditional
beach(Temperature) ->
	case Temperature of
	{celsius, N} when N >= 20, N =< 45 -> 'favorable';
	{kelvin, N} when N >= 293, N =< 318 -> 'scientifically favorable';
	{fahrenheit, N} when N >= 68, N =< 113 -> 'favorable in the US';
	_ -> 'avoid beach'
	end.
	
%%type conversion-> erlang:list_to_float("54.32").

%%recursion, will complain about no clause when fac(0) is called
fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

%%anonymous functions
%%fun(Args1) -> Expression1, Expression2, .., ExpressionN;
%%   (Args2) -> Expression1, Expression2, .., ExpressionN;
%%   (Args3) -> Expression1, Expression2, .., ExpressionN;
%%end

%% named anonymous functions
%%fun MyFunc(Args1) -> Expression1, Expression2, .., ExpressionN;
%%   MyFunc(Args2) -> Expression1, Expression2, .., ExpressionN;
%%   MyFunc(Args3) -> Expression1, Expression2, .., ExpressionN end.
%%end

%% exceptions: erlang:error(Reason), generates a stack trace

%% main function,
%% compile with $erlc functions.erl
%% run with $erl -noshell -run functions main filename
%% only need the .beam file that is produced after compiling
main([FileName]) ->
	io:format("asdsad22 ~s ~n", [FileName]),
	erlang:halt(0).
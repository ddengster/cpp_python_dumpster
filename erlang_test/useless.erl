%%module name should be the same as the file name!
-module(useless).
-export([add/2,hello/1,greet_and_add_two/1]).
%%

%%reference: http://learnyousomeerlang.com/modules

add(A,B) ->
	A + B.
	
%% Comment!

hello(Name) ->
	io:format("Hello world!~nwaewe~n"),
	io:format("Hello again! ~s!~n", [Name]).
	
	
greet_and_add_two(X) ->
	hello("asd"),
	add(X, 2).
	
-define(mymacro(X,Y), X - Y).
%%Macro Usage: ?mymacro(33,11)


%%bash compilation: 
%% erlc flags file.erl
%%erl shell compile:
%% compile:file(FileName)
%%erl module complie: 
%% c()

%%shell cd: 
%% cd directory
%%directory has to be forward slashes and ending with a forward slash

%% compile to native code: hipe:c(Module,[native])

%%  useless:module_info() to check exports


-module(file_io).
%%-compile(export_all).
-export([main/1]).

%% main function,
%% compile with $erlc functions.erl
%% run with $erl -noshell -run functions main filename
%% only need the .beam file that is produced after compiling
main(FileName) ->
	io:format("reading ~s ~n", [FileName]),
	
	%%https://erldocs.com/18.0/kernel/file.html#open/2
	case file:open([FileName], read) of 
		{ error, Reason } -> io:format("cannot open, reason: ~s ~n", [Reason]), io:format("statement2~n"), io:format("statement3~n"), not_ok;
		{ ok, IoDevice } -> io:format("can open~n"), 
		%% read from IoDevice (there's a shortcut function called file:read_file, but we're going lower level
		case file:read(IoDevice, 9999) of
			{ ok, Data } -> io:format("read results: ~n~s~n", [Data]);
			{ error, Reason2 } -> io:format("cannot read, reason: ~s ~n", [Reason2])
		end,
		file:close(IoDevice)
	end
.

	%%end
	%%if 1 =:= 1 -> io:format("asdsd~n")
	%%end
	%%.
	
	%%erlang:halt(0). will close the shell!
	
	
	
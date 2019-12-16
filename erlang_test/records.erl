-module(records).
-compile(export_all).

%% records are kinda like structs -record(recordname, {named member, ...})
%% when in the shell, make sure to do $rr(records) for struct registration
-record(robot, {name, type=industrial, h, details=[]}).

-record(user, {id, name, group, age}).

main() ->
	#robot { name="asd", type=lala, details=["lalala"]},
	admin_panel(#user{name="ssss", group=admin}),
	repairman(#robot{name="zzz"})
	.


 
%% use pattern matching to filter
admin_panel(#user{name=Name, group=admin}) ->
	Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
	Name ++ " is not allowed".
 
%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
		%% Show stuff that can't be written in such a text
		allowed;
	adult_section(_) ->
		%% redirect to sesame street site
		forbidden.
		
%% updating records member values
repairman(Rob) ->
	Details = Rob#robot.details,
	NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
	{repaired, NewRob}.
		
%%header files have extensions named .hrl!
%% it usually contains all your structs
%% then you include it using -include("records.hrl").
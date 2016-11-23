-module(bi_room).

%% bi_room: bi_room library's entry point.

-export([init/0, add/1, del/1, send/2, send_msg/2]).

-record(user,{
	id,
	pidList
}).


%% API

init() ->
	ets:new(user, [set, public, named_table, {keypos, #user.id}, {write_concurrency,false}, {read_concurrency, false}]).
	

add(Pid) ->
	case ets:lookup(user, 1) of
		[#user{pidList = List}|_] ->
			ets:insert(user, #user{id = 1, pidList = [Pid|List]});
		[] -> 
			ets:insert(user, #user{id = 1, pidList = [Pid]})
	end.


del(Pid) ->
	case ets:lookup(user, 1) of
		[#user{pidList = List}|_] ->
			List1 = List -- [Pid],
			ets:insert(user, #user{id = 1, pidList = List1});
		[] -> 
			ets:insert(user, #user{id = 1, pidList = []})
	end.

send(Pid, Msg) ->
	case ets:lookup(user, 1) of
		[#user{pidList = List}|_] ->
			PidList = List -- [Pid],
			spawn(fun() -> send_msg(PidList, Msg) end);
		[] -> 
			ok
	end.

%% Internals

send_msg([Pid|List], Msg) ->
	Pid ! {chat, Msg},
	send_msg(List, Msg);
send_msg([], _Msg) -> ok.


%% End of Module.

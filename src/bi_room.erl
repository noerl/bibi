-module(bi_room).

%% bi_room: bi_room library's entry point.

-export([init/0, name/0, add/1, del/1, send/2, send_msg/2]).

-record(user,{
	id,
	data
}).


%% API

init() ->
	ets:new(user, [set, public, named_table, {keypos, #user.id}, {write_concurrency,false}, {read_concurrency, false}]),
	Data = bi_name:get(),
	ets:insert(user, #user{id = 2, data = Data}).
	


add(Pid) ->
	case ets:lookup(user, 1) of
		[#user{data = List}|_] ->
			ets:insert(user, #user{id = 1, data = [Pid|List]});
		[] -> 
			ets:insert(user, #user{id = 1, data = [Pid]})
	end.


name() ->
	[#user{data = {Len, NameList}}] = ets:lookup(user, 2),
	NewLen = 
		case Len of
			0 -> length(NameList);
			_ -> Len
		end,
	Index = crypto:rand_uniform(1, NewLen+1),
	Name = proplists:get_value(Index, NameList),
	case Index =/= NewLen of
		true -> 
			LastName = proplists:get_value(NewLen, NameList),
			NameList1 = lists:keystore(NewLen, 1, NameList, {NewLen, Name}),
			NameList2 = lists:keystore(Index, 1, NameList1, {Index, LastName}),
			ets:insert(user, #user{id = 2, data = {NewLen-1, NameList2}});
		false ->
			ets:insert(user, #user{id = 2, data = {NewLen-1, NameList}})
	end,
	Name.			


del(Pid) ->
	case ets:lookup(user, 1) of
		[#user{data = List}|_] ->
			List1 = List -- [Pid],
			ets:insert(user, #user{id = 1, data = List1});
		[] -> 
			ets:insert(user, #user{id = 1, data = []})
	end.

send(Pid, Msg) ->
	case ets:lookup(user, 1) of
		[#user{data = List}|_] ->
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

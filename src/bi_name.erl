-module(bi_name).

-export([load/0, update/0]).


load() ->
	Path = code:priv_dir(bibi),
	PathName = Path ++ "/name.txt",
	{ok, Binary} = file:read_file(PathName),
	% unicode:characters_to_binary(<<"，"/utf8>>),
	split(Binary, <<>>, 0, []).


split(<<239:8, 188:8, 140:8, Binary/binary>>, Name, Len, List) ->
	split(Binary, <<>>, Len+1, [{Len+1, Name}|List]);
split(<<44:8, Binary/binary>>, Name, Len, List) ->
	split(Binary, <<>>, Len+1, [{Len+1, Name}|List]);
split(<<Char:8, Binary/binary>>, Name, Len, List) ->
	split(Binary, <<Name/binary, Char:8>>, Len, List);
split(<<>>, Name, Len, List) ->
	{Len+1, [{Len+1, Name}|List]}.


update() ->
	{Cur, List} = bi_room:get_name(),
	Len = length(List),
	{Len1, List1} = load(),
	case Len1 =/= Len of
		true -> 
			{Use, Unuse} = split_use(Cur, List),
			{Add, _Last} = split_use(Len, List1),
			{Cur1, AddList1} = merge(Add, Cur, Unuse),
			{_Cur2, AddList2} = merge(Use, Cur1, AddList1),
			bi_room:update_name(Cur1, AddList2);
		false ->
			ok
	end.

split_use(Cur, List) ->
	split_use(Cur, List, [], []).

split_use(Cur, [{Index, Name}|List], Use, Unuse) ->
	case Cur < Index of
		true -> 
			split_use(Cur, List, [Name|Use], Unuse);
		false ->
			split_use(Cur, List, Use, [{Index, Name}|Unuse])
	end;
split_use(_Cur, [], Use, Unuse) -> {Use, Unuse}.


merge([Name|New], Cur, Old) ->
	NewCur = Cur+1,
	merge(New, NewCur, [{NewCur, Name}|Old]);
merge([], Cur, Old) -> 
	{Cur, Old}.

-module(bi_name).

-export([get/0]).


get() ->
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

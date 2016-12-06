-module(bi_server).

-include("bibi.hrl").

-export([start/0]).


start() ->
	Port = application:get_env(bibi, port, ?PORT),
	Connects = application:get_env(bibi, connects, ?CONNECTS),
	ApiList = application:get_env(bibi, api, []),
	Dispatch = cowboy_router:compile([{'_', ApiList}]),
	{ok, _} = cowboy:start_clear(http, Connects, [{port, Port}], #{
		env => #{dispatch => Dispatch}
	}).


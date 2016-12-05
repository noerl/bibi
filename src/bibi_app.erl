-module(bibi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, []) ->
	ok = application:start(crypto),
	ok = application:start(cowlib),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, bibi, "static"}},
			{"/[...]", cowboy_static, {priv_file, bibi, "index.html"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, 100, [{port, 80}], #{
		env => #{dispatch => Dispatch}
	}),
	bi_room:init(),
    bibi_sup:start_link().

stop(_State) ->
    ok.

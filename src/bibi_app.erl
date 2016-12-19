-module(bibi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, []) ->
	ok = inets:start(),
	ok = crypto:start(),
	ok = ssl:start(),
	ok = lager:start(),
	ok = application:start(cowlib),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	bi_server:start(),
	bi_room:init(),

    bibi_sup:start_link().

stop(_State) ->
    ok.

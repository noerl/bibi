-module(ws_handler).

-include("chat.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(user, {
	name
}).


init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init([]) ->
	Name = bi_room:name(),
	MsgList = [{<<"pt">>, ?PROTOCOL_LOGIN}, {<<"sid">>, 1}, {<<"rid">>, 2}, {<<"name">>, Name}],
	MsgJson = jsx:encode(MsgList),
	{reply, {text, MsgJson}, #user{name = Name}}.


websocket_handle({text, MsgBin}, State) ->
	MsgList = jsx:decode(MsgBin),
	Name = State#user.name,
	case proplists:get_value(<<"name">>, MsgList) of
		Name ->
			NewMsgList = [{<<"pt">>, ?PROTOCOL_MSG}, {<<"mid">>, 1}, {<<"sid">>, 1}, {<<"rid">>, 2}|MsgList],
			NewMsgBin = jsx:encode(NewMsgList),
			bi_room:send(self(), NewMsgBin);
		_ ->
			ok
	end,
	{ok, State};
websocket_handle(Data, State) ->
	io:format("[~p:~p] ~p~n", [?MODULE, ?LINE, Data]),
	{ok, State}.



websocket_info({chat, Msg}, State) ->
	{reply, {text, Msg}, State};
websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(5000, self(), <<"">>),
	{reply, {text, Msg}, State};
websocket_info(_Info, State) ->
	{ok, State}.

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
	MsgList = gen_server:call(bi_queue, history),
	InitList = [[{<<"pt">>, ?PROTOCOL_LOGIN}, {<<"sid">>, 1}, {<<"rid">>, 2}, {<<"name">>, Name}]|MsgList],
	InitData = jsx:encode(InitList),
	{reply, {text, InitData}, #user{name = Name}}.


websocket_handle({text, MsgBin}, State) ->
	MsgList = jsx:decode(MsgBin),
	Name = State#user.name,
	case proplists:get_value(<<"name">>, MsgList) of
		Name ->
			Time = os:system_time() div 1000000,
			CommonMsg = [{<<"mid">>, 1}, {<<"sid">>, 1}, {<<"rid">>, 2}, {<<"time">>, Time}|MsgList],
			RecvMsg = [{<<"pt">>, ?PROTOCOL_MSG}|CommonMsg],
			RecvMsgBin = jsx:encode([RecvMsg]),
			bi_room:send(self(), RecvMsgBin),
			gen_server:cast(bi_queue, {join, RecvMsg}),

			SendMsg = [[{<<"pt">>, ?PROTOCOL_ACK}|CommonMsg]],
			SendMsgBin = jsx:encode(SendMsg),
			{reply, {text, SendMsgBin}, State};
		_ ->
			{ok, State}
	end;
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

-module(ws_handler).

-include("chat.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).



init(Req, []) ->
	State = #{name => <<"123">>},
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
	MsgList = [{<<"pt">>, ?PROTOCOL_LOGIN}, {<<"sid">>, 1}, {<<"rid">>, 2}, {<<"name">>, <<"bin">>}],
	MsgJson = jsx:encode(MsgList),
	{reply, {text, MsgJson}, State}.


websocket_handle({text, MsgBin}, State) ->
	MsgList = jsx:decode(MsgBin),
	Msg = proplists:get_value(<<"msg">>, MsgList),
	Name = proplists:get_value(<<"name">>, MsgList),
	NewMsgList = [{<<"pt">>, ?PROTOCOL_MSG}, {<<"sid">>, 1}, {<<"rid">>, 2}, {<<"name">>, Name}, {<<"msg">>, Msg}],
	NewMsgBin = jsx:encode(NewMsgList),
	bi_room:send(self(), NewMsgBin),
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

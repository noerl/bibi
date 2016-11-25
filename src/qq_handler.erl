-module(qq_handler).

-export([init/2, loop/1]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	Req = login(Method, Req0),
	{ok, Req, Opts}.


login(<<"GET">>, Req) ->
	QRUrl = "https://ssl.ptlogin2.qq.com/ptqrshow?appid=501004106&e=0&l=M&s=5&d=72&v=4",
	case httpc:request(QRUrl) of
		{ok,{{"HTTP/1.1",200,"OK"}, ResponseHeader, Response}} ->
			Cookie = proplists:get_value("set-cookie", ResponseHeader),
			[QRSIG|_CookieList] = string:tokens(Cookie, ";"),
			spawn(?MODULE, loop, [QRSIG]),
			cowboy_req:reply(200, #{
				<<"content-type">> => <<"image/png; charset=utf-8">>
			}, Response, Req);
		_Error ->
			_Error
	end;
login(Method, Req) ->
	{Method, Req}.



% login_server() ->
% 	"https://ui.ptlogin2.qq.com/cgi-bin/login?daid=164&target=self&style=16&mibao_css=m_webqq&appid=501004106&enable_qlogin=0&no_verifyimg=1&s_url=http%3A%2F%2Fw.qq.com%2Fproxy.html&f_url=loginerroralert&strong_login=1&login_state=10&t=20131024001"


loop(Cookie) ->
	PtQRlogin = "https://ssl.ptlogin2.qq.com/ptqrlogin?webqq_type=10&remember_uin=1&login2qq=1&aid=501004106&u1=http%3A%2F%2Fw.qq.com%2Fproxy.html%3Flogin2qq%3D1%26webqq_type%3D10&ptredirect=0&ptlang=2052&daid=164&from_ui=1&pttype=1&dumy=&fp=loginerroralert&action=0-0-22363&mibao_css=m_webqq&t=undefined&g=1&js_type=0&js_ver=10185&login_sig=&pt_randsalt=0",
	case httpc:request(get, {PtQRlogin, [{"Cookie", Cookie}]}, [], []) of
		{ok,{{"HTTP/1.1",200,"OK"}, ResponseHeader, Response}} ->
			case string:substr(Response, 9, 1) of
				"6" -> 
					timer:sleep(2000),
					loop(Cookie);
				"0" -> 
					CookieList = proplists:get_all_values("set-cookie", ResponseHeader),
					PtwebCookie = lists:last(CookieList),
					[Ptweb|_] = string:tokens(PtwebCookie, ";"),
					SigUrl = lists:nth(4, string:tokens(Response, "','")),
					check_sig(SigUrl, Ptweb);
				ErrorCode ->
					io:format("ErrorCode:~p~n", [ErrorCode])
			end;
		_Error ->
			io:format("_Error:~p~n", [_Error]),
			_Error
	end.

check_sig(SigUrl, Ptweb)->
	case httpc:request(get, {SigUrl, [{"te", "gzip, deflate, sdch"}]}, [{autoredirect, false}], []) of
		{ok,{{"HTTP/1.1",302,"Found"}, ResponseHeader, _Response}} ->
			Cookie = create_cookie(Ptweb, ResponseHeader),
			getvf(Ptweb, Cookie);
		_Error ->
			io:format("_Error:~p~n", [_Error]),
			_Error
	end.


create_cookie(Ptweb, ResponseHeader) ->
	[Pt2gguinStr, UinStr, SkeyStr, PuinStr, PskeyStr, Pt4TokenStr|_] = 
		proplists:get_all_values("set-cookie", ResponseHeader),
	[Pt2gguin|_] = string:tokens(Pt2gguinStr, ";"),
	[Uin|_] = string:tokens(UinStr, ";"),
	[Skey|_] = string:tokens(SkeyStr, ";"),
	[Puin|_] = string:tokens(PuinStr, ";"),
	[Pskey|_] = string:tokens(PskeyStr, ";"),
	[Pt4Token|_] = string:tokens(Pt4TokenStr, ";"),
	NewCookieList = lists:join("; ", [Pt2gguin, Uin, Skey, Puin, Pskey, Pt4Token, Ptweb]),
	lists:concat(NewCookieList).



getvf(Ptweb, Cookie) ->
	GetvfUrl = "http://s.web2.qq.com/api/getvfwebqq?" ++ Ptweb ++ "&clientid=53999199&psessionid=&t=1481296800505",
	case httpc:request(get, {GetvfUrl, [{"cookie", Cookie}, {"te", "gzip, deflate, sdch"}, {"referer", "http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}]}, [], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			Result = proplists:get_value(<<"result">>, jsx:decode(Response)),
			Vfwebqq = proplists:get_value(<<"vfwebqq">>, Result),
			io:format("get list info Vfwebqq:~p~n", [Vfwebqq]),
			login2(Ptweb, Cookie);
		_Error ->
			io:format("_Error:~p~n", [_Error]),
			_Error
	end.


login2(Ptweb, Cookie) ->
	Login2Url = "http://d1.web2.qq.com/channel/login2",
	[_, PtwebValue] = string:tokens(Ptweb, "="),
	Body = "r={\"ptwebqq\":\"" ++ PtwebValue ++ "\",\"clientid\":53999199,\"psessionid\":\"\",\"status\":\"online\"}",
	case httpc:request(post, {Login2Url, [{"cookie", Cookie}, {"te", "gzip, deflate"}, {"referer","http://d1.web2.qq.com/proxy.html?v=20151105001&callback=1&id=2"}], "application/x-www-form-urlencoded", Body}, [], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			Result = proplists:get_value(<<"result">>, jsx:decode(Response)),
			Psessionid = proplists:get_value(<<"psessionid">>, Result),
			poll2(Ptweb, Cookie, Psessionid);
		_Error ->
			io:format("_Error:~p~n", [_Error]),
			_Error
	end.

poll2(Ptweb, Cookie, Psessionid) ->
	Login2Url = "https://d1.web2.qq.com/channel/poll2",
	[_, PtwebValue] = string:tokens(Ptweb, "="),
	BodyList = [{<<"ptwebqq">>, list_to_binary(PtwebValue)}, {<<"clientid">>, 53999199}, {<<"psessionid">>, Psessionid}, {<<"key">>, <<>>}],
	R = jsx:encode(BodyList),
	Body = <<<<"r=">>/binary, R/binary>>,
	case httpc:request(post, {Login2Url, [{"cookie", Cookie}, {"te", "gzip, deflate, br"}, {"referer","https://d1.web2.qq.com/cfproxy.html?v=20151105001&callback=1"}], "application/x-www-form-urlencoded", Body}, [], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			ResultList = proplists:get_value(<<"result">>, jsx:decode(Response)),
			case ResultList of
				[Result] -> 
					Value = proplists:get_value(<<"value">>, Result),
					io:format("Value:~p~n", [Value]),
					[_Font|Msg] = proplists:get_value(<<"content">>, Value),
					Fid = proplists:get_value(<<"from_uin">>, Value),
					Tid = proplists:get_value(<<"to_uin">>, Value),
					io:format("Fid:~p Tid:~p Msg:~p~n", [Fid, Tid, Msg]);
				_ ->
					io:format("ResultList:~p~n", [ResultList])
			end,
			poll2(Ptweb, Cookie, Psessionid);
		_Error ->
			io:format("_Error:~p~n", [_Error]),
			_Error
	end.
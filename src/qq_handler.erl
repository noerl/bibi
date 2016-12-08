-module(qq_handler).

-export([init/2, loop/1]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	Req = login(Method, Req0),
	{ok, Req, Opts}.


login(<<"GET">>, Req) ->
	httpc:set_options([{cookies, enabled}]),
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
					[Pt2gguinStr, UinStr, SkeyStr|CookieList] = proplists:get_all_values("set-cookie", ResponseHeader),
					[Pt2gguin|_] = string:tokens(Pt2gguinStr, ";"),
					[Uin|_] = string:tokens(UinStr, ";"),
					[Skey|_] = string:tokens(SkeyStr, ";"),
					PtwebCookie = lists:last(CookieList),
					[Ptweb|_] = string:tokens(PtwebCookie, ";"),
					SigUrl = lists:nth(4, string:tokens(Response, "','")),
					NewCookieList = lists:join("; ", [Pt2gguin,Uin,Skey,Ptweb]),
					NewCookieStr = lists:concat(NewCookieList),
					check_sig(SigUrl, NewCookieStr, Ptweb);
				ErrorCode ->
					io:format("ErrorCode:~p~n", [ErrorCode])
			end;
		_Error ->
			io:format("_Error:~p~n", [_Error]),
			_Error
	end.

check_sig(SigUrl, CookieStr, Ptweb)->
	URL = "http://ptlogin4.web2.qq.com/check_sig?pttype=1&uin=120148245&service=ptqrlogin&nodirect=0&ptsigx=28b3a36b8004cc892df2cf1ce8d35acb9b2d1ca892bd6722d8e285c29c7777d86d778a0dd6d9590f66a0cc48c573c3930dac07306e4a11fd27c6d00588a7057e&s_url=http%3A%2F%2Fw.qq.com%2Fproxy.html%3Flogin2qq%3D1%26webqq_type%3D10&f_url=&ptlang=2052&ptredirect=100&aid=501004106&daid=164&j_later=0&low_login_hour=0&regmaster=0&pt_login_type=3&pt_aid=0&pt_aaid=16&pt_light=0&pt_3rd_aid=0",
	io:format("SigUrl:~p CookieStr:~p, Ptweb:~p~n", [SigUrl, CookieStr, Ptweb]),
	case httpc:request(get, {SigUrl, [{"te", "gzip, deflate, sdch"}]}, [{autoredirect, false}], []) of
		{ok,{{"HTTP/1.1",200,"OK"}, ResponseHeader, Response}} ->
			io:format("ResponseHeader:~p~n", [ResponseHeader]),
			io:format("Response:~p~n", [Response]);
		_Error ->
			io:format("_Error:~p~n", [_Error]),
			_Error
	end.



login2(Ptweb) ->
	Login2Url = "http://d1.web2.qq.com/channel/login2",
	[_, PtwebValue] = string:tokens(Ptweb, "="),
	Body = "r:{\"ptwebqq\":\"" ++ PtwebValue ++ "\",\"clientid\":53999199,\"psessionid\":\"\",\"status\":\"online\"}",
	case httpc:request(post, {Login2Url, [{"Cookie", Ptweb}, {"Origin", "http://d1.web2.qq.com"}], "application/x-www-form-urlencoded", Body}, [], []) of
		{ok,{{"HTTP/1.1",200,"OK"}, ResponseHeader, Response}} ->
			io:format("ResponseHeader:~p~n", [ResponseHeader]),
			io:format("Response:~p~n", [Response]);
		_Error ->
			io:format("_Error:~p~n", [_Error]),
			_Error
	end.
	
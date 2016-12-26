-module(qq_handler).

-include("chat.hrl").

-export([init/2, loop/1]).

-compile([export_all]).

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
			lager:error("ptqrshow request error:~p", [_Error])
	end;
login(Method, Req) ->
	lager:error("ptqrshow method error:~p, ~p", [Method, Req]).



loop(Cookie) ->
	PtQRlogin = "https://ssl.ptlogin2.qq.com/ptqrlogin?webqq_type=10&remember_uin=1&login2qq=1&aid=501004106&u1=http%3A%2F%2Fw.qq.com%2Fproxy.html%3Flogin2qq%3D1%26webqq_type%3D10&ptredirect=0&ptlang=2052&daid=164&from_ui=1&pttype=1&dumy=&fp=loginerroralert&action=0-0-22363&mibao_css=m_webqq&t=undefined&g=1&js_type=0&js_ver=10185&login_sig=&pt_randsalt=0",
	case httpc:request(get, {PtQRlogin, [{"Cookie", Cookie}]}, [{timeout, 3000}], []) of
		{ok,{{"HTTP/1.1",200,"OK"}, ResponseHeader, Response}} ->
			case string:substr(Response, 9, 1) of
				"6" -> 
					timer:sleep(1000),
					loop(Cookie);
				"0" -> 
					CookieList = proplists:get_all_values("set-cookie", ResponseHeader),
					PtwebCookie = lists:last(CookieList),
					PtispStr = lists:nth(9, CookieList),
					RKStr = lists:nth(10, CookieList),
					[Ptisp|_] = string:tokens(PtispStr, ";"),
					[RK|_] = string:tokens(RKStr, ";"),
					[Ptweb|_] = string:tokens(PtwebCookie, ";"),
					SigUrl = lists:nth(4, string:tokens(Response, "','")),
					check_sig(SigUrl, Ptweb, Ptisp, RK);
				ErrorCode ->
					lager:error("ptqrlogin error code:~p", [ErrorCode])
			end;
		{error, timeout} ->
			loop(Cookie);
		_Error ->
			lager:error("ptqrlogin request error:~p", [_Error]),
			_Error
	end.

check_sig(SigUrl, Ptweb, Ptisp, RK) ->
	case httpc:request(get, {SigUrl, [{"te", "gzip, deflate, sdch"}]}, [{autoredirect, false},{timeout, 3000}], []) of
		{ok,{{"HTTP/1.1",302,"Found"}, ResponseHeader, _Response}} ->
			Cookie = create_cookie(Ptweb, ResponseHeader, Ptisp, RK),
			getvf(Ptweb, Cookie);
		{error, timeout} ->
			check_sig(SigUrl, Ptweb, Ptisp, RK);
		_Error ->
			lager:error("check_sig request error:~p", [_Error])
	end.


create_cookie(Ptweb, ResponseHeader, Ptisp, RK) ->
	[Pt2gguinStr, UinStr, SkeyStr, PuinStr, PskeyStr, Pt4TokenStr|_] = 
		proplists:get_all_values("set-cookie", ResponseHeader),
	[Pt2gguin|_] = string:tokens(Pt2gguinStr, ";"),
	[Uin|_] = string:tokens(UinStr, ";"),
	[Skey|_] = string:tokens(SkeyStr, ";"),
	[Puin|_] = string:tokens(PuinStr, ";"),
	[Pskey|_] = string:tokens(PskeyStr, ";"),
	[Pt4Token|_] = string:tokens(Pt4TokenStr, ";"),
	Rand1 = rand() div 10,
	PgvInfo = lists:concat(["pgv_info=ssid=s", Rand1]),
	Rand2 = rand(),
	PgvPvid = lists:concat(["pgv_pvid=", Rand2]),
	NewCookieList = join("; ", [PgvInfo, PgvPvid, Ptisp, RK, Pt2gguin, Uin, Skey, Puin, Pskey, Pt4Token, Ptweb]),
	lists:concat(NewCookieList).

join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].


getvf(Ptweb, Cookie) ->
	Time = unixtime(),
	GetvfUrl = lists:concat(["http://s.web2.qq.com/api/getvfwebqq?", Ptweb, "&clientid=53999199&psessionid=&t=", Time]),
	case httpc:request(get, {GetvfUrl, [{"cookie", Cookie}, {"te", "gzip, deflate, sdch"}, {"referer", "http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}]}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			Result = proplists:get_value(<<"result">>, jsx:decode(Response)),
			VfwebqqBin = proplists:get_value(<<"vfwebqq">>, Result),
			Vfwebqq = binary_to_list(VfwebqqBin),
			login2(Ptweb, Vfwebqq, Cookie);
		{error, timeout} ->
			getvf(Ptweb, Cookie);
		_Error ->
			lager:error("getvfwebqq request error:~p", [_Error])
	end.


login2(Ptweb, Vfwebqq, Cookie) ->
	Login2Url = "http://d1.web2.qq.com/channel/login2",
	[_, PtwebValue] = string:tokens(Ptweb, "="),
	Body = "r={\"ptwebqq\":\"" ++ PtwebValue ++ "\",\"clientid\":53999199,\"psessionid\":\"\",\"status\":\"online\"}",
	case httpc:request(post, {Login2Url, [{"cookie", Cookie}, {"te", "gzip, deflate"}, {"referer","http://d1.web2.qq.com/proxy.html?v=20151105001&callback=1&id=2"}], "application/x-www-form-urlencoded", Body}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			Result = proplists:get_value(<<"result">>, jsx:decode(Response)),
			Psessionid = proplists:get_value(<<"psessionid">>, Result),
			Uin = proplists:get_value(<<"uin">>, Result),
			
			Hash = hash2(Uin, PtwebValue),
			user_friend(Vfwebqq, Hash, Cookie),
			group_name_list(Vfwebqq, Hash, Cookie),
			discus_list(Psessionid, Vfwebqq, Cookie),
			_Face = self_info(Cookie),
			online_buddies(Psessionid, Vfwebqq, Cookie),
			% send_group_msg(1171821961, Face, Psessionid, Cookie),
			% send_group_msg(Face, Psessionid, Cookie),
			bi_room:add_user(Uin, {Ptweb, Vfwebqq, Cookie, Psessionid}),
			poll(Ptweb, Vfwebqq, Cookie, Psessionid);
		{error, timeout} ->
			login2(Ptweb, Vfwebqq, Cookie);
		_Error ->
			lager:error("login2 request error:~p", [_Error])
	end.

user_friend(Vfwebqq, Hash, Cookie) ->
	UserFriendUrl = "http://s.web2.qq.com/api/get_user_friends2",
	Body = "r={\"vfwebqq\":\"" ++ Vfwebqq ++ "\",\"hash\":\"" ++  Hash ++ "\"}",
	case httpc:request(post, {UserFriendUrl, [{"cookie", Cookie}, {"te", "gzip, deflate"}, {"referer","http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}], "application/x-www-form-urlencoded", Body}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			io:format("user_friend:~ts~n", [Response]);
		{error, timeout} ->
			user_friend(Vfwebqq, Hash, Cookie);
		_Error ->
			lager:error("get_user_friends2 request error:~p", [_Error])
	end.


group_name_list(Vfwebqq, Hash, Cookie) ->
	UserFriendUrl = "http://s.web2.qq.com/api/get_group_name_list_mask2",
	Body = "r={\"vfwebqq\":\"" ++ Vfwebqq ++ "\",\"hash\":\"" ++  Hash ++ "\"}",
	case httpc:request(post, {UserFriendUrl, [{"cookie", Cookie}, {"te", "gzip, deflate"}, {"referer","http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}], "application/x-www-form-urlencoded", Body}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			Result = proplists:get_value(<<"result">>, jsx:decode(Response)),
			Gnamelist = proplists:get_value(<<"gnamelist">>, Result),
			Fun = fun(NameList) ->
					{proplists:get_value(<<"gid"/utf8>>, NameList),
					 proplists:get_value(<<"name"/utf8>>, NameList),
					 proplists:get_value(<<"code"/utf8>>, NameList)}
			end,
			GroupList = lists:map(Fun, Gnamelist),
			put(group_list, GroupList);
		{error, timeout} ->
			group_name_list(Vfwebqq, Hash, Cookie);
		_Error ->
			lager:error("get_group_name_list_mask2 request error:~p", [_Error])
	end.


discus_list(PsessionidBin, Vfwebqq, Cookie) ->
	Psessionid = binary_to_list(PsessionidBin),
	Time = unixtime(),
	DiscusUrl = lists:concat(["http://s.web2.qq.com/api/get_discus_list?clientid=53999199&psessionid=", Psessionid, "&vfwebqq=", Vfwebqq, "&t=", Time]),
	case httpc:request(get, {DiscusUrl, [{"cookie", Cookie}, {"te", "gzip, deflate, sdch"}, {"referer", "http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}]}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			io:format("discus_list:~ts~n", [Response]);
		{error, timeout} ->
			discus_list(PsessionidBin, Vfwebqq, Cookie);
		_Error ->
			lager:error("get_discus_list request error:~p", [_Error])
	end.



self_info(Cookie) ->
	Time = unixtime(),
	SelfInfoUrl = lists:concat(["http://s.web2.qq.com/api/get_self_info2?t=", Time]),
	case httpc:request(get, {SelfInfoUrl, [{"cookie", Cookie}, {"te", "gzip, deflate, sdch"}, {"referer", "http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}]}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			io:format("self_info:~ts~n", [Response]),
			Result = proplists:get_value(<<"result">>, jsx:decode(Response)),
			proplists:get_value(<<"face">>, Result);
		{error, timeout} ->
			self_info(Cookie);
		_Error ->
			lager:error("get_self_info2 request error:~p", [_Error])
	end.


online_buddies(PsessionidBin, Vfwebqq, Cookie) ->
	Psessionid = binary_to_list(PsessionidBin),
	Time = unixtime(),
	DiscusUrl = lists:concat(["http://d1.web2.qq.com/channel/get_online_buddies2?vfwebqq=", Vfwebqq, "&clientid=53999199&psessionid=", Psessionid, "&t=", Time]),
	case httpc:request(get, {DiscusUrl, [{"cookie", Cookie}, {"te", "gzip, deflate, sdch"}, {"referer", "http://d1.web2.qq.com/proxy.html?v=20151105001&callback=1&id=2"}]}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			io:format("online_buddies:~ts~n", [Response]);
		{error, timeout} ->
			online_buddies(PsessionidBin, Vfwebqq, Cookie);
		_Error ->
			lager:error("get_online_buddies2 request error:~p", [_Error])
	end.

poll(Ptweb, Vfwebqq, Cookie, Psessionid) ->
	try 
		poll2(Ptweb, Vfwebqq, Cookie, Psessionid)
	catch
		Type:Reason ->
			lager:error("~p:~p", [Type, Reason]),
			poll(Ptweb, Vfwebqq, Cookie, Psessionid)
	end.


poll2(Ptweb, Vfwebqq, Cookie, Psessionid) ->
	Login2Url = "https://d1.web2.qq.com/channel/poll2",
	[_, PtwebValue] = string:tokens(Ptweb, "="),
	BodyList = [{<<"ptwebqq">>, list_to_binary(PtwebValue)}, {<<"clientid">>, 53999199}, {<<"psessionid">>, Psessionid}, {<<"key">>, <<>>}],
	R = jsx:encode(BodyList),
	Body = <<<<"r=">>/binary, R/binary>>,
	case httpc:request(post, {Login2Url, [{"cookie", Cookie}, {"te", "gzip, deflate, br"}, {"referer","https://d1.web2.qq.com/cfproxy.html?v=20151105001&callback=1"}], "application/x-www-form-urlencoded", Body}, [{timeout, 7000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			ResultList = proplists:get_value(<<"result">>, jsx:decode(Response)),
			case ResultList of
				[Result] -> 
					Value = proplists:get_value(<<"value">>, Result),
					Gid = proplists:get_value(<<"group_code">>, Value),
					[_Font|Msg] = proplists:get_value(<<"content">>, Value),
					Fid = proplists:get_value(<<"send_uin">>, Value),
					Time = proplists:get_value(<<"time">>, Value),
					Mid = proplists:get_value(<<"msg_id">>, Value),
					MsgBin = msg_to_bin(Msg, <<>>),
					Name = name(Gid, Vfwebqq, Cookie, Fid),
					msg(Name, Mid, Time, MsgBin),
					QQ = real_qq(Fid, Vfwebqq, Psessionid, Cookie),
					
					if  is_binary(Name) ->
							lager:info("~ts[~p]: ~ts", [Name, QQ, MsgBin]);
						true ->
							lager:info("~p[~p]: ~ts", [Name, QQ, MsgBin])
					end;
				_ ->
					ok
			end;
		{ok,{{"HTTP/1.1",504,"Gateway Time-out"}, _, _}} ->
			ok;
		{ok,{{"HTTP/1.1",502,"Bad Gateway"}, _, _}} ->
			ok;
		{error, timeout} ->
			ok;
		_Error ->
			lager:error("poll2 request error:~p", [_Error])
	end,
	poll2(Ptweb, Vfwebqq, Cookie, Psessionid).


msg_to_bin([[<<"face">>, FaceId]|List], MsgBin) ->
	MsgBin1 = <<MsgBin/binary, <<"[face:">>/binary, (integer_to_binary(FaceId))/binary, <<"]">>/binary>>,
	msg_to_bin(List, MsgBin1);
msg_to_bin([Bin|List], MsgBin) ->
	MsgBin1 = <<MsgBin/binary, Bin/binary>>,
	msg_to_bin(List, MsgBin1);
msg_to_bin([], MsgBin) -> MsgBin.


msg(Name, Mid, Time, Msg) ->
	MillTime = Time * 1000,
	RecvMsg = [{<<"pt">>, ?PROTOCOL_MSG_2}, {<<"mid">>, Mid}, {<<"sid">>, 1}, {<<"rid">>, 2}, {<<"time">>, MillTime}, {<<"name">>, Name}, {<<"type">>, 1}, {<<"msg">>, Msg}],
	RecvMsgBin = jsx:encode([RecvMsg]),
	bi_room:send(undefined, RecvMsgBin),
	gen_server:cast(bi_queue, {join, RecvMsg}).


name(Gid, VfwebQQ, Cookie, Id) ->
	GCode = id2code(Gid),
	case get(GCode) of
		CardList when is_list(CardList) ->
			proplists:get_value(Id, CardList, Id);
		_ ->
			case group_ext(GCode, VfwebQQ, Cookie) of
				[] -> Id;
				CardList1 -> 
					proplists:get_value(Id, CardList1, Id)
			end
	end.

id2code(Gid) ->
	GroupList =
		case get(group_list) of
			undefined -> [];
			List -> List
		end,
	case lists:keyfind(Gid, 1, GroupList) of
		{Gid, _Name, Code} -> Code;
		_ -> Gid
	end.


group_ext(GCode, VfwebQQ, Cookie) when is_number(GCode) ->
	Time = unixtime(),
	GroupExt = lists:concat(["http://s.web2.qq.com/api/get_group_info_ext2?gcode=", GCode, "&vfwebqq=", VfwebQQ, "&t=", Time]),
	RequestHead = [{"Content-Type","utf-8"}, {"cookie", Cookie}, {"te", "gzip, deflate, sdch"}, {"referer","http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}],
	case httpc:request(get, {GroupExt, RequestHead}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			Result = proplists:get_value(<<"result">>, jsx:decode(Response)),
			case Result of
				undefined -> 
					lager:warning("group_ext result undefined Response:~ts", [Response]),
					[];
				_ ->
					Cards = proplists:get_value(<<"minfo">>, Result),
					CardList = [{proplists:get_value(<<"uin">>, One), proplists:get_value(<<"nick">>, One)} || One <- Cards],
					put(GCode, CardList),
					CardList
			end;
		{error, timeout} ->
			[];
		_Error ->
			lager:error("get_group_info_ext2 request error:~p", [_Error]),
			[]
	end;
group_ext(GCode, _VfwebQQ, _Cookie) ->
	lager:error("group_ext gcode error:~p", [GCode]),
	[].

send_group_msg(Face, Psessionid, Cookie) ->
	GroupList =
		case get(group_list) of
			undefined -> [];
			List -> List
		end,
	Fun = fun({_Gid, _Name, Code}) ->
			send_group_msg(Code, Face, Psessionid, Cookie)
	end,
	lists:foreach(Fun, GroupList).



send_group_msg(Gid, Face, Psessionid, Cookie) ->
	GidBin = integer_to_binary(Gid),
	GroupMsgUrl = "https://d1.web2.qq.com/channel/send_qun_msg2",
	Mid = msg_id(),
	MidBin = integer_to_binary(Mid),
	FaceBin = integer_to_binary(Face),
	Body = <<<<"r={\"group_uin\":">>/binary, GidBin/binary, <<",\"content\":\"[\\\"">>/binary, <<"早上好，各位小伙伴！"/utf8>>/binary, <<"\\\",[\\\"font\\\",{\\\"name\\\":\\\"宋体\\\",\\\"size\\\":10,\\\"style\\\":[0,0,0],\\\"color\\\":\\\"000000\\\"}]]\",\"face\":"/utf8>>/binary, FaceBin/binary, <<",\"clientid\":53999199,\"msg_id\":"/utf8>>/binary, MidBin/binary,  <<",\"psessionid\":\"">>/binary, Psessionid/binary, <<"\"}">>/binary>>,
	case httpc:request(post, {GroupMsgUrl, [{"cookie", Cookie}, {"te", "gzip, deflate, br"}, {"referer","https://d1.web2.qq.com/cfproxy.html?v=20151105001&callback=1"}], "application/x-www-form-urlencoded", Body}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			io:format("send_group_msg:~ts~n", [Response]);
		{error, timeout} ->
			send_group_msg(Gid, Face, Psessionid, Cookie);
		_Error ->
			lager:error("send_qun_msg2 request error:~p", [_Error])
	end.


unixtime() ->
	{S1,S2,S3} = erlang:timestamp(),
	S1 * 1000000000 + S2 * 1000 + S3 div 1000.


ptb([H|List], 0, Ptb0, Ptb1, Ptb2, Ptb3) ->
	NewPtb0 = Ptb0 bxor H,
	ptb(List, 1, NewPtb0, Ptb1, Ptb2, Ptb3);
ptb([H|List], 1, Ptb0, Ptb1, Ptb2, Ptb3) ->
	NewPtb1 = Ptb1 bxor H,
	ptb(List, 2, Ptb0, NewPtb1, Ptb2, Ptb3);
ptb([H|List], 2, Ptb0, Ptb1, Ptb2, Ptb3) ->
	NewPtb2 = Ptb2 bxor H,
	ptb(List, 3, Ptb0, Ptb1, NewPtb2, Ptb3);
ptb([H|List], 3, Ptb0, Ptb1, Ptb2, Ptb3) ->
	NewPtb3 = Ptb3 bxor H,
	ptb(List, 0, Ptb0, Ptb1, Ptb2, NewPtb3);
ptb([], _, Ptb0, Ptb1, Ptb2, Ptb3) ->
	{Ptb0, Ptb1, Ptb2, Ptb3}.

hash2(Uin, Ptvfwebqq) ->
	{Ptb0, Ptb1, Ptb2, Ptb3} = ptb(Ptvfwebqq, 0, 0, 0, 0, 0),

	% var salt = ["EC", "OK"];
	Uin0 = ((Uin bsr 24) band 255) bxor 69,
	Uin1 = ((Uin bsr 16) band 255) bxor 67,
	Uin2 = ((Uin bsr 8) band 255) bxor 79,
	Uin3 = (Uin band 255) bxor 75,

    Result = [Ptb0, Uin0, Ptb1, Uin1, Ptb2, Uin2, Ptb3, Uin3],
    HexList = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"],
    Fun = fun(Byte) ->
    		Index1 = ((Byte bsr 4) band 15) + 1,
    		Index2 = (Byte band 15) + 1,
    		[lists:nth(Index1, HexList), lists:nth(Index2, HexList)]
    	end,
    BufList = lists:map(Fun, Result),
   	lists:flatten(BufList).


msg_id() ->
	Time = unixtime(),
	T1 = Time div 1000,
	T2 = (T1 rem 10000) * 10000,
	Seq1 = 
		case get(sequence) of
			undefined -> 0;
			Seq -> Seq
		end,
	NewSeq = Seq1 + 1,
	put(sequence, NewSeq),
	T2 + NewSeq.


rand() ->
	{_S1,_S2,S3} = erlang:timestamp(),
	S = S3 div 1000,
	(round((abs(rand:uniform() - 1)) * 2147483647) * S) rem (round(math:pow(10, 10))).



real_qq(Fid, Vfwebqq, Psessionid, Cookie) ->
	Key = lists:concat(["qq_uid_", Fid]),
	case get(Key) of
		undefined -> request_real_qq(Fid, Vfwebqq, Psessionid, Cookie);
		QQ -> QQ
	end.

request_real_qq(Fid, Vfwebqq, _Psessionid, Cookie) ->
	RealQQUrl = lists:concat(["http://s.web2.qq.com/api/get_friend_uin2?tuin=", Fid, "&type=1&vfwebqq=", Vfwebqq]),
	case httpc:request(get, {RealQQUrl, [{"cookie", Cookie}, {"te", "gzip, deflate, sdch"}, {"referer","http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}]}, [{timeout, 3000}], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			case proplists:get_value(<<"result">>, jsx:decode(Response)) of
				undefined -> Fid;
				Result ->
					% friend_info(Fid, Vfwebqq, Psessionid, Cookie),
					QQ = proplists:get_value(<<"account">>, Result),
					Key = lists:concat(["qq_uid_", Fid]),
					put(Key, QQ),
					QQ
			end;
		{error, timeout} ->
			request_real_qq(Fid, Vfwebqq, _Psessionid, Cookie);
		_Error ->
			lager:error("real_qq request error:~p", [_Error]),
			Fid
	end.
	


friend_info(Fid, Vfwebqq, PsessionidBin, Cookie) ->
	Psessionid = binary_to_list(PsessionidBin),
	FriendInfoUrl = lists:concat(["http://s.web2.qq.com/api/get_friend_info2?tuin=", Fid, "&vfwebqq=", Vfwebqq, "&clientid=53999199&psessionid=", Psessionid]),
	case httpc:request(get, {FriendInfoUrl, [{"cookie", Cookie}, {"te", "gzip, deflate, sdch"}, {"referer","http://s.web2.qq.com/proxy.html?v=20130916001&callback=1&id=1"}]}, [], [{body_format, binary}]) of
		{ok,{{"HTTP/1.1",200,"OK"}, _ResponseHeader, Response}} ->
			lager:info("friend_info:~ts", [Response]);
		_Error ->
			lager:error("friend_info request error:~p", [_Error]),
			Fid
	end.
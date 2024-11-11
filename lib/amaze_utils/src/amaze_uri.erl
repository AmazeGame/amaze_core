-module(amaze_uri).

-export([parse/1]).

parse(Url)->
	UrlInfo = uri_string:parse(Url),
	Port0 = maps:get(port,UrlInfo),
	Scheme = maps:get(scheme,UrlInfo),
	Port = scheme_port(Scheme,Port0),
	UrlInfo#{port=>Port}.

scheme_port(http,Port) when Port==undefined orelse Port==0->
	80;
scheme_port(https,Port)when Port==undefined orelse Port==0->
	443;
scheme_port(ws,Port) when Port==undefined orelse Port==0->
	80;
scheme_port(wss,Port)when Port==undefined orelse Port==0->
	443;
scheme_port(ftp,Port)when Port==undefined orelse Port==0->
	21;
scheme_port(sftp,Port)when Port==undefined orelse Port==0->
	22;
scheme_port(ftps,Port)when Port==undefined orelse Port==0->
	990;
scheme_port(smtp,Port)when Port==undefined orelse Port==0->
	25;
scheme_port(imap,Port)when Port==undefined orelse Port==0->
	143;
scheme_port(pop3,Port)when Port==undefined orelse Port==0->
	110;
scheme_port(rtmp,Port)when Port==undefined orelse Port==0->
	1935;
scheme_port(mysql,Port)when Port==undefined orelse Port==0->
	3306;
scheme_port(redis,Port)when Port==undefined orelse Port==0->
	6379;
scheme_port(pgsql,Port)when Port==undefined orelse Port==0->
	5432;
scheme_port(oracledb,Port)when Port==undefined orelse Port==0->
	1521;
scheme_port(_Scheme,Port)->
	Port.
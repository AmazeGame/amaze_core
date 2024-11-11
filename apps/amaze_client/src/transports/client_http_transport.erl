%%%-------------------------------------------------------------------
%%% @author 刘金鑫
%%% @copyright (C) 2024, QingTian
%%% @doc
%%%
%%% @end
%%% Created : 11. 11月 2024 20:32
%%%-------------------------------------------------------------------
-module(client_http_transport).
-author("刘金鑫").
-behavior(amaze_client_transport).
%% API
-export([init/1]).
init(#{url=Url, pack_frame=PackFrame})->
    UrlInfo = amaze_uri:parse(Url),
    Port = maps:get(port,UrlInfo),
    Scheme = maps:get(scheme,UrlInfo),
    Host = maps:get(host,UrlInfo),

    {ok, ConnPid} = case Scheme of
                         http->gun:open(Host, Port);
                        https-> gun:open(Host,Port,#{transport => tls})
                    end.
%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, adrianx@163.com
%%% @doc
%%%
%%% @end
%%% Created : 11. 11月 2024 20:32
%%%-------------------------------------------------------------------
-module(client_http_transport).
-author("adrianx@163.com").
-behavior(amaze_client_transport).
-include("amaze_client.hrl").
-include("amaze_global.hrl").
%% Callback API
-export([init/1,on_post_open/1,on_frame/3,next_frame/1]).
init(#{url:=Url,pack_frame:=PackFrame})->
    UrlInfo = amaze_uri:parse(Url),
    Port = maps:get(port,UrlInfo),
    Scheme = maps:get(scheme,UrlInfo),
    Host = maps:get(host,UrlInfo),
    case Scheme of
        http->
            {ok,ConnPid} = gun:open(Host, Port),
            {ok,#client_mock_context{pack_frame=PackFrame, protocol = http , connection_pid = ConnPid,connection_context = UrlInfo,transport_handle=?MODULE}};
        https->
            {ok,ConnPid} = gun:open(Host,Port,#{transport => tls}),
            {ok,#client_mock_context{pack_frame=PackFrame, protocol = https ,connection_pid = ConnPid,connection_context = UrlInfo,transport_handle=?MODULE}}
    end.

on_post_open(#client_mock_context{}=Context)->
    {ok,Context}.

headers()->
    [ {<<"accept">>, "application/json"}].

next_frame(#client_mock_context{pack_frame = PackFrame,connection_context=ConnCtx}=Context)->
    Path = case maps:get(path,ConnCtx,?UNDEFINED) of
               ?UNDEFINED-> "/";
               Path0 -> Path0
           end,
    Headers =headers(),
    gun:post(Context#client_mock_context.connection_pid,Path,Headers),

    {ok,Context}.
on_frame(Context,Body,Header)->
    {ok,Context}.

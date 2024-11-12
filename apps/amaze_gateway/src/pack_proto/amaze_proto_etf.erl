%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, adrianx@163.com
%%% @doc
%%%
%%% @end
%%% Created : 12. 11月 2024 12:01
%%%-------------------------------------------------------------------
-module(amaze_proto_etf).
-author("adrianx@163.com").
-behaviour(amaze_gateway_pack_proto).
%% API
-export([]).
name()->
    etf.

content_type()->
    "application/x-erlang-term".

router(MsgName)->
    {amaze_gateway_pack_serializer,MsgName}.

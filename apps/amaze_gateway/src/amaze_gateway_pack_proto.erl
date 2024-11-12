%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, adrianx@163.com
%%% @doc
%%%
%%% @end
%%% Created : 12. 11月 2024 11:55
%%%-------------------------------------------------------------------
-module(amaze_gateway_pack_proto).
-author("adrianx@163.com").
%% API
-callback name()->atom().
-callback content_type()-> string().
-callback router(MsgName::string()|binary()|integer()|atom())->{module(),atom()}.
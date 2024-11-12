%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, adrianx@163.com
%%% @doc
%%%
%%% @end
%%% Created : 11. 11月 2024 20:33
%%%-------------------------------------------------------------------
-module(amaze_client_transport).
-author("adrianx@163.com").
-include("amaze_client.hrl").
%% API

-callback init(Arg::term())-> {ok,Context::#client_mock_context{}}.
-callback post_open(Context::#client_mock_context{})-> {ok,Context::#client_mock_context{}}.
-callback next_frame(Context::#client_mock_context{})-> {ok,Context::#client_mock_context{}}.
-callback on_frame(Context::#client_mock_context{},Body::binary(),Header::term())->{ok,Context::#client_mock_context{}}.
-callback terminate(Context::#client_mock_context{})->ok.

%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, adrianx@163.com
%%% @doc
%%%
%%% @end
%%% Created : 10. 11月 2024 10:21
%%%-------------------------------------------------------------------
-module(au_ets).
-author("adrianx@163.com").

%% API
-export([new/2]).

new(Name, Options)->
	ets:new(Name, Options).

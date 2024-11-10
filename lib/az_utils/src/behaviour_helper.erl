%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 20:17
%%%-------------------------------------------------------------------
-module(behaviour_helper).

%% API
-export([check_behaviour/2]).

-spec check_behaviour(module(),module())-> boolean().
check_behaviour(Module,_Behaviour)->
	Module:module_info(),
	[].

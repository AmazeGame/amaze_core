%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 20:17
%%%-------------------------------------------------------------------
-module(amaze_behaviour).

%% API
-export([check_behaviour/2]).

-spec check_behaviour(module(),module())-> boolean().
check_behaviour(Module,TargetBehaviour)->
	MInfo = Module:module_info(attributes),
	Behaviours = proplists:get_value(behaviour,MInfo),
	lists:member(TargetBehaviour,Behaviours).

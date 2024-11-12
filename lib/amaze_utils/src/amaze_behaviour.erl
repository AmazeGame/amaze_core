%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, adrianx@163.com
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 20:17
%%%-------------------------------------------------------------------
-module(amaze_behaviour).

%% API
-export([check_behaviour/2,check_export/2]).

-spec check_behaviour(module(),module())-> boolean().
check_behaviour(Module,TargetBehaviour)->
	case check_export(TargetBehaviour,{behaviour_info,1}) of
		false-> false;
		true->
			Attributes = Module:module_info(attributes),
			Behaviours = proplists:get_value(behaviour,Attributes),
			Callbacks = TargetBehaviour:behaviour_info(callbacks),
			case lists:member(TargetBehaviour,Behaviours) of	%%检查是否包含exports的函数
				true->
					OptionCallbacks = TargetBehaviour:behaviour_info(optional_callbacks),
					CheckCallbacks = Callbacks -- OptionCallbacks,
					Exports = Module:module_info(exports),
					lists:all(
						fun(CheckExport)->
							lists:member(CheckExport,Exports)
						end,CheckCallbacks);
				false->
					false
			end
	end.

check_export(Module,Export)->
	Exports = Module:module_info(exports),
	lists:member(Export,Exports).

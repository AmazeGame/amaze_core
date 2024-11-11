%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 20:16
%%%-------------------------------------------------------------------
-module(amaze_gateway_transport).
-include("amaze_global.hrl").
%% API
-export([init/0,stop/0]).

-callback init_transport(Listeners::term())->ok.
-callback stop_transport(Listeners::term())->ok.

-spec init()-> ok.
init()->
	{ok,Transports} = application:get_env(transport),
	case Transports of
	[]-> ok;
	[_|_]->
		lists:foreach(
				fun(Transport)->
					case amaze_behaviour:check_behaviour(Transport,?MODULE) of
						true->
							case application:get_env(Transport) of
							{ok,ListenerInfos}->
								ok = Transport:init_transport(ListenerInfos);
							?UNDEFINED->
								ok
							end,
							ok;
						_->
							ok
					end end,Transports)
	end.


-spec stop()->ok.
stop()->
	Transports = application:get_env(transport),
	case Transports of
		[]-> ok;
		[_|_]->
			lists:foreach(
				fun(Transport)->
					case amaze_behaviour:check_behaviour(Transport,?MODULE) of
						true->
							case application:get_env(Transport) of
								{ok,ListenerInfos}->
									ok = Transport:stop_transport(ListenerInfos);
								?UNDEFINED->
									ok
							end,
							ok;
						_->
							ok
					end end,lists:reverse(Transports))
	end.

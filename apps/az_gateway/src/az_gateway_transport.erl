%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 20:16
%%%-------------------------------------------------------------------
-module(az_gateway_transport).
-include("az_global.hrl").
%% API
-export([init/0,stop/0]).

-callback init(Listeners::term())->ok.
-callback stop(Listeners::term())->ok.

-spec init()-> ok.
init()->
	{ok,Transports} = application:get_env(transport),
	case Transports of
		[]-> ok;
		[_|_]->
			lists:foreach(
				fun(Transport)->
					case behaviour_helper:check_behaviour(Transport,?MODULE) of
						true->
							case application:get_env(Transport) of
								{ok,Listeners}->
									ok = Transport:init(Listeners);
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
					case behaviour_helper:check_behaviour(Transport,?MODULE) of
						true->
							case application:get_env(Transport) of
								{ok,Listeners}->
									ok = Transport:stop(Listeners);
								?UNDEFINED->
									ok
							end,
							ok;
						_->
							ok
					end end,lists:reverse(Transports))
	end.

%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 20:56
%%%-------------------------------------------------------------------
-module(http_multi_transport).
-include("az_global.hrl").
-behaviour(az_gateway_transport).
%% API
-export([init/1]).
-export([stop/1]).



%%-export([init/2]).

-spec init([{atom(),term(),term()}])-> ok.
init(Listeners)->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", http_multi_transport, []}]}
	]),

	lists:foreach(
		fun({Listener,StartFunc,TransportOpts})->
			{ok, _} = cowboy:StartFunc(Listener,
				TransportOpts,
				#{env => #{dispatch => Dispatch}}
			) end,Listeners),
	ok.

-spec stop([{atom(),term(),term()}])-> ok.
stop(Listeners)->
	lists:foreach(
		fun({Listener,_,_})->
			cowboy:stop_listener(Listener)
		end,Listeners),
	ok.


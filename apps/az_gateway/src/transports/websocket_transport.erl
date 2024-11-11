%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 20:56
%%%-------------------------------------------------------------------
-module(websocket_transport).
-author("adrianx@163.com").
-include("az_global.hrl").
-behaviour(az_gateway_transport).
%% API
-export([init_transport/1]).
-export([stop_transport/1]).

-export([init/2]). %% for cowboy handle
-spec init_transport([{atom(),term(),term()}])-> ok.
init_transport(ListenerInfos)->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", ?MODULE, []}]}
	]),
	lists:foreach(
		fun
			({Listener,clear,TransportOpts,PackFrame})->
				{ok, _} =
					cowboy:start_clear(Listener,TransportOpts,
						#{
							env => #{dispatch => Dispatch,
								pack_frame=> PackFrame
							}}
					);
			({Listener,tls,TransportOpts,PackFrame})->
				{ok, _} =
					cowboy:start_tls(Listener,TransportOpts,
						#{
							env => #{dispatch => Dispatch,
								pack_frame=> PackFrame
							}}
					)
		end,ListenerInfos),
	ok.

-spec stop_transport([{atom(),term(),term()}])-> ok.
stop_transport(Listeners)->
	lists:foreach(
		fun({Listener,_,_})->
			cowboy:stop_listener(Listener)
		end,Listeners),
	ok.

-spec init(term(),term())-> {ok,term(),term()}.
init(Req0, Opts) ->
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, <<"Hello world!">>, Req0),
	{ok, Req, Opts}.

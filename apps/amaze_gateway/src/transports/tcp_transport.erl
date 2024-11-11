%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 20:56
%%%-------------------------------------------------------------------
-module(tcp_transport).
-author("adrianx@163.com").
-include("amaze_global.hrl").
-behaviour(amaze_gateway_transport).
%% API
-export([init_transport/1,stop_transport/1]).

-export([init/1,init/4,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).
-record(state,{socket,transport}).
-spec init_transport([{atom(),term(),term()}])-> ok.
init_transport(ListenerInfos)->
	lists:foreach(
		fun
			({Listener,clear,TransportOpts,PackFrame})->
				{ok, _} = ranch:start_listener(Listener,ranch_tcp,TransportOpts,?MODULE,[#{pack_frame=>PackFrame}]);
			({Listener,tls,TransportOpts,PackFrame})->
				{ok, _} = ranch:start_listener(Listener,ranch_ssl,TransportOpts,?MODULE,[#{pack_frame=>PackFrame}])
		end,ListenerInfos).

-spec stop_transport([{atom(),term(),term()}])-> ok.
stop_transport(Listeners)->
	lists:foreach(
		fun({Listener,_,_})->
			cowboy:stop_listener(Listener)
		end,Listeners),
	ok.

init(Args) ->
	io:format("init tcp transport:~p~n",[Args]),
	process_flag(trap_exit, true),
	{ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}, {packet, 0}]),
	gen_server:enter_loop(?MODULE, [], #state{socket = Socket, transport = Transport}, 1000).

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({tcp, Socket, Data}, State = #state{socket = Socket, transport = TransPort}) ->
	Response = Data,
	TransPort:setopts(Socket, [{active, once}]),
	TransPort:send(Socket, Response),
	{noreply, State, 0};
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, normal, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

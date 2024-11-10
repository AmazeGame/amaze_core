%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, adrianx@163.com
%%% @doc
%%%
%%% @end
%%% Created : 10. 11月 2024 22:14
%%%-------------------------------------------------------------------
-module(az_ets_server).
-author("adrianx@163.com").
-include("az_global.hrl").
-include("../lager/include/lager.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([i/0,i/1,create_ets/2,delete_ets/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(ALL_TABLE,'$all_table$').
-record(ets_infos,{table_id::reference(),table_name::atom(),options::term()}).

-record(az_ets_server_state, {owner_table::reference()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
-spec(i()->term()).
i()->
	gen_server:call(?MODULE,i).

-spec(i(Name::reference()|atom())->term()).
i(Name)->
	gen_server:call(?MODULE,{i,Name}).

-spec(create_ets(Name::reference()|atom(),Opts::term())->term()).
create_ets(Name,Opts)->
	gen_server:call(?MODULE,{create_ets,Name,Opts}).

-spec(delete_ets(Name::reference()|atom())->term()).
delete_ets(Name)->
	gen_server:call(?MODULE,{delete_ets,Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #az_ets_server_state{}} | {ok, State :: #az_ets_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	OwnerTabId = ets:new(?ALL_TABLE,[named_table,set,protected,{keypos,#ets_infos.table_id},{read_concurrency,true}]),
	{ok,EtsList} = application:get_env(az_ets_server),
	{ok,EtsDir} = application:get_env(ets_dir),
	lists:foreach(
		fun
			({Name,Opts, {config,File}})->
				TabId = ets:new(Name,Opts),
				ets:insert(OwnerTabId,#ets_infos{table_id = TabId,table_name = Name,options = Opts}),
				do_load(TabId,EtsDir,File);
			({Name,Opts})->
				TabId = ets:new(Name,Opts),
				ets:insert(OwnerTabId,#ets_infos{table_id = TabId,table_name = Name,options = Opts})
		end,EtsList),
	{ok, #az_ets_server_state{owner_table = OwnerTabId}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #az_ets_server_state{}) ->
	{reply, Reply :: term(), NewState :: #az_ets_server_state{}} |
	{reply, Reply :: term(), NewState :: #az_ets_server_state{}, timeout() | hibernate} |
	{noreply, NewState :: #az_ets_server_state{}} |
	{noreply, NewState :: #az_ets_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #az_ets_server_state{}} |
	{stop, Reason :: term(), NewState :: #az_ets_server_state{}}).
handle_call({i,Name}, _From, State =  #az_ets_server_state{owner_table = OwnerTabId}) when is_atom(Name) ->
	Reply =case ets:match_object(OwnerTabId,#ets_infos{table_name = Name,_='_'}) of
		       []-> {error,az_utils:sprintf("Can not find table:~s",[Name])};
		       [TabInfos]-> {ok,TabInfos};
		       TabInfos-> {ok,TabInfos}
	       end,
	{reply, Reply, State};
handle_call({i,TabRef}, _From, State = #az_ets_server_state{owner_table = OwnerTabId}) when is_reference(TabRef) ->
	Reply = case ets:lookup(OwnerTabId,TabRef) of
		        [TabInfo]->{ok,TabInfo};
		        []-> {error,az_utils:sprintf("Can not find table:~s",[TabRef])}
	        end,
	{reply, Reply, State};
handle_call(i, _From, State = #az_ets_server_state{owner_table = OwnerTabId}) ->
	Reply = case ets:tab2list(OwnerTabId) of
		        []-> {error,"Can not find any table"};
		        [TabInfo]-> {ok, TabInfo};
		        TabInfos-> {ok, TabInfos}
	        end,
	{reply, Reply, State};
handle_call({create_ets,Name,Opts}, _From, State = #az_ets_server_state{owner_table = OwnerTabId}) ->
	Reply = case catch ets:new(Name,Opts) of
		        {'EXIT',Reason}-> {error,Reason};
		        TabId->
			        Object = #ets_infos{table_id = TabId,table_name = Name,options = Opts},
			        ets:insert(OwnerTabId,Object),
			        {ok,Object}
	        end,
	{reply, Reply, State};
handle_call({delete_ets,Name}, _From, State = #az_ets_server_state{owner_table = OwnerTabId}) ->
	Reply = case catch ets:delete(Name) of
		        {'EXIT',Reason}->
			        {error,Reason};
		        true->
			        ets:delete(OwnerTabId,Name),
			        {ok,true}
	        end,
	{reply, Reply, State};
handle_call(_Request, _From, State = #az_ets_server_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #az_ets_server_state{}) ->
	{noreply, NewState :: #az_ets_server_state{}} |
	{noreply, NewState :: #az_ets_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #az_ets_server_state{}}).
handle_cast(_Request, State = #az_ets_server_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #az_ets_server_state{}) ->
	{noreply, NewState :: #az_ets_server_state{}} |
	{noreply, NewState :: #az_ets_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #az_ets_server_state{}}).
handle_info(_Info, State = #az_ets_server_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #az_ets_server_state{}) -> term()).
terminate(_Reason, _State = #az_ets_server_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #az_ets_server_state{},
	Extra :: term()) ->
	{ok, NewState :: #az_ets_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #az_ets_server_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_load(TabId,Dir,File)->
	EtsFile = filename:join(Dir,File),
	case file:consult(EtsFile) of
		{ok,Lines}->
			lists:foreach(
				fun(Line)->
					ets:insert(TabId,Line)
				end,Lines);
		{error,Reason} ->
			?lager_error("open ets file:~s error:~w",[EtsFile,Reason])
	end.
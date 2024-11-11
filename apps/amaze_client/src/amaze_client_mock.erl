%%%-------------------------------------------------------------------
%%% @author 刘金鑫
%%% @copyright (C) 2024, QingTian
%%% @doc
%%%
%%% @end
%%% Created : 11. 11月 2024 21:11
%%%-------------------------------------------------------------------
-module(amaze_client_mock).
-author("刘金鑫").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(amaze_client_mock_state, {con_context}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Arg::term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Arg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Arg], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #amaze_client_mock_state{}} | {ok, State :: #amaze_client_mock_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(Args) ->
    #{transport_handle:=TransportHandle}= Args,
    Context = TransportHandle:init(Args),
    {ok, #amaze_client_mock_state{con_context = Context}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #amaze_client_mock_state{}) ->
    {reply, Reply :: term(), NewState :: #amaze_client_mock_state{}} |
    {reply, Reply :: term(), NewState :: #amaze_client_mock_state{}, timeout() | hibernate} |
    {noreply, NewState :: #amaze_client_mock_state{}} |
    {noreply, NewState :: #amaze_client_mock_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #amaze_client_mock_state{}} |
    {stop, Reason :: term(), NewState :: #amaze_client_mock_state{}}).
handle_call(_Request, _From, State = #amaze_client_mock_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #amaze_client_mock_state{}) ->
    {noreply, NewState :: #amaze_client_mock_state{}} |
    {noreply, NewState :: #amaze_client_mock_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #amaze_client_mock_state{}}).
handle_cast(_Request, State = #amaze_client_mock_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #amaze_client_mock_state{}) ->
    {noreply, NewState :: #amaze_client_mock_state{}} |
    {noreply, NewState :: #amaze_client_mock_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #amaze_client_mock_state{}}).
handle_info(_Info, State = #amaze_client_mock_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #amaze_client_mock_state{}) -> term()).
terminate(_Reason, _State = #amaze_client_mock_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #amaze_client_mock_state{},
    Extra :: term()) ->
    {ok, NewState :: #amaze_client_mock_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #amaze_client_mock_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jän 2020 20:34
%%%-------------------------------------------------------------------
-module(evel_task).
-author("heyoka").

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).

%% gen_server callbacks
-export([init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
   task_id :: binary(),
   priority :: non_neg_integer(),
   leader :: pid(),
   certificate :: pid(),
   is_leader :: true|false
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(TaskId :: binary()) ->
   {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(TaskId) ->
   gen_server:start_link(?MODULE, [TaskId, 1], []).

start_link(TaskId, Priority) ->
   gen_server:start_link(?MODULE, [TaskId, Priority], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
   {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term()} | ignore).
init([TaskId, Priority]) ->
   erlang:send_after(5, self(), global_register),
   {ok, #state{task_id = TaskId, priority = Priority}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
   {reply, Reply :: term(), NewState :: #state{}} |
   {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
   {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_info(global_register, State=#state{task_id = TaskId, priority = Priority}) ->
   Self = self(),
   {Winner, Certificate} = evel:elect(TaskId, Self, [{priority, Priority},{link, false}]),
   logger:notice("[~p] election done for ~p, winner is: ~p (certificate: ~p)",[node(), TaskId, Winner, Certificate]),
   erlang:monitor(process, Certificate),
   NewState =
   case Winner of
      Self  -> logger:notice("[~p] I am the leader for task ~p!!",[node(), TaskId]),
               State#state{is_leader = true, certificate = Certificate, leader = Winner};
      _     ->
            State#state{is_leader = false, certificate = Certificate, leader = Winner}

   end,
   {noreply, NewState};
handle_info({'DOWN', _MonitorRef, process, Cert, _Info},
    #state{certificate = Cert, task_id = Task, is_leader = AmLeader} = State) ->
   case AmLeader of
      true -> logger:notice("[~p] I am not leader anymore for task ~p",[node(), Task]);
      false -> logger:warning("[~p] leader on other node surrendered for task(~p)",[node(), Task])
   end,
   erlang:send_after(0, self(), global_register),
   {noreply, State#state{leader = undefined, certificate = undefined}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
   ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
   {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

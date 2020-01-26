%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jän 2020 17:45
%%%-------------------------------------------------------------------
-module(leaders).
-author("heyoka").

%% API
-export([start_task/1, get_leader/1]).

start_task(TaskId) ->
   rpc:multicall(evel_task, start_link, [TaskId]).

get_leader(TaskId) ->
   Result = rpc:multicall(evel, find_leader, [TaskId]),
   {ResultList, _BadNodes} = Result,
   [{Winner, node(Winner)} || {ok, {Winner, _Cert}} <- ResultList].

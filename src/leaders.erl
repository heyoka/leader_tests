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
-export([start_task/1, get_leader/1, preflist/1, preflist/2]).

start_task(TaskId) ->
   rpc:multicall(evel_task, start_link, [TaskId]).

get_leader(TaskId) ->
   Result = rpc:multicall(evel, find_leader, [TaskId]),
   {ResultList, _BadNodes} = Result,
   [{Winner, node(Winner)} || {ok, {Winner, _Cert}} <- ResultList].

-spec preflist(term()) -> [{non_neg_integer(), atom()}].
preflist(Key) ->
   preflist(Key, length(nodes())+1).
preflist(Key, Num) ->
   Nodes = [node()|nodes()],
   HRNodes = hash_ring:list_to_nodes(Nodes),
   Ring = hash_ring:make(HRNodes),
   PrefNodes = hash_ring:collect_nodes(Key, Num, Ring),
   PrefList = [Node || {hash_ring_node, Node, Node, _Weight} <- PrefNodes],
   lists:zip(PrefList, lists:seq(length(PrefList), 1, -1)).




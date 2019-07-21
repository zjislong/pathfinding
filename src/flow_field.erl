%%% @author ZhengJia <zj952067409@163.com>
%%% @copyright 2018 ZhengJia
%%% @doc gen_server callback module implementation:
%%%
%%% @end
-module(flow_field).
-author('ZhengJia <zj952067409@163.com>').
%% API
-export([init/3, init_csv/3, write_csv/0, update/3, search/1]).

-record(node, {
               x = 0,
               y = 0,
               h = 0,
               s = true
              }).

pos2node({X, Y}) ->
    #node{x = X, y = Y}.

init(IndexList, MapMod, BarName) ->
    OpenGbSets = gb_sets:new(),
    ClosedDict = dict:new(),
    F = fun(EndIndex, OpenGbSets0) ->
                EndNode = pos2node(EndIndex),
                push_open_nodes(EndNode, OpenGbSets0)
        end,
    OpenGbSets1 = lists:foldl(F, OpenGbSets, IndexList),
    VisitedSets = sets:from_list(IndexList),
    do_init(OpenGbSets1, ClosedDict, VisitedSets, MapMod, BarName).

init_csv(End, MapMod, BarName) ->
    init(End, MapMod, BarName),
    write_csv().

write_csv() ->
    D = erlang:get(flow_field),
    file:delete("flow_field.csv"),
    [begin
         L = [begin
                  case dict:find({X, Y}, D) of
                      {ok, #node{h=H, s=true}} -> to_list(H);
                      {ok, #node{h=H, s=false}} -> "@"++to_list(H);
                      _ -> "255"
                  end
              end||X<-lists:seq(0,70)],
         S = string:join(L, ","),
         file:write_file("flow_field.csv", S++"\n", [append])
     end||Y<-lists:seq(0,60)].

to_list(H) ->
    H1 = trunc(H),
    H2 = trunc((H - H1)*100),
    integer_to_list(H1)++"."++integer_to_list(H2).

update([Index|_] = Indexs, H, State) ->
    ClosedDict = erlang:get(flow_field),
    case dict:find(Index, ClosedDict) of
        {ok, Node} ->
            case State of
                null ->
                    do_update(Indexs, Node, H, State, ClosedDict);
                _ ->
                    do_update(Indexs, Node, H, State, push_closed_node(Node#node{s=State}, ClosedDict))
            end;
        _ ->
            none
    end.

search([Index|Indexs]) ->
    ClosedDict = erlang:get(flow_field),
    case dict:find(Index, ClosedDict) of
        {ok, CurNode} ->
            case do_search(Indexs, CurNode, ClosedDict) of
                CurNode ->
                    none;
                NextNode ->
                    {NextNode#node.x * 60 + 30, NextNode#node.y * 60 + 30}
            end;
        _ ->
            none
    end.

do_init(OpenGbTree, ClosedDict, VisitedSets, MapMod, BarName) ->
    case dequeue_cheapest_node(OpenGbTree) of
        none -> %% no other node
            erlang:put(flow_field, ClosedDict);
        {ok, OpenGbTree0, CurrentNode} ->
            %% move currentNode from open to closed
            ClosedDict0 = push_closed_node(CurrentNode, ClosedDict),
            %% Find all neighbors for the current node.
            %% NeighborsNodes = neighbors_nodes(diagonal, CurrentNode),
            X = CurrentNode#node.x,
            Y = CurrentNode#node.y,
            NeighborsNodes = [{X+AddX, Y+AddY}||AddX<-[0,-1,1], AddY<-[0,-1,1], X+AddX>=0, Y+AddY>=0],
            %% push neighbors if node validity
            {OpenGbTree1, ClosedDict1, VisitedSets0} = push_neighbors(NeighborsNodes, CurrentNode, OpenGbTree0, ClosedDict0, VisitedSets, MapMod, BarName),
            %% tail recursion
            do_init(OpenGbTree1, ClosedDict1, VisitedSets0, MapMod, BarName)
    end.

%% 获取相邻的node
push_neighbors([], _CurrentNode, OpenGbSets, ClosedDict, VisitedSets, _MapMod, _BarName) ->
    {OpenGbSets, ClosedDict, VisitedSets};
push_neighbors([{X, Y}|T], CurrentNode, OpenGbSets, ClosedDict, VisitedSets, MapMod, BarName) ->
    case is_closed(X, Y, ClosedDict) of
        true ->
            push_neighbors(T, CurrentNode, OpenGbSets, ClosedDict, VisitedSets, MapMod, BarName);
        false ->
            %% 当前点到重点的H值
            H = h({X, Y}, {CurrentNode#node.x, CurrentNode#node.y}) + CurrentNode#node.h,
            NeighborsNode0 = #node{x = X, y = Y, h = H},
            case sets:is_element({X, Y}, VisitedSets) of
                true -> %% 已经寻到过的点，更新open列表node
                    OpenGbSets0 = rescore_open_node(NeighborsNode0, OpenGbSets),
                    push_neighbors(T, CurrentNode, OpenGbSets0, ClosedDict, VisitedSets, MapMod, BarName);
                false ->
                    case is_walk_grid(MapMod, BarName, {X, Y}) of
                        true ->
                            OpenGbSets0 = push_open_nodes(NeighborsNode0, OpenGbSets),
                            VisitedSets0 = sets:add_element({X, Y}, VisitedSets),
                            push_neighbors(T, CurrentNode, OpenGbSets0, ClosedDict, VisitedSets0, MapMod, BarName);
                        false ->
                            NeighborsNode1 = NeighborsNode0#node{h = 255, s = false},
                            ClosedDict0 = push_closed_node(NeighborsNode1, ClosedDict),
                            push_neighbors(T, CurrentNode, OpenGbSets, ClosedDict0, VisitedSets, MapMod, BarName)
                    end
            end
    end.

%% 选择代价最小的点
dequeue_cheapest_node(OpenGbSets) ->
    case gb_sets:is_empty(OpenGbSets) of
        true ->
            none;
        false ->
            {{_Score, Node}, OpenGbSets0} = gb_sets:take_smallest(OpenGbSets),
            {ok, OpenGbSets0, Node}
    end.

is_closed(X, Y, ClosedDict) ->
    dict:is_key({X, Y}, ClosedDict).

%% 更新已经在open列表的节点信息
%% f值越小则替换
rescore_open_node(Node = #node{x = X, y = Y, h = H}, OpenGbSets) ->
    case search_open_node(X, Y, OpenGbSets) of
        {ok, Key = {_Score, #node{h = H1}}} ->
            case H < H1 of
                true -> %% 代价更小
                    OpenGbSets0 = gb_sets:delete(Key, OpenGbSets),
                    push_open_nodes(Node, OpenGbSets0);
                false ->
                    OpenGbSets
            end;
        _ ->
            push_open_nodes(Node, OpenGbSets)
    end.

search_open_node(X, Y, OpenGbTree) ->
    Iter = gb_sets:iterator(OpenGbTree),
    do_search_open_node(gb_sets:next(Iter), X, Y).
do_search_open_node(none, _X, _Y) ->
    none;
do_search_open_node({Key = {_Score, #node{x = X, y = Y}}, _Iter}, X, Y) ->
    {ok, Key};
do_search_open_node({_Key, Iter}, X, Y) ->
    do_search_open_node(gb_sets:next(Iter), X, Y).

push_open_nodes(#node{h = H} = Node, OpenGbSets) ->
    gb_sets:insert({H, Node}, OpenGbSets).

do_update([], _, _, _, ClosedDict) ->
    erlang:put(flow_field, ClosedDict);
do_update([CurIndex|Indexs], Node, H, State, ClosedDict) ->
    case dict:find(CurIndex, ClosedDict) of
        {ok, #node{h=H1} = CurNode}->
            CurNode1 = CurNode#node{h=H1+H/(1+h(CurNode, Node))},
% lager:info("格子~p电势改变~p=>~p", [CurIndex, H1, CurNode1#node.h]),
            do_update(Indexs, Node, H, State, push_closed_node(CurNode1, ClosedDict));
        _ ->
            do_update(Indexs, Node, H, State, ClosedDict)
    end.

do_search([], Node, _) ->
    Node;
do_search([CurIndex|Indexs], #node{h=H1}=Node, ClosedDict) ->
    case dict:find(CurIndex, ClosedDict) of
        {ok, #node{h=H2, s=true}=CurNode} when H2 < H1->
            do_search(Indexs, CurNode, ClosedDict);
        _ ->
            do_search(Indexs, Node, ClosedDict)
    end.

%% @doc 计算该点到终点的距离加权值
%% 启发函数: 曼哈顿距离
%% h(manhattan, #node{x = X1, y = Y1}, #node{x = X2, y = Y2}) ->
%%     erlang:abs(X2 - X1) + erlang:abs(Y2 - Y1);
%% 启发函数: 斜对角线
h(#node{x = X1, y = Y1}, #node{x = X2, y = Y2}) ->
    h({X1, Y1}, {X2, Y2});
h({X1, Y1}, {X2, Y2}) ->
    D1 = erlang:abs(X2 - X1),
    D2 = erlang:abs(Y2 - Y1),
    (D1 + D2) + (-0.5857864376269049 * erlang:min(D1, D2)).

push_closed_node(#node{x = X, y = Y} = Node, ClosedDict) ->
    dict:store({X, Y}, Node, ClosedDict).

is_walk_grid(_, [], _) -> true;
is_walk_grid(MapMod, [BarName|BarNames], Pos) ->
    case MapMod:is(BarName, Pos) of
        true ->
            is_walk_grid(MapMod, BarNames, Pos);
        false ->
            false
    end.
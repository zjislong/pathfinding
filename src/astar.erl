%%% @author ZhengJia <zj952067409@163.com>
%%% @copyright 2018 ZhengJia
%%% @doc gen_server callback module implementation:
%%%
%%% @end
-module(astar).
-author('ZhengJia <zj952067409@163.com>').
%% API
-export([search/5]).

-record(node, {
               x = 0,
               y = 0,
               h = 0,
               g = 0,
               parent = undefined
              }).

pos2node({X, Y}) ->
    #node{x = X, y = Y}.

search(Start, End, MapMod, JpsMod, BarName) ->
    OpenGbSets = gb_sets:new(),
    ClosedDict = dict:new(),
    VisitedSets = sets:new(),
    StartNode = pos2node(Start),
    EndNode = pos2node(End),
    OpenGbSets0 = push_open_nodes(StartNode#node{g = 0, h = h(diagonal, StartNode, EndNode)}, OpenGbSets),
    do_search(EndNode, OpenGbSets0, ClosedDict, VisitedSets, MapMod, JpsMod, BarName).

do_search(EndNode, OpenGbTree, ClosedDict, VisitedSets, MapMod, JpsMod, BarName) ->
    case dequeue_cheapest_node(OpenGbTree) of
        none -> %% no other node
            none;
        {ok, OpenGbTree0, CurrentNode} ->
            case eq(CurrentNode, EndNode) of
                true -> %% Find it!
                    make_path(CurrentNode, ClosedDict);
                false ->
                    %% move currentNode from open to closed
                    ClosedDict0 = push_closed_node(CurrentNode, ClosedDict),
                    %% Find all neighbors for the current node.
                    %% NeighborsNodes = neighbors_nodes(diagonal, CurrentNode),
                    NeighborsNodes = jps:search(CurrentNode, EndNode, JpsMod),
                    %% push neighbors if node validity
                    {OpenGbTree1, ClosedDict1, VisitedSets0} = push_neighbors(NeighborsNodes, EndNode, CurrentNode, diagonal, OpenGbTree0, ClosedDict0, VisitedSets, MapMod, BarName),
                    %% tail recursion
                    do_search(EndNode, OpenGbTree1, ClosedDict1, VisitedSets0, MapMod, JpsMod, BarName)
            end
    end.

%% 获取相邻的node
push_neighbors([], _EndNode, _CurrentNode, _HeuristicsType, OpenGbSets, ClosedDict, VisitedSets, _MapMod, _BarName) ->
    {OpenGbSets, ClosedDict, VisitedSets};
push_neighbors([NeighborsNode = #node{x = X, y = Y}|T], EndNode, CurrentNode, HeuristicsType, OpenGbSets, ClosedDict, VisitedSets, MapMod, BarName) ->
    case is_walk_grid(MapMod, BarName, {X, Y}) andalso not is_closed(X, Y, ClosedDict) of
        true -> %% 可行走点，没有在closed列表
            %% 计算子节点G值 = 父节点G值 + G值开销
            G = CurrentNode#node.g + g(NeighborsNode, CurrentNode),
            %% 当前点到重点的H值
            H = h(HeuristicsType, NeighborsNode, EndNode),
            NeighborsNode0 = NeighborsNode#node{g = G, h = H, parent = {CurrentNode#node.x, CurrentNode#node.y}},
            case sets:is_element({X, Y}, VisitedSets) of
                true -> %% 已经寻到过的点，更新open列表node
                    OpenGbSets0 = rescore_open_node(NeighborsNode0, OpenGbSets),
                    push_neighbors(T, EndNode, CurrentNode, HeuristicsType, OpenGbSets0, ClosedDict, VisitedSets, MapMod, BarName);
                false ->
                    OpenGbSets0 = push_open_nodes(NeighborsNode0, OpenGbSets),
                    VisitedSets0 = sets:add_element({X, Y}, VisitedSets),
                    push_neighbors(T, EndNode, CurrentNode, HeuristicsType, OpenGbSets0, ClosedDict, VisitedSets0, MapMod, BarName)
            end;
        false ->
            push_neighbors(T, EndNode, CurrentNode, HeuristicsType, OpenGbSets, ClosedDict, VisitedSets, MapMod, BarName)
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

%% 生成路径
make_path(Node, ClosedDict) ->
    make_path(Node, ClosedDict, []).
make_path(#node{x = X, y = Y, parent = Parent}, ClosedDict, Path) ->
    case dict:find(Parent, ClosedDict) of
        'error' ->
            [{X, Y}|Path];
        {ok, ParentNode} ->
            make_path(ParentNode, ClosedDict, [{X, Y}|Path])
    end.

%% @doc 计算该点到终点的距离加权值
%% 启发函数: 曼哈顿距离
%% h(manhattan, #node{x = X1, y = Y1}, #node{x = X2, y = Y2}) ->
%%     erlang:abs(X2 - X1) + erlang:abs(Y2 - Y1);
%% 启发函数: 斜对角线
h(diagonal, #node{x = X1, y = Y1}, #node{x = X2, y = Y2}) ->
    D1 = erlang:abs(X2 - X1),
    D2 = erlang:abs(Y2 - Y1),
    (D1 + D2) + (-0.5857864376269049 * erlang:min(D1, D2)).

%% 更新已经在open列表的节点信息
%% f值越小则替换
rescore_open_node(Node = #node{x = X, y = Y, h = H, g = G}, OpenGbSets) ->
    case search_open_node(X, Y, OpenGbSets) of
        {ok, Key = {_Score, #node{h = H1, g = G1}}} ->
            case H + G < H1 + G1 of
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

push_open_nodes(#node{g = G, h = H} = Node, OpenGbSets) ->
    gb_sets:insert({G + H, Node}, OpenGbSets).

push_closed_node(#node{x = X, y = Y} = Node, ClosedDict) ->
    dict:store({X, Y}, Node, ClosedDict).

%% @doc G值开销
%% 直走
g(#node{x = X1, y = Y1}, #node{x = X2, y = Y2}) when X1 == X2 orelse Y1 == Y2 -> 1;
%% 斜对角行走
g(#node{}, #node{}) -> 1.41421.

%% @doc 判断是否是同一个点
eq(#node{x = X, y = Y}, #node{x = X, y = Y}) -> true;
eq(#node{}, #node{}) -> false.

is_walk_grid(_, [], _) -> true;
is_walk_grid(MapMod, [BarName|BarNames], Pos) ->
    case MapMod:is(BarName, Pos) of
        true ->
            is_walk_grid(MapMod, BarNames, Pos);
        false ->
            false
    end.
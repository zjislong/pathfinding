%%% @author ZhengJia <zj952067409@163.com>
%%% @copyright 2018 ZhengJia
%%% @doc gen_server callback module implementation:
%%%
%%% @end
-module(flow_field).
-author('ZhengJia <zj952067409@163.com>').
%% API
-export([init/2, init_csv/2, write_csv/0, update/4, search/2]).

-record(node, {
               x = 0,
               y = 0,
               g = 0,
               s = true
              }).

pos2node({X, Y}) ->
    #node{x = X, y = Y}.

init(End, MapMod) ->
    [{IndexX, IndexY}] = pos_2d:pos2index(End, 60, 1),
    IndexList = [{IndexX + AddX, IndexY + AddY}||AddX <- [-2,-1,0,1,2,3], AddY <- [0,-1], IndexX + AddX >= 0, IndexY + AddY >= 0],
    OpenGbSets = gb_sets:new(),
    ClosedDict = dict:new(),
    F = fun(EndIndex, OpenGbSets0) ->
                EndNode = pos2node(EndIndex),
                push_open_nodes(EndNode#node{s = false}, OpenGbSets0)
        end,
    OpenGbSets1 = lists:foldl(F, OpenGbSets, IndexList),
    VisitedSets = sets:from_list(IndexList),
    do_init(OpenGbSets1, ClosedDict, VisitedSets, MapMod).

init_csv(End, MapMod) ->
    init(End, MapMod),
    write_csv().

write_csv() ->
    D = erlang:get(ff),
    file:delete("ff.csv"),
    Str = [io_lib:format("~p", [X])||X <- lists:seq(0, 70)],
    S = string:join(["////////"|Str], ","),
    file:write_file("ff.csv", S++"\n", [append]),
    write_csv(0, 0, D, [io_lib:format("~p", [0])]).

write_csv(71, 61, _, _) ->
    ok;
write_csv(71, Y, D, Str) ->
    S = string:join(Str, ","),
    file:write_file("ff.csv", S++"\n", [append]),
    write_csv(0, Y+1, D, [io_lib:format("~p", [Y+1])]);
write_csv(X, Y, D, Str) ->
    case dict:find({X, Y}, D) of
        {ok, #node{g=G, s=true}} when G < 200 -> Str1 = Str ++ [io_lib:format("~p", [G])];
        {ok, #node{g=G, s=false}} when G < 200 -> Str1 = Str ++ [io_lib:format("@~p", [G])];
        _ -> Str1 = Str ++ ["########"]
    end,
    write_csv(X+1, Y, D, Str1).

update(Index, State, H, D) ->
    ClosedDict = erlang:get(ff),
    case dict:find(Index, ClosedDict) of
        {ok, #node{g=G} = Node} ->
            Count = trunc(abs(H) / D) - 1,
            case State of
                null ->
                    ClosedDict1 = push_closed_node(Node#node{g=G+H}, ClosedDict);
                _ ->
                    ClosedDict1 = push_closed_node(Node#node{g=G+H, s=State}, ClosedDict)
            end,
            ClosedDict2 = do_update(Index, Count, H, D, east, ClosedDict1),
            ClosedDict3 = do_update(Index, Count, H, D, west, ClosedDict2),
            ClosedDict4 = do_update(Index, Count, H, D, north, ClosedDict3),
            ClosedDict5 = do_update(Index, Count, H, D, south, ClosedDict4),
            ClosedDict6 = do_update(Index, Count, H, D, northeast, ClosedDict5),
            ClosedDict7 = do_update(Index, Count, H, D, northwest, ClosedDict6),
            ClosedDict8 = do_update(Index, Count, H, D, southeast, ClosedDict7),
            ClosedDict9 = do_update(Index, Count, H, D, southwest, ClosedDict8),
            erlang:put(ff, ClosedDict9);
        _ ->
            none
    end.

search(Pos, Direction) ->
    [Index|Indexs] = pos_2d:pos2index(Pos, 60, 3),
    ClosedDict = erlang:get(ff),
    case dict:find(Index, ClosedDict) of
        {ok, CurNode} ->
            case do_search(Indexs, CurNode, ClosedDict, Direction) of
                CurNode ->
                    none;
                NextNode ->
                    {{NextNode#node.x * 60 + 30, NextNode#node.y * 60 + 30}, direction(CurNode, NextNode)}
            end;
        _ ->
            none
    end.

do_init(OpenGbTree, ClosedDict, VisitedSets, MapMod) ->
    case dequeue_cheapest_node(OpenGbTree) of
        none -> %% no other node
            erlang:put(ff, ClosedDict);
        {ok, OpenGbTree0, CurrentNode} ->
            %% move currentNode from open to closed
            ClosedDict0 = push_closed_node(CurrentNode, ClosedDict),
            %% Find all neighbors for the current node.
            %% NeighborsNodes = neighbors_nodes(diagonal, CurrentNode),
            NeighborsNodes = pos_2d:pos2index({CurrentNode#node.x, CurrentNode#node.y}, 1, 3),
            %% push neighbors if node validity
            {OpenGbTree1, ClosedDict1, VisitedSets0} = push_neighbors(NeighborsNodes, CurrentNode, OpenGbTree0,
                                                                      ClosedDict0, VisitedSets, MapMod),
            %% tail recursion
            do_init(OpenGbTree1, ClosedDict1, VisitedSets0, MapMod)
    end.

%% 获取相邻的node
push_neighbors([], _CurrentNode, OpenGbSets, ClosedDict, VisitedSets, _MapMod) ->
    {OpenGbSets, ClosedDict, VisitedSets};
push_neighbors([{X, Y}|T], CurrentNode, OpenGbSets, ClosedDict, VisitedSets, MapMod) ->
    case is_closed(X, Y, ClosedDict) of
        true ->
            push_neighbors(T, CurrentNode, OpenGbSets, ClosedDict, VisitedSets, MapMod);
        false ->
            NeighborsNode = pos2node({X, Y}),
            G = CurrentNode#node.g + g(NeighborsNode, CurrentNode),
            NeighborsNode0 = #node{x = X, y = Y, g = G},
            case sets:is_element({X, Y}, VisitedSets) of
                true -> %% 已经寻到过的点，更新open列表node
                    OpenGbSets0 = rescore_open_node(NeighborsNode0, OpenGbSets),
                    push_neighbors(T, CurrentNode, OpenGbSets0, ClosedDict, VisitedSets, MapMod);
                false ->
                    case not MapMod:is(bar, {X, Y}) of
                        true ->
                            OpenGbSets0 = push_open_nodes(NeighborsNode0, OpenGbSets),
                            VisitedSets0 = sets:add_element({X, Y}, VisitedSets),
                            push_neighbors(T, CurrentNode, OpenGbSets0, ClosedDict, VisitedSets0, MapMod);
                        false ->
                            NeighborsNode1 = NeighborsNode0#node{g = 255, s = false},
                            ClosedDict0 = push_closed_node(NeighborsNode1, ClosedDict),
                            push_neighbors(T, CurrentNode, OpenGbSets, ClosedDict0, VisitedSets, MapMod)
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
rescore_open_node(Node = #node{x = X, y = Y, g = G}, OpenGbSets) ->
    case search_open_node(X, Y, OpenGbSets) of
        {ok, Key = {_Score, #node{g = G1}}} ->
            case G < G1 of
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

push_open_nodes(#node{g = G} = Node, OpenGbSets) ->
    gb_sets:insert({G, Node}, OpenGbSets).

do_update(_, 0, _, _, _, ClosedDict) -> ClosedDict;
%%向东更新
do_update({X, Y}, Count, H, D, east, ClosedDict) ->
    CurIndex = {X+1, Y},
    case do_update(CurIndex, H, D, ClosedDict) of
        {H1, ClosedDict1} ->
            do_update(CurIndex, Count - 1, H1, D, east, ClosedDict1);
        none ->
            ClosedDict
    end;
%%向西更新
do_update({X, Y}, Count, H, D, west, ClosedDict) ->
    CurIndex = {X-1, Y},
    case do_update(CurIndex, H, D, ClosedDict) of
        {H1, ClosedDict1} ->
            do_update(CurIndex, Count - 1, H1, D, west, ClosedDict1);
        none ->
            ClosedDict
    end;
%%向北更新
do_update({X, Y}, Count, H, D, north, ClosedDict) ->
    CurIndex = {X, Y-1},
    case do_update(CurIndex, H, D, ClosedDict) of
        {H1, ClosedDict1} ->
            do_update(CurIndex, Count - 1, H1, D, north, ClosedDict1);
        none ->
            ClosedDict
    end;
%%向南更新
do_update({X, Y}, Count, H, D, south, ClosedDict) ->
    CurIndex = {X, Y+1},
    case do_update(CurIndex, H, D, ClosedDict) of
        {H1, ClosedDict1} ->
            do_update(CurIndex, Count - 1, H1, D, south, ClosedDict1);
        none ->
            ClosedDict
    end;
%%向东北更新
do_update({X, Y}, Count, H, D, northeast, ClosedDict) ->
    CurIndex = {X+1, Y-1},
    case do_update(CurIndex, H, D, ClosedDict) of
        {H1, ClosedDict1} ->
            ClosedDict2 = do_update(CurIndex, Count - 1, H1, D, north, ClosedDict1),
            ClosedDict3 = do_update(CurIndex, Count - 1, H1, D, east, ClosedDict2),
            do_update(CurIndex, Count - 1, H1, D, northeast, ClosedDict3);
        none ->
            ClosedDict
    end;
%%向西北更新
do_update({X, Y}, Count, H, D, northwest, ClosedDict) ->
    CurIndex = {X-1, Y-1},
    case do_update(CurIndex, H, D, ClosedDict) of
        {H1, ClosedDict1} ->
            ClosedDict2 = do_update(CurIndex, Count - 1, H1, D, north, ClosedDict1),
            ClosedDict3 = do_update(CurIndex, Count - 1, H1, D, west, ClosedDict2),
            do_update(CurIndex, Count - 1, H1, D, northwest, ClosedDict3);
        none ->
            ClosedDict
    end;
%%向东南更新
do_update({X, Y}, Count, H, D, southeast, ClosedDict) ->
    CurIndex = {X+1, Y+1},
    case do_update(CurIndex, H, D, ClosedDict) of
        {H1, ClosedDict1} ->
            ClosedDict2 = do_update(CurIndex, Count - 1, H1, D, south, ClosedDict1),
            ClosedDict3 = do_update(CurIndex, Count - 1, H1, D, east, ClosedDict2),
            do_update(CurIndex, Count - 1, H1, D, southeast, ClosedDict3);
        none ->
            ClosedDict
    end;
%%向西南更新
do_update({X, Y}, Count, H, D, southwest, ClosedDict) ->
    CurIndex = {X-1, Y+1},
    case do_update(CurIndex, H, D, ClosedDict) of
        {H1, ClosedDict1} ->
            ClosedDict2 = do_update(CurIndex, Count - 1, H1, D, south, ClosedDict1),
            ClosedDict3 = do_update(CurIndex, Count - 1, H1, D, west, ClosedDict2),
            do_update(CurIndex, Count - 1, H1, D, southwest, ClosedDict3);
        none ->
            ClosedDict
    end.

do_update(CurIndex, H, D, ClosedDict) ->
    case dict:find(CurIndex, ClosedDict) of
        {ok, #node{g=G} = CurNode} when G < 200 ->
            case H > 0 of
                true ->
                    H1 = H - D;
                false ->
                    H1 = H + D
            end,
            CurNode1 = CurNode#node{g = G + H1},
            {H1, push_closed_node(CurNode1, ClosedDict)};
        _ ->
            none
    end.

do_search(Indexs, StartNode, ClosedDict, Direction) ->
    do_search(Indexs, StartNode, StartNode, ClosedDict, Direction).

do_search([], Node, _, _, _) ->
    Node;
do_search([CurIndex|Indexs], #node{g=G1}=Node, StartNode, ClosedDict, Direction) ->
    case dict:find(CurIndex, ClosedDict) of
        {ok, #node{g=G2, s=true}=CurNode} ->
            case G2 + d(StartNode, CurNode, Direction)*0.02 < G1 of
                true ->
                    do_search(Indexs, CurNode, StartNode, ClosedDict, Direction);
                false ->
                    do_search(Indexs, Node, StartNode, ClosedDict, Direction)
            end;
        _ ->
            do_search(Indexs, Node, StartNode, ClosedDict, Direction)
    end.

%% @doc G值开销
%% 直走
g(#node{x = X1, y = Y1}, #node{x = X2, y = Y2}) when X1 == X2 orelse Y1 == Y2 -> 1;
%% 斜对角行走
g(#node{}, #node{}) -> 1.41421.

%%运动方向惯性
d(_, _, undefined) -> 0;
d(Node1, Node2, Direction) ->
    Dir = direction(Node1, Node2),
    D = abs(Dir - Direction),
    case D > 180 of
        true ->
            360 - D;
        false ->
            D
    end.

%%运动方向
direction(#node{x = Cx, y = Cy}, #node{x = Px, y = Py}) ->
    if
        Cx > Px ->
            if
                Cy > Py -> -45;
                Cy == Py -> 0;
                true -> 45
            end;
        Cx == Px ->
            if
                Cy > Py -> -90;
                true -> 90
            end;
        true ->
            if
                Cy > Py -> -135;
                Cy == Py -> 180;
                true -> 135
            end
    end.

push_closed_node(#node{x = X, y = Y} = Node, ClosedDict) ->
    dict:store({X, Y}, Node, ClosedDict).

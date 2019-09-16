%%% @author ZhengJia <zj952067409@163.com>
%%% @copyright 2018 ZhengJia
%%% @doc gen_server callback module implementation:
%%%
%%% @end
-module(jps).

-author('ZhengJia <zj952067409@163.com>').

-export([make/3, search/3]).

-record(node, {
               x = 0,
               y = 0,
               h = 0,
               g = 0,
               parent = undefined
              }).
%%====================================================================
%% API functions
%%====================================================================
make(DataMod, W, H) ->
    TarFile = "./data/"++atom_to_list(DataMod)++"_jps.erl",
    make_east({W - 1, 0}, {W, H}, -1, false, DataMod),
    make_west({0, 0}, {W, H}, -1, false, DataMod),
    make_south({0, H - 1}, {W, H}, -1, false, DataMod),
    make_north({0, 0}, {W, H}, -1, false, DataMod),
    make_northeast({W - 1, 0}, {W, H}, DataMod),
	make_northwest({0, 0}, {W, H}, DataMod),
    make_southeast({W - 1, H - 1}, {W, H}, DataMod),
	make_southwest({0, H - 1}, {W, H}, DataMod),
    file:delete(TarFile),
    write_file(TarFile, "-module("++atom_to_list(DataMod)++"_jps).
-export([distance/2]).
"),
    LayerDataStr = ["distance({"++integer_to_list(X)++","++integer_to_list(Y)++"}, "++atom_to_list(Dir)++") -> "++integer_to_list(D)||{{{X, Y}, Dir}, D}<-get()],
    write_file(TarFile, string:join(LayerDataStr, ";\n")),
    write_file(TarFile, ";\ndistance(_, _) -> 0.\n").

search(CurNode, GoalNode, JpsMod) ->
    case CurNode of
        #node{x = Cx, y = Cy, parent = {Px, Py}} ->
            if
                Cx > Px ->
                    if
                        Cy > Py -> Dir = southeast;
                        Cy == Py -> Dir = east;
                        true -> Dir = northeast
                    end;
                Cx == Px ->
                    if
                        Cy > Py -> Dir = south;
                        true -> Dir = north
                    end;
                true ->
                    if
                        Cy > Py -> Dir = southwest;
                        Cy == Py -> Dir = west;
                        true -> Dir = northwest
                    end
            end,
            Dirs = dir_lookup(Dir);
        _ ->
            Dirs = [southeast, east, northeast, south, north, southwest, west, northwest]
    end,
    search(Dirs, CurNode, GoalNode, JpsMod, []).
%%====================================================================
%% Internal functions
%%====================================================================
make_east({_, Y}, {_, H}, _, _, _) when Y >= H -> ok;
make_east({X, Y}, {W, H}, _, _, JpsMod) when X < 0 ->
    make_east({W - 1, Y + 1}, {W, H}, -1, false, JpsMod);
make_east({X, Y}, {W, H}, Count, MeetJP, JpsMod) ->
    case JpsMod:is_bar({X, Y}) of
        true ->
            put({{X, Y}, east}, 0),
            make_east({X - 1, Y}, {W, H}, -1, false, JpsMod);
        false ->
            Count1 = Count + 1,
            case MeetJP of
                true -> put({{X, Y}, east}, Count1);
                false -> put({{X, Y}, east}, -Count1)
            end,
            case is_jp({X, Y}, east, JpsMod) of
                true -> make_east({X - 1, Y}, {W, H}, 0, true, JpsMod);
                false -> make_east({X - 1, Y}, {W, H}, Count1, MeetJP, JpsMod)
            end
    end.

make_west({_, Y}, {_, H}, _, _, _) when Y >= H -> ok;
make_west({X, Y}, {W, H}, _, _, JpsMod) when X >= W ->
    make_west({0, Y + 1}, {W, H}, -1, false, JpsMod);
make_west({X, Y}, {W, H}, Count, MeetJP, JpsMod) ->
    case JpsMod:is_bar({X, Y}) of
        true ->
            put({{X, Y}, west}, 0),
            make_west({X + 1, Y}, {W, H}, -1, false, JpsMod);
        false ->
            Count1 = Count + 1,
            case MeetJP of
                true -> put({{X, Y}, west}, Count1);
                false -> put({{X, Y}, west}, -Count1)
            end,
            case is_jp({X, Y}, west, JpsMod) of
                true -> make_west({X + 1, Y}, {W, H}, 0, true, JpsMod);
                false -> make_west({X + 1, Y}, {W, H}, Count1, MeetJP, JpsMod)
            end
    end.

make_south({X, _}, {W, _}, _, _, _) when X >= W -> ok;
make_south({X, Y}, {W, H}, _, _, JpsMod) when Y < 0 ->
    make_south({X + 1, H - 1}, {W, H}, -1, false, JpsMod);
make_south({X, Y}, {W, H}, Count, MeetJP, JpsMod) ->
    case JpsMod:is_bar({X, Y}) of
        true ->
            put({{X, Y}, south}, 0),
            make_south({X, Y - 1}, {W, H}, -1, false, JpsMod);
        false ->
            Count1 = Count + 1,
            case MeetJP of
                true -> put({{X, Y}, south}, Count1);
                false -> put({{X, Y}, south}, -Count1)
            end,
            case is_jp({X, Y}, south, JpsMod) of
                true -> make_south({X, Y - 1}, {W, H}, 0, true, JpsMod);
                false -> make_south({X, Y - 1}, {W, H}, Count1, MeetJP, JpsMod)
            end
    end.

make_north({X, _}, {W, _}, _, _, _) when X >= W  -> ok;
make_north({X, Y}, {W, H}, _, _, JpsMod) when Y >= H ->
    make_north({X + 1, 0}, {W, H}, -1, false, JpsMod);
make_north({X, Y}, {W, H}, Count, MeetJP, JpsMod) ->
    case JpsMod:is_bar({X, Y}) of
        true ->
            put({{X, Y}, north}, 0),
            make_north({X, Y + 1}, {W, H}, -1, false, JpsMod);
        false ->
            Count1 = Count + 1,
            case MeetJP of
                true -> put({{X, Y}, north}, Count1);
                false -> put({{X, Y}, north}, -Count1)
            end,
            case is_jp({X, Y}, north, JpsMod) of
                true -> make_north({X, Y + 1}, {W, H}, 0, true, JpsMod);
                false -> make_north({X, Y + 1}, {W, H}, Count1, MeetJP, JpsMod)
            end
    end.

make_northeast({_, Y}, {_, H}, _) when Y >= H -> ok;
make_northeast({X, Y}, {W, H}, JpsMod) when X < 0 ->
    make_northeast({W - 1, Y + 1}, {W, H}, JpsMod);
make_northeast({X, Y} = Pos, {W, H}, JpsMod) ->
    case JpsMod:is_bar(Pos) of
        true -> make_northeast({X - 1, Y}, {W, H}, JpsMod);
        false ->
            Condition1 = JpsMod:is_bar({X + 1, Y}) orelse
                         JpsMod:is_bar({X, Y - 1}) orelse
                         JpsMod:is_bar({X + 1, Y - 1}),
            Condition2 = not JpsMod:is_bar({X + 1, Y}) andalso
                             not JpsMod:is_bar({X, Y - 1}) andalso
                                 (get({{X + 1, Y - 1}, north}) > 0 orelse
                                  get({{X + 1, Y - 1}, east}) > 0),
            if Condition1 -> put({Pos, northeast}, 0);
               Condition2 -> put({Pos, northeast}, 1);
               true ->
                    Increment = get({{X + 1, Y - 1}, northeast}),
                    case Increment > 0 of
                        true -> put({Pos, northeast}, Increment + 1);
                        false -> put({Pos, northeast}, Increment - 1)
                    end
            end,
            make_northeast({X - 1, Y}, {W, H}, JpsMod)
    end.

make_northwest({_, Y}, {_, H}, _) when Y >= H -> ok;
make_northwest({X, Y}, {W, H}, JpsMod) when X >= W ->
    make_northwest({0, Y + 1}, {W, H}, JpsMod);
make_northwest({X, Y} = Pos, {W, H}, JpsMod) ->
    case JpsMod:is_bar(Pos) of
        true -> make_northwest({X + 1, Y}, {W, H}, JpsMod);
        false ->
            Condition1 = JpsMod:is_bar({X - 1, Y}) orelse
                         JpsMod:is_bar({X, Y - 1}) orelse
                         JpsMod:is_bar({X - 1, Y - 1}),
            Condition2 = not JpsMod:is_bar({X - 1, Y}) andalso
                             not JpsMod:is_bar({X, Y - 1}) andalso
                                 (get({{X - 1, Y - 1}, north}) > 0 orelse
                                  get({{X - 1, Y - 1}, west}) > 0),
            if Condition1 -> put({Pos, northwest}, 0);
               Condition2 -> put({Pos, northwest}, 1);
               true ->
                    Increment = get({{X - 1, Y - 1}, northwest}),
                    case Increment > 0 of
                        true -> put({Pos, northwest}, Increment + 1);
                        false -> put({Pos, northwest}, Increment - 1)
                    end
            end,
            make_northwest({X + 1, Y}, {W, H}, JpsMod)
    end.

make_southeast({_, Y}, _, _) when Y < 0 -> ok;
make_southeast({X, Y}, {W, H}, JpsMod) when X < 0 ->
    make_southeast({W - 1, Y - 1}, {W, H}, JpsMod);
make_southeast({X, Y} = Pos, {W, H}, JpsMod) ->
    case JpsMod:is_bar(Pos) of
        true -> make_southeast({X - 1, Y}, {W, H}, JpsMod);
        false ->
            Condition1 = JpsMod:is_bar({X + 1, Y}) orelse
                         JpsMod:is_bar({X, Y + 1}) orelse
                         JpsMod:is_bar({X + 1, Y + 1}),
            Condition2 = not JpsMod:is_bar({X + 1, Y}) andalso
                             not JpsMod:is_bar({X, Y + 1}) andalso
                                 (get({{X + 1, Y + 1}, south}) > 0 orelse
                                  get({{X + 1, Y + 1}, east}) > 0),
            if Condition1 -> put({Pos, southeast}, 0);
               Condition2 -> put({Pos, southeast}, 1);
               true ->
                    Increment = get({{X + 1, Y + 1}, southeast}),
                    case Increment > 0 of
                        true -> put({Pos, southeast}, Increment + 1);
                        false -> put({Pos, southeast}, Increment - 1)
                    end
            end,
            make_southeast({X - 1, Y}, {W, H}, JpsMod)
    end.

make_southwest({_, Y}, _, _) when Y < 0 -> ok;
make_southwest({X, Y}, {W, H}, JpsMod) when X >= W ->
    make_southwest({0, Y - 1}, {W, H}, JpsMod);
make_southwest({X, Y} = Pos, {W, H}, JpsMod) ->
    case JpsMod:is_bar(Pos) of
        true -> make_southwest({X + 1, Y}, {W, H}, JpsMod);
        false ->
            Condition1 = JpsMod:is_bar({X - 1, Y}) orelse
                         JpsMod:is_bar({X, Y + 1}) orelse
                         JpsMod:is_bar({X - 1, Y + 1}),
            Condition2 = not JpsMod:is_bar({X - 1, Y}) andalso
                             not JpsMod:is_bar({X, Y + 1}) andalso
                                 (get({{X - 1, Y + 1}, south}) > 0 orelse
                                  get({{X - 1, Y + 1}, west}) > 0),
            if Condition1 -> put({Pos, southwest}, 0);
               Condition2 -> put({Pos, southwest}, 1);
               true ->
                    Increment = get({{X - 1, Y + 1}, southwest}),
                    case Increment > 0 of
                        true -> put({Pos, southwest}, Increment + 1);
                        false -> put({Pos, southwest}, Increment - 1)
                    end
            end,
            make_southwest({X + 1, Y}, {W, H}, JpsMod)
    end.

is_jp({X, Y}, east, JpsMod) ->
    not JpsMod:is_bar({X - 1, Y}) andalso
        ((JpsMod:is_bar({X - 1, Y + 1}) andalso not JpsMod:is_bar({X, Y + 1}))
         orelse
         (JpsMod:is_bar({X - 1, Y - 1}) andalso not JpsMod:is_bar({X, Y - 1})));
is_jp({X, Y}, west, JpsMod) ->
    not JpsMod:is_bar({X + 1, Y}) andalso
        ((JpsMod:is_bar({X + 1, Y + 1}) andalso not JpsMod:is_bar({X, Y + 1}))
         orelse
         (JpsMod:is_bar({X + 1, Y - 1}) andalso not JpsMod:is_bar({X, Y - 1})));
is_jp({X, Y}, south, JpsMod) ->
    not JpsMod:is_bar({X, Y - 1}) andalso
        ((JpsMod:is_bar({X - 1, Y - 1}) andalso not JpsMod:is_bar({X - 1, Y}))
         orelse
         (JpsMod:is_bar({X + 1, Y - 1}) andalso not JpsMod:is_bar({X + 1, Y})));
is_jp({X, Y}, north, JpsMod) ->
    not JpsMod:is_bar({X, Y + 1}) andalso
        ((JpsMod:is_bar({X - 1, Y + 1}) andalso not JpsMod:is_bar({X - 1, Y}))
         orelse
         (JpsMod:is_bar({X + 1, Y + 1}) andalso not JpsMod:is_bar({X + 1, Y}))).

dir_lookup(south) -> [west, southwest, south, southeast, east];
dir_lookup(southeast) -> [south, southeast, east];
dir_lookup(east) -> [south, southeast, east, northeast, north];
dir_lookup(northeast) -> [east, northeast, north];
dir_lookup(north) -> [east, northeast, north, northwest, west];
dir_lookup(northwest) -> [north, northwest, west];
dir_lookup(west) -> [north, northwest, west, southwest, south];
dir_lookup(southwest) -> [west, southwest, south].

search([], _, _, _, Success) -> Success;
search([east|Dirs], CurNode, GoalNode, JpsMod, Success) ->
    Distance = get_distance_value(JpsMod, CurNode, east),
    DiffCol = diff_col(CurNode, GoalNode),
    case is_in_direction(CurNode, GoalNode, east) andalso abs(Distance) >= DiffCol of
        true ->
            search(Dirs, CurNode, GoalNode, JpsMod, [GoalNode|Success]);
        false ->
            case Distance > 0 of
                true ->
                    NewSuccess = #node{x = CurNode#node.x + Distance, y = CurNode#node.y},
                    search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
                false ->
                    search(Dirs, CurNode, GoalNode, JpsMod, Success)
            end
    end;
search([west|Dirs], CurNode, GoalNode, JpsMod, Success) ->
    Distance = get_distance_value(JpsMod, CurNode, west),
    DiffCol = diff_col(CurNode, GoalNode),
    case is_in_direction(CurNode, GoalNode, west) andalso abs(Distance) >= DiffCol of
        true ->
            search(Dirs, CurNode, GoalNode, JpsMod, [GoalNode|Success]);
        false ->
            case Distance > 0 of
                true ->
                    NewSuccess = #node{x = CurNode#node.x - Distance, y = CurNode#node.y},
                    search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
                false ->
                    search(Dirs, CurNode, GoalNode, JpsMod, Success)
            end
    end;
search([south|Dirs], CurNode, GoalNode, JpsMod, Success) ->
    Distance = get_distance_value(JpsMod, CurNode, south),
    DiffRow = diff_row(CurNode, GoalNode),
    case is_in_direction(CurNode, GoalNode, south) andalso abs(Distance) >= DiffRow of
        true ->
            search(Dirs, CurNode, GoalNode, JpsMod, [GoalNode|Success]);
        false ->
            case Distance > 0 of
                true ->
                    NewSuccess = #node{x = CurNode#node.x, y = CurNode#node.y + Distance},
                    search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
                false ->
                    search(Dirs, CurNode, GoalNode, JpsMod, Success)
            end
    end;
search([north|Dirs], CurNode, GoalNode,JpsMod,  Success) ->
    Distance = get_distance_value(JpsMod, CurNode, north),
    DiffRow = diff_row(CurNode, GoalNode),
    case is_in_direction(CurNode, GoalNode, north) andalso abs(Distance) >= DiffRow of
        true ->
            search(Dirs, CurNode, GoalNode, JpsMod, [GoalNode|Success]);
        false ->
            case Distance > 0 of
                true ->
                    NewSuccess = #node{x = CurNode#node.x, y = CurNode#node.y - Distance},
                    search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
                false ->
                    search(Dirs, CurNode, GoalNode, JpsMod, Success)
            end
    end;
search([northeast|Dirs], CurNode, GoalNode, JpsMod, Success) ->
    Distance = get_distance_value(JpsMod, CurNode, northeast),
    DiffMin = diff_min(CurNode, GoalNode),
    case is_in_direction(CurNode, GoalNode, northeast) andalso abs(Distance) >= DiffMin of
        true ->
            NewSuccess = #node{x = CurNode#node.x + DiffMin, y = CurNode#node.y - DiffMin},
            search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
        false ->
            case Distance > 0 of
                true ->
                    NewSuccess = #node{x = CurNode#node.x + Distance, y = CurNode#node.y - Distance},
                    search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
                false ->
                    search(Dirs, CurNode, GoalNode, JpsMod, Success)
            end
    end;
search([northwest|Dirs], CurNode, GoalNode, JpsMod, Success) ->
    Distance = get_distance_value(JpsMod, CurNode, northwest),
    DiffMin = diff_min(CurNode, GoalNode),
    case is_in_direction(CurNode, GoalNode, northwest) andalso abs(Distance) >= DiffMin of
        true ->
            NewSuccess = #node{x = CurNode#node.x - DiffMin, y = CurNode#node.y - DiffMin},
            search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
        false ->
            case Distance > 0 of
                true ->
                    NewSuccess = #node{x = CurNode#node.x - Distance, y = CurNode#node.y - Distance},
                    search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
                false ->
                    search(Dirs, CurNode, GoalNode, JpsMod, Success)
            end
    end;
search([southeast|Dirs], CurNode, GoalNode, JpsMod, Success) ->
    Distance = get_distance_value(JpsMod, CurNode, southeast),
    DiffMin = diff_min(CurNode, GoalNode),
    case is_in_direction(CurNode, GoalNode, southeast) andalso abs(Distance) >= DiffMin of
        true ->
            NewSuccess = #node{x = CurNode#node.x + DiffMin, y = CurNode#node.y + DiffMin},
            search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
        false ->
            case Distance > 0 of
                true ->
                    NewSuccess = #node{x = CurNode#node.x + Distance, y = CurNode#node.y + Distance},
                    search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
                false ->
                    search(Dirs, CurNode, GoalNode, JpsMod, Success)
            end
    end;
search([southwest|Dirs], CurNode, GoalNode, JpsMod, Success) ->
    Distance = get_distance_value(JpsMod, CurNode, southwest),
    DiffMin = diff_min(CurNode, GoalNode),
    case is_in_direction(CurNode, GoalNode, southwest) andalso abs(Distance) >= DiffMin of
        true ->
            NewSuccess = #node{x = CurNode#node.x - DiffMin, y = CurNode#node.y + DiffMin},
            search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
        false ->
            case Distance > 0 of
                true ->
                    NewSuccess = #node{x = CurNode#node.x - Distance, y = CurNode#node.y + Distance},
                    search(Dirs, CurNode, GoalNode, JpsMod, [NewSuccess|Success]);
                false ->
                    search(Dirs, CurNode, GoalNode, JpsMod, Success)
            end
    end.

diff_min(Node1, Node2) -> min(diff_col(Node1, Node2), diff_row(Node1, Node2)).

diff_col(#node{x = X1}, #node{x = X2}) -> abs(X1 - X2).

diff_row(#node{y = Y1}, #node{y = Y2}) -> abs(Y1 - Y2).

get_distance_value(JpsMod, #node{x = X, y= Y}, Dir) -> JpsMod:distance({X, Y}, Dir).

is_in_direction(CurNode, GoalNode, east) ->
    CurNode#node.y == GoalNode#node.y andalso GoalNode#node.x > CurNode#node.x;
is_in_direction(CurNode, GoalNode, west) ->
    CurNode#node.y == GoalNode#node.y andalso GoalNode#node.x < CurNode#node.x;
is_in_direction(CurNode, GoalNode, south) ->
    CurNode#node.x == GoalNode#node.x andalso GoalNode#node.y > CurNode#node.y;
is_in_direction(CurNode, GoalNode, north) ->
    CurNode#node.x == GoalNode#node.x andalso GoalNode#node.y < CurNode#node.y;
is_in_direction(CurNode, GoalNode, northeast) ->
    GoalNode#node.x > CurNode#node.x andalso CurNode#node.y > GoalNode#node.y;
is_in_direction(CurNode, GoalNode, northwest) ->
    GoalNode#node.x < CurNode#node.x andalso CurNode#node.y > GoalNode#node.y;
is_in_direction(CurNode, GoalNode, southeast) ->
    GoalNode#node.x > CurNode#node.x andalso CurNode#node.y < GoalNode#node.y;
is_in_direction(CurNode, GoalNode, southwest) ->
    GoalNode#node.x < CurNode#node.x andalso CurNode#node.y < GoalNode#node.y.

write_file(FileName, Data) ->
    file:write_file(FileName, unicode:characters_to_binary(Data), [append]).

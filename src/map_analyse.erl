%%% @author ZhengJia <zj952067409@163.com>
%%% @copyright 2018 ZhengJia
%%% @doc gen_server callback module implementation:
%%%
%%% @end
-module(map_analyse).
-author('ZhengJia <zj952067409@163.com>').

-export([decode/3]).

%%====================================================================
%% API functions
%%====================================================================
%%解析json文件
decode(MapMod, SrcFile, LayerOptions) ->
    DirName = filename:dirname(SrcFile),
    TarFile = filename:join(DirName, MapMod++".erl"),
    file:delete(TarFile),
    case file:read_file(SrcFile) of
        {ok, Bin} ->
            case jsx:decode(Bin, [return_maps]) of
                #{<<"width">> := W, <<"height">> := H, <<"tilewidth">> := S, <<"layers">> := Layers} ->
                write_file(TarFile, "-module("++MapMod++").\n-export([info/0,pos/1,is/2,id/2]).\n"),
                write_file(TarFile, "info() -> {"++integer_to_list(W)++", "++integer_to_list(H)++"}.\n"),
                {PosString, IsString, IdString} = decode_layers(W, H, S, Layers, LayerOptions),
                write_file(TarFile, PosString),
                write_file(TarFile, IsString),
                write_file(TarFile, IdString),
                ok;
            _ ->
                error
            end;
        _ ->
            error
    end.
%%====================================================================
%% Internal functions
%%====================================================================
%%解析某一种场景元素的数据
decode_layers(W, H, S, Layers, LayerOptions) ->
    decode_layers(W, H, S, Layers, LayerOptions, [], [], []).

decode_layers(_, _, _, [], _LayerOptions, PosData, IsData, IdData) ->
    PosString = string:join(PosData, "\n") ++ "\npos(_) -> [].\n",
    IsString = string:join(IsData, "\n") ++ "\nis(_, _) -> false.\n",
    IdString = string:join(IdData, "\n") ++ "\nid(_, _) -> 0.\n",
    {PosString, IsString, IdString};
decode_layers(W, H, S, [#{<<"name">> := LayerName0, <<"data">> := Data}|Layers], LayerOptions, PosData, IsData, IdData) ->
    LayerName = binary_to_list(LayerName0),
    case lists:keyfind(LayerName, 1, LayerOptions) of
        {_, pos_analyse, Offset} ->
            PosList = pos_analyse(Data, W, S, Offset),
            String = pos_string(LayerName, PosList),
            decode_layers(W, H, S, Layers, LayerOptions, [String|PosData], IsData, IdData);
        {_, is_analyse} ->
            Bin = is_analyse(Data, <<>>),
            String = is_string(LayerName, Bin, W, H),
            decode_layers(W, H, S, Layers, LayerOptions, PosData, [String|IsData], IdData);
        {_, id_analyse} ->
            PosList = id_analyse(Data, 0, W, []),
            String = id_string(LayerName, PosList),
            decode_layers(W, H, S, Layers, LayerOptions, PosData, IsData, [String|IdData]);
        _ ->
            decode_layers(W, H, S, Layers, LayerOptions, PosData, IsData, IdData)
    end.

%%分析计算出场景元素坐标
pos_analyse(Data, W, S, Offset) ->
    pos_analyse(Data, W, S, Offset, 0, []).

pos_analyse([], _, _, _, _, Res) ->
    Res;
pos_analyse([0|Data], W, S, Offset, Index, Res) ->
    pos_analyse(Data, W, S, Offset, Index+1, Res);
pos_analyse([_|Data], W, S, {OffsetX, OffsetY}, Index, Res) ->
    Pos = {Index rem W * S + OffsetX, Index div W * S + OffsetY},
    pos_analyse(Data, W, S, {OffsetX, OffsetY}, Index+1, [Pos|Res]).

pos_string(LayerName, PosList) ->
    PosList1 = ["{"++integer_to_list(X)++","++integer_to_list(Y)++"}"||{X, Y}<-PosList],
    "pos("++LayerName++") ->\n    ["++string:join(PosList1, ",")++"];".

is_analyse([], Bin) -> Bin;
is_analyse([0|Data], Bin) -> is_analyse(Data, <<Bin/bits, 0:1>>);
is_analyse([_|Data], Bin) -> is_analyse(Data, <<Bin/bits, 1:1>>).

%%给连成一片的区域编号
id_analyse([], _, _, Res) ->
    merge_tent(Res, 1),
    Res;
id_analyse([0|Data], Index, W, Res) ->
    id_analyse(Data, Index+1, W, Res);
id_analyse([_|Data], Index, W, Res) ->
    X = Index rem W,
    Y = Index div W,
    erlang:put({X, Y}, true),
    id_analyse(Data, Index+1, W, [{X, Y}|Res]).

merge_tent([], _) -> ok;
merge_tent([Pos|PosList], ID) ->
    case erlang:get(Pos) of
        undefined ->
            merge_tent(PosList, ID);
        true ->
            merge_tent1([Pos], ID),
            merge_tent(PosList, ID + 1);
        _ ->
            merge_tent(PosList, ID)
    end.

merge_tent1([], _) -> ok;
merge_tent1([Pos|PosList], ID) ->
    case erlang:get(Pos) of
        undefined ->
            merge_tent1(PosList, ID);
        true ->
            {X, Y} = Pos,
            erlang:put(Pos, ID),
            merge_tent1([{X-1, Y-1}, {X, Y-1}, {X+1, Y-1}, {X-1, Y}, {X+1, Y}, {X-1, Y+1}, {X, Y+1}, {X+1, Y+1}], ID),
            merge_tent1(PosList, ID);
        _ ->
            merge_tent1(PosList, ID)
    end.

is_string(LayerName, Bin, W, H) ->
    Bin1 = binary_to_list(term_to_bitstring(Bin)),
    Head = case Bin of
        <<1:1, _/bits>> ->
            true;
        _ ->
            false
    end,
    "
is("++LayerName++", {0, 0}) ->
    "++atom_to_list(Head)++";
is("++LayerName++", {X, _}) when X < 0 orelse X >= "++integer_to_list(W)++" ->
    true;
is("++LayerName++", {_, Y}) when Y < 0 orelse Y >= "++integer_to_list(H)++" ->
    true;
is("++LayerName++", {X, Y}) ->
    Index = Y * "++integer_to_list(W)++" + X + 1,
    A = Index - 1,
    case "++Bin1++" of
        <<_:A, 1:1, _/bits>> ->
            true;
        _ ->
            false
    end;".

id_string(LayerName, PosList) ->
    F = fun({X, Y}) ->
            ID = erlang:get({X, Y}),
            "id("++LayerName++", {"++integer_to_list(X)++", "++integer_to_list(Y)++"}) -> "++integer_to_list(ID)++";"
        end,
    LayerDataStr = [F(Pos)||Pos<-PosList],
    string:join(LayerDataStr, "\n").

write_file(FileName, Data) ->
    file:write_file(FileName, unicode:characters_to_binary(Data), [append]).

term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/4

-mode(compile).

main(Args) ->
  Input = read_input(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_input() ->
  to_map(read_lines([])).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res ->
      [T|_] = string:split(Res, "\n"),
      read_lines([T|Acc])
  end.

to_map(Lines) ->
  Fold1 =
    fun(Line, {J, Acc}) ->
        Fold2 =
          fun(C, {I, #{mx := MX} = Map}) ->
              {I + 1, Map#{{I, J} => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first({MMY, #{mx := MX} = Map}) ->
  MY = MMY - 1,
  Dirs =
    [ { 1, 0}
    , {-1, 0}
    , { 0, 1}
    , { 0,-1}
    , { 1, 1}
    , { 1,-1}
    , {-1, 1}
    , {-1,-1}
    ],
  Fold =
    fun(Dir, Acc) ->
        Find = find_all({MX, MY}, Dir, Map),
        Find + Acc
    end,
  lists:foldl(Fold, 0, Dirs).

find_all({MX, MY}, Dir, Map) ->
  Starts = [{X, Y} || X <- lists:seq(1, MX), Y <- lists:seq(1, MY)],
  Fold =
    fun(Start, Acc) ->
        case find(Start, Dir, Map, "XMAS") of
          true -> Acc + 1;
          false -> Acc
        end
    end,
  lists:foldl(Fold, 0, Starts).

find(_, _, _, []) -> true;
find({X, Y}, {DX, DY} = D, Map, [H|T]) ->
  case maps:get({X,Y}, Map, $#) of
    H -> find({X + DX, Y + DY}, D, Map, T);
    _ -> false
  end.                       

solve_second({MMY, #{mx := MX} = Map}) ->
  MY = MMY - 1,
  Starts = [{X, Y} || X <- lists:seq(1, MX), Y <- lists:seq(1, MY)],
  Fold =
    fun(Start, Acc) ->
        case find_x(Start, Map) of
          true -> Acc + 1;
          false -> Acc
        end
    end,
  lists:foldl(Fold, 0, Starts).

find_x({X, Y} = Center, Map) ->
  case maps:get(Center, Map, $#) of
    $A ->
      TL = maps:get({X-1, Y-1}, Map, $#),
      BL = maps:get({X+1, Y-1}, Map, $#),
      TR = maps:get({X-1, Y+1}, Map, $#),
      BR = maps:get({X+1, Y+1}, Map, $#),
      D1 =
        case {TL, BR} of
          {$M, $S} -> true;
          {$S, $M} -> true;
          _ -> false
        end,
      D2 =
        case {TR, BL} of
          {$M, $S} -> true;
          {$S, $M} -> true;
          _ -> false
        end,
      D1 andalso D2;
    _ -> false
  end.

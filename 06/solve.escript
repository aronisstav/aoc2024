#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/6

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
              NMap =
                case C of
                  $^ -> Map#{s => {I, J}};
                  _ -> Map
                end,
              {I + 1, NMap#{{I, J} => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first({_, #{s := S} = Map}) ->
  Visit = walk(S, u, Map, #{S => 1}),
  maps:size(Visit).

walk({X, Y}, Dir, Map, Visit) ->
  {NX, NY, NDir} =
    case Dir of
      u -> {X, Y - 1, r};
      d -> {X, Y + 1, l};
      l -> {X - 1, Y, u};
      r -> {X + 1, Y, d}
    end,
  NPos = {NX, NY},
  case maps:get(NPos, Map, $x) of
    $x -> Visit;
    $# -> walk({X, Y}, NDir, Map, Visit);
    _ ->
      NVisit = Visit#{NPos => 1},
      walk(NPos, Dir, Map, NVisit)
  end.

solve_second({MMY, #{mx := MX} = Map}) ->
  MY = MMY - 1,
  Fold =
    fun({X, Y}, Acc) ->
        Acc + is_loop({X, Y}, Map)
    end,
  Adds = [{X, Y} || X <- lists:seq(1, MX), Y <- lists:seq(1, MY)],
  lists:foldl(Fold, 0, Adds).

is_loop(Pos, #{s := S} = Map) ->
  case maps:get(Pos, Map) of
    $. ->
      NMap = Map#{Pos => $#},
      case is_loop(S, u, NMap, #{}) of
        true -> 1;
        false -> 0
      end;
    _ -> 0
  end.

is_loop({X, Y} = Pos, Dir, Map, Visit) ->
  case maps:get({Pos, Dir}, Visit, 0) of
    1 -> true;
    0 ->
      NVisit = Visit#{{Pos, Dir} => 1},
      {NX, NY, NDir} =
        case Dir of
          u -> {X, Y - 1, r};
          d -> {X, Y + 1, l};
          l -> {X - 1, Y, u};
          r -> {X + 1, Y, d}
        end,
      NPos = {NX, NY},
      case maps:get(NPos, Map, $x) of
        $x -> false;
        $# -> is_loop(Pos, NDir, Map, NVisit);
        _ -> is_loop(NPos, Dir, Map, NVisit)
      end
  end.

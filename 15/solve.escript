#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/15

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
  read_lines(1, [], #{}).

read_lines(1, Acc, Input) ->
  case io:get_line("") of
    "\n" ->
      {MY, Map} = to_map(lists:reverse(Acc)),
      read_lines(2, [], Input#{map => Map#{my => MY - 1}});
    Res ->
      [T|_] = string:split(Res, "\n"),
      read_lines(1, [T|Acc], Input)
  end;
read_lines(2, Acc, Input) ->
  case io:get_line("") of
    eof -> Input#{moves => lists:append(lists:reverse(Acc))};
    Res ->
      [T|_] = string:split(Res, "\n"),
      read_lines(2, [T|Acc], Input)
  end.

to_map(Lines) ->
  Fold1 =
    fun(Line, {J, Acc}) ->
        Fold2 =
          fun(C, {I, #{mx := MX} = Map}) ->
              Pos = {I, J},
              NMap =
                case C of
                  $@ -> Map#{s => {I, J}};
                  _ -> Map
                end,
              {I + 1, NMap#{Pos => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first(#{map := Map, moves := Moves}) ->
  FMap = move(Moves, Map, fun push/3),
  score(FMap).

move([], Map, _) -> Map;
move([M|Ove], #{s := {X, Y} = Pos} = Map, Push) ->
  {DX, DY} = Dir =
    case M of
      $^ -> { 0, -1};
      $v -> { 0,  1};
      $> -> { 1,  0};
      $< -> {-1,  0}
    end,
  NPos = {X + DX, Y + DY},
  case Push(NPos, Dir, Map) of
    false -> move(Ove, Map, Push);
    {true, NMap} ->
      FMap = NMap#{Pos => $., NPos => $@, s => NPos},
      move(Ove, FMap, Push)
  end.

push(Pos, Dir, Map) ->
  push(Pos, Dir, false, Map).

push({X, Y} = Pos, {DX, DY} = Dir, Box, Map) ->
  case maps:get(Pos, Map) of
    $O -> push({X + DX, Y + DY}, Dir, true, Map);
    $. ->
      NMap =
        case Box of
          true -> Map#{Pos => $O};
          false -> Map
        end,
      {true, NMap};
    $# -> false
  end.

score(#{mx := MX, my := MY} = Map) ->
  print(Map, {MX, MY}),
  Fold =
    fun({X, Y}, V, Acc) ->
        case V of
          D when D == $O; D == $[ -> Acc + 100 * (Y - 1) + X - 1;
          _ -> Acc
        end;
       (_, _, Acc) -> Acc
    end,
  maps:fold(Fold, 0, Map).

print(M, {MX, MY}) ->
  For1 =
    fun(Y) ->
        S = [maps:get({X, Y}, M, $ ) || X <- lists:seq(1, MX)],
        io:format("~s~n", [S])
    end,
  lists:foreach(For1, lists:seq(1, MY)).

solve_second(#{map := SMap, moves := Moves}) ->
  Map = expand(SMap),
  FMap = move(Moves, Map, fun push2/3),
  score(FMap).

expand(Map) ->
  Fold =
    fun({X, Y}, V, Acc) ->
        {V1, V2} =
          case V of
            $# -> {$#, $#};
            $. -> {$., $.};
            $O -> {$[, $]};
            $@ -> {$@, $.}
          end,
        Acc#{{2 * X - 1, Y} => V1, {2 * X, Y} => V2};
       (my, MY, Acc) -> Acc#{my => MY};
       (mx, MX, Acc) -> Acc#{mx => 2 * MX};
       (s, {X, Y}, Acc) -> Acc#{s => {2 * X - 1, Y}}
    end,
  maps:fold(Fold, #{}, Map).

push2(Pos, Dir, Map) ->
  push3(queue:from_list([{$@, Pos}]), Dir, Map).

push3(Queue, Dir, Map) ->
  {V, NQ} = queue:out(Queue),
  case V of
    empty -> {true, Map};
    {value, {What, {X, Y} = Pos}} ->
      case maps:get(Pos, Map) of
        $. -> push3(NQ, Dir, Map#{Pos => What});
        $# -> false;
        Else ->
          NM = Map#{Pos => What},
          {Moves, FM} =
            case Dir of
              {DX, 0} -> {[{Else, {X + DX, Y}}], NM};
              {0, DY} ->
                case {What, Else} of
                  {A,  A} -> {[{Else, {X, Y + DY}}], NM};
                  {_, $[} ->
                    {[{$[, {X, Y + DY}}, {$], {X + 1, Y + DY}}], NM#{{X + 1, Y} => $.}};
                  {_, $]} ->
                    {[{$], {X, Y + DY}}, {$[, {X - 1, Y + DY}}], NM#{{X - 1, Y} => $.}}
                end
            end,
          FQ = lists:foldl(fun queue:in/2, NQ, Moves),
          push3(FQ, Dir, FM)
      end
  end.

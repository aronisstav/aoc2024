#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/20

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
                  $S -> Map#{s => {I, J}};
                  $E -> Map#{e => {I, J}};
                  _ -> Map
                end,
              {I + 1, NMap#{{I, J} => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first({_, #{e := E} = Map}) ->
  Vs = walk({0, E}, Map, #{}),
  Ds =
  [ { 2,  0}
  , {-2,  0}
  , { 0,  2}
  , { 0, -2}
  ],
  Cheat = cheat({2, Ds}, Vs, #{}),
  count(Cheat).

walk({C, S}, #{s := S}, V) ->
  V#{S => C};
walk({C, {X, Y} = Pos}, Map, V) ->
  NV = V#{Pos => C},
  Ds =
    [ { 1,  0}
    , {-1,  0}
    , { 0,  1}
    , { 0, -1}
    ],
  Cs = [{X + DX, Y + DY} || {DX, DY} <- Ds],
  Vs = [{P, maps:get(P, V, x), maps:get(P, Map)} || P <- Cs],
  [N] = [P || {P, PV, PX} <- Vs, PV =:= x, PX =/= $#],
  walk({C + 1, N}, Map, NV).

cheat({N, Ds}, Vs, Acc0) ->
  Fold =
    fun({X, Y} = P, V, Acc) ->
      Cs = [{X + DX, Y + DY} || {DX, DY} <- Ds],
      CVs = [{C, maps:get(C, Vs, x)} || C <- Cs],
      CC = [{{P, C}, V - CV - N} || {C, CV} <- CVs, CV < V - N],
      Fold2 = fun({D, W}, Acc1) -> Acc1#{D => W} end,
      lists:foldl(Fold2, Acc, CC)
    end,
  maps:fold(Fold, Acc0, Vs).

solve_second({_, #{e := E} = Map}) ->
    Vs = walk({0, E}, Map, #{}),
    Cheat = cheatB(Vs),
    count(Cheat).

cheatB(Vs) ->
  Fold1 =
    fun(T, Acc0) ->
      Fold2 =
        fun(X, Acc1) ->
          Y = T - X,
          case X + Y > 1 of
            true -> [{T, [{X, Y}, {-X, Y}, {X , -Y}, {-X, -Y}]}|Acc1];
            false -> Acc1
          end
        end,
      lists:foldl(Fold2, Acc0, lists:seq(0, T))
    end,
  Ds = lists:foldl(Fold1, [], lists:seq(2, 20)),
  Fold2 = fun(D, Acc) -> cheat(D, Vs, Acc) end,
  lists:foldl(Fold2, #{}, Ds).

count(C) ->
  T = 100,
  Fold =
    fun(_, V, A) ->
      case V >= T of
        true -> A + 1;
        false -> A
      end
    end,
  maps:fold(Fold, 0, C).


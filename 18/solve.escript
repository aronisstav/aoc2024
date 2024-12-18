#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/18

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
  read_many("~d,~d", []).

read_many(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  L = 1024,
  I = lists:sublist(Input, L),
  walk(I).

walk(I) ->
  Ms = {70, 70},
  Fold = fun([X, Y], Acc) -> Acc#{{X, Y} => $#} end,
  Map = lists:foldl(Fold, #{g => Ms}, I),
  S = {0, 0},
  E = est(S, Map),
  LQ = gb_sets:from_list([{0, E, S}]),
  walk(LQ, Map, #{}).

est({X, Y}, #{g := {A, B}}) ->
  case
    X < 0 orelse
    Y < 0 orelse
    X > A orelse
    Y > B
  of
    true -> -1;
    false -> abs(X - A) + abs(Y - B)
  end.
  
walk({0, nil}, _, _) -> false;
walk(Q, Map, V) ->
  {{C, E0, {X, Y} = Pos}, NQ} = gb_sets:take_smallest(Q),
  case E0 =:= 0 of
    true -> C;
    false ->
      case
        (maps:get(Pos, Map, $.) =:= $#) orelse
        (maps:get(Pos, V, infinity) < C)
      of
        true -> walk(NQ, Map, V);
        false ->
          NV = V#{Pos => C},
          NPs =
            [ {X + 1, Y}
            , {X - 1, Y}
            , {X, Y + 1}
            , {X, Y - 1}
            ],
          NPos = [{C + 1, est(NP, Map), NP} || NP <- NPs],
          Fs = [NP || {_, E, _} = NP <- NPos, E >= 0],
          FQ = lists:foldl(fun gb_sets:add_element/2, NQ, Fs),
          walk(FQ, Map, NV)
      end
  end.

solve_second(Input) ->
  solve_second(1, Input).

solve_second(L, Input) ->
  I = lists:sublist(Input, L),
  case walk(I) of
    false ->
      [H|_] = lists:reverse(I),
      H;
    _ ->solve_second(L + 1, Input)
  end.
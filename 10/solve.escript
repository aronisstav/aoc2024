#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/10

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
          fun(C, {I, #{s := S, mx := MX} = Map}) ->
              Pos = {I, J},
              NMap =
                case C of
                  $0 -> Map#{s => [Pos|S]};
                  _ -> Map
                end,
              {I + 1, NMap#{Pos => C - $0, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{s => [], mx => 0}}, Lines).

solve_first({_, #{s := Ss} = Map}) ->
  Fold = fun(S, Acc) -> Acc + score(S, Map) end,
  lists:foldl(Fold, 0, Ss).

score(P, Map) ->
  score(queue:from_list([P]), Map, #{}, #{}).

score(Queue, Map, Visit, Goals) ->
  {Val, NQ} = queue:out(Queue),
  case Val of
    empty -> maps:size(Goals);
    {value, {X, Y} = Pos} ->
      V = maps:get(Pos, Map),
      if V == 9 -> score(NQ, Map, Visit, Goals#{Pos => x});
         true ->
          Ds = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
          Ns = [{X + DX, Y + DY} || {DX, DY} <- Ds],
          NVs = [{N, maps:get(N, Map, x), maps:get(N, Visit, x)} || N <- Ns],
          Cs = [N || {N, NV, NVis} <- NVs, NV == V + 1, NVis == x],
          Fold = fun(N, {QQ, NVis}) -> {queue:in(N, QQ), NVis#{N => o}} end,
          {FQ, FVisit} = lists:foldl(Fold, {NQ, Visit}, Cs),
          score(FQ, Map, FVisit, Goals)
      end
  end.                      

solve_second({_, #{s := Ss} = Map}) ->
  Fold = fun(S, Acc) -> Acc + score2(S, Map) end,
  lists:foldl(Fold, 0, Ss).

score2(P, Map) ->
  {S, _} = score2(P, Map, #{}),
  S.

score2({X, Y} = Pos, Map, Memo) ->
  case maps:get(Pos, Memo, x) of
    x ->
      V = maps:get(Pos, Map),
      if V == 9 -> {1, Memo#{Pos => 1}};
         true ->
          Ds = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
          Ns = [{X + DX, Y + DY} || {DX, DY} <- Ds],
          NVs = [{N, maps:get(N, Map, x)} || N <- Ns],
          Cs = [N || {N, NV} <- NVs, NV == V + 1],
          Fold =
            fun(N, {Acc, NMem}) ->
                {S, Mem} = score2(N, Map, NMem),
                {S + Acc, Mem}
            end,
          {FS, FMem} = lists:foldl(Fold, {0, Memo}, Cs),
          {FS, FMem#{Pos => FS}}
      end;
    V -> {V, Memo}
  end.

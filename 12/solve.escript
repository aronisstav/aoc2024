#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/12

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
              Pos = {I, J},
              {I + 1, Map#{Pos => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{s => [], mx => 0}}, Lines).

solve_first({MY, Map}) ->
  {Regions, _} = regionify(Map#{my => MY - 1}),
  Fold = fun(R, Acc) -> Acc + score(R) end,
  lists:foldl(Fold, 0, Regions).

regionify(#{mx := MX, my := MY} = Map) ->
  Fold =
    fun(Pos, {Regions, Covered}) ->
        case maps:get(Pos, Covered, x) of
          x ->
            C = maps:get(Pos, Map),
            NR = region(queue:from_list([Pos]), C, Map, #{Pos => C}),
            {[NR|Regions], maps:merge(Covered, NR)};
          _ -> {Regions, Covered}
        end
    end,
  Pos = [{X, Y} || X <- lists:seq(1, MX), Y <- lists:seq(1, MY)],
  lists:foldl(Fold, {[], #{}}, Pos).

region(Queue, C, Map, Region) ->
  {Out, NQ} = queue:out(Queue),
  case Out of
    empty -> Region;
    {value, {X, Y}} ->
      Ds = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
      Ns = [{X + DX, Y + DY} || {DX, DY} <- Ds],
      Is = [{Pos, maps:get(Pos, Map, x), maps:get(Pos, Region, x)} || Pos <- Ns],
      Cs = [Pos || {Pos, CC, RR} <- Is, CC =:= C, RR =:= x],
      FQ = lists:foldl(fun queue:in/2, NQ, Cs),
      FR = lists:foldl(fun(P, R) -> R#{P => C} end, Region, Cs),
      region(FQ, C, Map, FR)
  end.

score(Region) ->
  Es = maps:keys(Region),
  Area = maps:size(Region),
  Fold =
    fun({X, Y}, P) ->
        Ds = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
        Ns = [{X + DX, Y + DY} || {DX, DY} <- Ds],
        Is = [maps:get(Pos, Region, x) || Pos <- Ns],
        Cs = [v || RR <- Is, RR =:= x],
        P + length(Cs)
    end,
  P = lists:foldl(Fold, 0, Es),
  P * Area.

solve_second({MY, Map}) ->
  {Regions, _} = regionify(Map#{my => MY - 1}),
  Fold = fun(R, Acc) -> Acc + score2(R) end,
  lists:foldl(Fold, 0, Regions).

score2(Region) ->
  Area = maps:size(Region),
  Planks = to_planks(Region),
  Sides = sides(Planks),
  Area * Sides.

to_planks(Region) ->
  Es = maps:keys(Region),
  Fold =
    fun({X, Y} = K, P) ->
        Ds = [{1, 0, r}, {-1, 0, l}, {0, 1, d}, {0, -1, u}],
        Ns = [{{X + DX, Y + DY}, Dir} || {DX, DY, Dir} <- Ds],
        Is = [{Dir, maps:get(Pos, Region, x)} || {Pos, Dir} <- Ns],
        Cs = [{K, Dir} || {Dir, RR} <- Is, RR =:= x],
        Fold = fun(XX, M) -> M#{XX => o} end,
        lists:foldl(Fold, P, Cs)
    end,
  lists:foldl(Fold, #{}, Es).

sides(Planks) ->
  %% The sort is important, to "follow" edges correctly.
  Es = lists:sort(maps:keys(Planks)),
  Fold =
    fun({{X, Y}, Dir} = K, {Pl, Sd}) ->
        NSd =
          case maps:get(K, Pl, x) of
            x -> Sd;
            o -> Sd#{K => o}
          end,
        Ds =
          case Dir of
            H when H =:= r; H =:= l -> [{0, 1}, {0, -1}];
            V when V =:= u; V =:= d -> [{1, 0}, {-1, 0}]
          end,
        Ns = [{{X + DX, Y + DY}, Dir} || {DX, DY} <- Ds],
        Fold2 = fun(XX, M) -> maps:remove(XX, M) end,
        {lists:foldl(Fold2, Pl, [K|Ns]), NSd}
    end,
  {#{}, Sd} = lists:foldl(Fold, {Planks, #{}}, Es),
  maps:size(Sd).

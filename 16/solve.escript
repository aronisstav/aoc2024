#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/16

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
                  $E -> Map#{g => {I, J}};
                  _ -> Map
                end,
              {I + 1, NMap#{{I, J} => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first({_, #{s := S} = Map}) ->
  E = est(S, Map),
  LQ = gb_sets:from_list([{0, E, S, r}]),
  walk(LQ, Map, #{}, best).

est({X, Y}, #{g := {A, B}}) ->
  abs(X - A) + abs(Y - B).

walk(Q, Map, V, Mode) ->
  {{C, _, {X, Y} = Pos, D}, NQ} = gb_sets:take_smallest(Q),
  E = est(Pos, Map),
  case {E, Mode} of
    {0, best} -> C;
    {_, {Best, Paths}} when Best < C ->
      maps:size(maps:get({Best, g}, Paths));
    {0, {_, Paths}} ->
      NG = maps:get({C, Pos, D}, Paths),
      Old = maps:get({C, g}, Paths, #{}),
      walk(NQ, Map, V, {C, Paths#{{C, g} => maps:merge(Old, NG)}});
    _ ->
      VKey = {Pos, D},
      case
        (maps:get(Pos, Map) =:= $#) orelse
        (maps:get(VKey, V, infinity) < C)
      of
        true -> walk(NQ, Map, V, Mode);
        false ->
          NV = V#{VKey => C},
          {NPos, D1, D2} =
            case D of
              r -> {{X + 1, Y}, u, d};
              l -> {{X - 1, Y}, d, u};
              u -> {{X, Y - 1}, l, r};
              d -> {{X, Y + 1}, r, l}
            end,
          Es =
            [ {C +    1, E, NPos,  D}
            , {C + 1000, E,  Pos, D1}
            , {C + 1000, E,  Pos, D2}
            ],
          FQ = lists:foldl(fun gb_sets:add_element/2, NQ, Es),
          NMode =
            case Mode of
              best -> best;
              {Best, Paths} ->
                PKey = {C, Pos, D},
                OP = maps:get(PKey, Paths),
                Fold =
                  fun({NC, _, PPos, PDir}, Acc) ->
                      NPKey = {NC, PPos, PDir},
                      Old = maps:get(NPKey, Paths, #{}),
                      Acc#{NPKey => maps:merge(Old, OP#{PPos => true})}
                  end,
                NPaths = lists:foldl(Fold, Paths, Es),
                {Best, NPaths}
            end,
          walk(FQ, Map, NV, NMode)
      end
  end.

solve_second({_, #{s := S} = Map}) ->
  E = est(S, Map),
  LQ = gb_sets:from_list([{0, E, S, r}]),
  walk(LQ, Map, #{}, {infinity, #{{0, S, r} => #{S => true}}}).

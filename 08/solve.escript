#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/8

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
          fun(C, {I, #{as := As, mx := MX} = Map}) ->
              P = {I, J},
              NMap =
                case C of
                  $. -> Map;
                  E ->
                    Es = maps:get(E, As, []),
                    Map#{as => As#{E => [P|Es]}}
                end,
              {I + 1, NMap#{{I, J} => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0, as => #{}}}, Lines).

solve_first({_, #{as := AS} = Map}) ->
  Fold = fun({_, Ps}, Acc) -> nd(Ps, Map, Acc) end,
  maps:size(lists:foldl(Fold, #{}, maps:to_list(AS))).

nd(Ps, Map, Acc0) ->
  Pairs = [{P, Q} || P <- Ps, Q <- Ps, P < Q],
  Fold =
    fun({{PX, PY}, {QX, QY}}, Acc) ->
        {DX, DY} = {QX - PX, QY - PY},
        N1 = {PX - DX, PY - DY},
        N2 = {QX + DX, QY + DY},
        M1 = maps:get(N1, Map, x),
        M2 = maps:get(N2, Map, x),
        Add =
          case {M1, M2} of
            {x, x} -> Acc;
            {x, _} -> Acc#{N2 => x};
            {_, x} -> Acc#{N1 => x};
            {_, _} -> Acc#{N1 => x, N2 => x}
          end,
        Add
    end,
  lists:foldl(Fold, Acc0, Pairs).

solve_second({_, #{as := AS} = Map}) ->
  Fold = fun({_, Ps}, Acc) -> ln(Ps, Map, Acc) end,
  maps:size(lists:foldl(Fold, #{}, maps:to_list(AS))).

ln(Ps, Map, Acc0) ->
  Pairs = [{P, Q} || P <- Ps, Q <- Ps, P < Q],
  Fold =
    fun({{PX, PY}, {QX, QY}}, Acc) ->
        {DX0, DY0} = {QX - PX, QY - PY},
        GCD = gcd(abs(DX0), abs(DY0)),
        {DX, DY} = {DX0 div GCD, DY0 div GCD},
        Acc1 = ln({PX, PY}, {-DX, -DY}, Map, Acc),
        ln({PX, PY}, {DX, DY}, Map, Acc1)
    end,
  lists:foldl(Fold, Acc0, Pairs).

ln({X, Y} = P, {DX, DY}, Map, Acc) ->
  case maps:get(P, Map, x) of
    x -> Acc;
    _ ->
      NAcc = Acc#{P => x},
      ln({X + DX, Y + DY}, {DX, DY}, Map, NAcc)
  end.

gcd(0, _) -> 0;
gcd(_, 0) -> 0;
gcd(M, N) when M rem N == 0 -> N;
gcd(M, N) -> gcd(N, M rem N).

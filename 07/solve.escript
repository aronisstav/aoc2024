#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/7

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
  read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res ->
      Ns = re:split(Res, "[\:\ \n]", [{return, list}]),
      [F|R] = [list_to_integer(N) || N <- Ns, N =/= ""],
      read_lines([{F, R}|Acc])
  end.

solve_first(Input) ->
  Fold = fun(I, Acc) -> Acc + sums(I, false) end,
  lists:foldl(Fold, 0, Input).

sums({G, [H|T]}, Concat) ->
  sums(H, G, T, Concat).

sums(G, G, [], _) -> G;
sums(_, _, [], _) -> 0;
sums(C, G, _, _) when C > G -> 0;
sums(C, G, [H|T], Concat) ->
  CR =
    case Concat of
      false -> 0;
      true ->
        NC = list_to_integer(integer_to_list(C) ++ integer_to_list(H)),
        sums(NC, G, T, Concat)
    end,
  case {sums(C + H, G, T, Concat), sums(C * H, G, T, Concat), CR} of
    {G, _, _} -> G;
    {_, G, _} -> G;
    {_, _, G} -> G;
    _ -> 0
  end.

solve_second(Input) ->
  Fold = fun(I, Acc) -> Acc + sums(I, true) end,
  lists:foldl(Fold, 0, Input).

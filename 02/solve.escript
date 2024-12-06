#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/2

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
  Lines = read_lines(),
  Map =
    fun(Line) -> [list_to_integer(I) || I <- string:split(Line, " ", all)] end,
  [Map(L) || L <- Lines].

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res ->
      [T|_] = string:split(Res, "\n"),
      read_lines([T|Acc])
  end.

solve_first(Input) ->
  lists:sum([is_safe(L) || L <- Input]).

is_safe([A|[B|_]] = Input) ->
  if A > B -> is_safe(Input, desc);
     A < B -> is_safe(Input, asc);
     true -> 0
  end.

is_safe([_], _) -> 1;
is_safe([A|[B|_] = T], Dir) ->
  Abs = abs(A - B),
  InDir =
    case Dir of
      asc when A < B -> true;
      desc when A > B -> true;
      _ -> false
    end,
  case InDir andalso Abs < 4 of
    true -> is_safe(T, Dir);
    false -> 0
  end.

solve_second(Input) ->
  lists:sum([is_any_safe(L) || L <- Input]).

is_any_safe(L) ->
  case is_safe(L) of
    1 -> 1;
    0 -> is_any_safe(L, length(L))
  end.

is_any_safe(_, 0) -> 0;
is_any_safe(L, N) ->
  {Pre, [_|R]} = lists:split(N - 1, L),
  case is_safe(Pre ++ R) of
    1 -> 1;
    0 -> is_any_safe(L, N - 1)
  end.

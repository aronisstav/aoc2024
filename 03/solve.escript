#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/3

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
  Ls = read_lines(),
  lists:append(Ls).

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res ->
      [T|_] = string:split(Res, "\n"),
      read_lines([T|Acc])
  end.

solve_first(Input) ->
  muls(Input).

muls(L) ->
  case re:run(L,"mul\\((?P<F>\\d+),(?P<S>\\d+)\\)",[{capture,all_names,list},global]) of
    {match, Matches} ->
      Fold = fun([A,B], Acc) -> Acc + list_to_integer(A) * list_to_integer(B) end,
      lists:foldl(Fold, 0, Matches);
    _ -> 0
  end.

solve_second(Input) ->
  solve_second(0, Input).

solve_second(Acc, Input) ->
  case string:split(Input, "don't()") of
    [UntilDont, Dont] ->
      NAcc = Acc + muls(UntilDont),
      case string:split(Dont, "do()") of
        [_, Rest] -> solve_second(NAcc, Rest);
        _ -> NAcc
      end;
    [Rest] -> Acc + muls(Rest)
  end.

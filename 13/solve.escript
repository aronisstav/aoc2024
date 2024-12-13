#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/13

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
  Pat =
    "Button A: X+~d, Y+~d\n"
    "Button B: X+~d, Y+~d\n"
    "Prize: X=~d, Y=~d\n",
  read_many(Pat).

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  lists:sum([score2(I, 0) || I <- Input]).

solve_second(Input) ->
  lists:sum([score2(I, 10000000000000) || I <- Input]).

score2([XA, YA, XB, YB, XS, YS], O) ->
  X = XS + O,
  Y = YS + O,
  D = XA * YB - YA * XB,
  case D =/= 0 of
    false -> 0;
    true ->
      DA = X * YB - Y * XB,
      DB = XA * Y - YA * X,
      case {DA rem D, DB rem D} of
        {0, 0} ->
          A = DA div D,
          B = DB div D,
          3 * A + B;
        _ -> 0
      end
  end.

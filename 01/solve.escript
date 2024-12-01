#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/1

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
  Pairs = read_many("~d ~d"),
  Fold = fun([A, B], {AA, BB}) -> {[A|AA], [B|BB]} end,
  lists:foldl(Fold, {[], []}, Pairs).

read(Pat) ->
  io:fread("", Pat).

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

solve_first({AA, BB}) ->
  Zip = fun(A, B) -> abs(A - B) end,
  lists:sum(lists:zipwith(Zip, lists:sort(AA), lists:sort(BB))).

solve_second({AA, BB}) ->
  Fold =
    fun(X, M) ->
        V = maps:get(X, M, 0),
        M#{X => V + 1}
    end,
  CC = lists:foldl(Fold, #{}, BB),
  Fold2 =
    fun(X, Acc) ->
        V = maps:get(X, CC, 0),
        Acc + X * V
    end,
  lists:foldl(Fold2, 0, AA).

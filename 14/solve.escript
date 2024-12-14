#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/14

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
  read_many("p=~d,~d v=~d,~d").

read_many(Pat) ->
  Is = read_many(Pat, []),
  Fold = fun([A,B,C,D], {I, Acc}) -> {I + 1, Acc#{I => {{A,B},{C,D}}}} end,
  {_, M} = lists:foldl(Fold, {0, #{}}, Is),
  M.

read_many(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  Ms = {101, 103},
  S = 100,
  FM = simulate(S, Input, Ms),
  score(FM, Ms).

simulate(0, M, _) -> M;
simulate(N, M, {MX, MY}) ->
  Fold =
    fun(K, {{X, Y}, {DX, DY}}, AM) ->
        AM#{K => {{(X + MX + DX) rem MX, (Y + MY + DY) rem MY}, {DX, DY}}}
    end,
  NM = maps:fold(Fold, #{}, M),
  simulate(N - 1, NM, {MX, MY}).

score(M, {MX, MY}) ->
  {DX, DY} = {MX div 2, MY div 2},
  Fold =
    fun(_, {{X, Y}, _}, {Q1, Q2, Q3, Q4} = Qs) ->
        case X == DX orelse Y == DY of
          true -> Qs;
          false ->
            case {X < DX, Y < DY} of
              {false, false} -> {Q1 + 1, Q2, Q3, Q4};
              {false,  true} -> {Q1, Q2 + 1, Q3, Q4};
              { true, false} -> {Q1, Q2, Q3 + 1, Q4};
              { true,  true} -> {Q1, Q2, Q3, Q4 + 1}
            end
        end
    end,
  {Q1, Q2, Q3, Q4} = maps:fold(Fold, {0, 0, 0, 0}, M),
  Q1 * Q2 * Q3 * Q4.

solve_second(Input) ->
  Ms = {101, 103},
  Skip = 5000,
  M = simulate(Skip, Input, Ms),
  play(Skip, M, Ms).

play(N, M, Ms) ->
  %%io:format("\033[2J"),
  io:format("~n~p~n", [N]),
  print(M, Ms),
  NM = simulate(1, M, Ms),
  timer:sleep(150),
  play(N + 1, NM, Ms).

print(M, {MX, MY}) ->
  Fold = fun(_, {Pos, _}, Acc) -> Acc#{Pos => $*} end,
  Grid = maps:fold(Fold, #{}, M),
  For1 =
    fun(Y) ->
        S = [maps:get({X, Y}, Grid, $ ) || X <- lists:seq(0, MX - 1)],
        io:format("~s~n", [S])
    end,
  lists:foreach(For1, lists:seq(0, MY - 1)).

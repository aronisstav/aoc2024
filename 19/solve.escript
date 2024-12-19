#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/19

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
  Res = io:get_line(""),
  [STs|_] = string:split(Res, "\n"),
  Ts = lists:sort(string:split(STs, ", ", all)),
  Pats = read_many("~s"),
  {Ts, lists:append(Pats)}.

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

compactify(Ts) ->
  LTs = [{length(T), T} || T <- Ts],
  compactify(1, LTs, []).

compactify(_, [], Acc) -> Acc;
compactify(N, Pats, Acc) ->
  Pred = fun({X, _}) -> X == N end,
  {New, Pend} = lists:partition(Pred, Pats),
  NAcc = lists:sort(Acc ++ [P || {_, P} <- New]),
  NPats = [LP || {_, P} = LP <- Pend, pos(P, NAcc, NAcc) =:= 0],
  compactify(N + 1, NPats, NAcc).

solve_first({PTs, Ps}) ->
  Ts = compactify(PTs),
  io:format("~p~n",[Ts]),
  Fold =
    fun(P, {N, Acc}) ->
      I = pos(P, Ts, Ts),
      io:format("~p~n",[{N, I}]),
      {N + 1, Acc + I} 
    end,
  {_, N} = lists:foldl(Fold, {1, 0}, Ps),
  N.

pos([], _, _) -> 1;
pos(_, [], _) -> 0;
pos(Pat, [T|E], Ts) when T =< Pat ->
  case string:prefix(Pat, T) of
    nomatch -> pos(Pat, E, Ts);
    NPat ->
      case pos(NPat, Ts, Ts) of
        0 -> pos(Pat, E, Ts);
        1 -> 1
      end
  end;
pos(_, _, _) -> 0.

solve_second({PTs, Ps}) ->
  M0 = maps:from_list([{{0, P}, 1} || P <- PTs]),
  Fold =
    fun(P, {Acc, Memo}) ->
      {I, NMemo} = cut(P, Memo),
      {Acc + I, NMemo}
    end,
  {N, _Memo} = lists:foldl(Fold, {0, M0}, Ps),
  N.

cut({0, _} = LP, M0) -> {maps:get(LP, M0, 0), M0};
cut(P, M0) ->
  M = maps:get(P, M0, x),
  case M =:= x of
    false -> {M, M0};
    true ->
      {Less, M1} = cut({0, P}, M0),
      Fold =
        fun(N, {Acc, MM}) ->
          {Pre, Post} = lists:split(N, P),
          {NN1, MM1} = cut({0, Pre}, MM),
          {NN2, MM2} = cut(Post, MM1),
          {Acc + NN1 * NN2, MM2}
        end,
      {V, M2} = lists:foldl(Fold, {Less, M1}, lists:seq(1, length(P) - 1)),
      {V, M2#{P => V}}
  end.
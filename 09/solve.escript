#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/9

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
  {ok, [Map]} = read("~s"),
  [N - $0 || N <- Map].

read(Pat) ->
  io:fread("", Pat).

solve_first(I) ->
  E = expand(I, 0, 0, false, array:new()),
  C = compress(E),
  checksum(0, C, 0).

expand([], N, _, _, A) -> {N, A};
expand([H|T], I, Id, Free, A0) ->
  {V, NFree, NId} =
    case Free of
      true -> {-1, false, Id};
      false -> {Id, true, Id + 1}
    end,
  Fold = fun(P, A) -> array:set(P, V, A) end,
  NA = lists:foldl(Fold, A0, lists:seq(I, I + H - 1)),
  expand(T, I + H, NId, NFree, NA).

compress({N, X}) ->
  compress(0, N - 1, X).

compress(A, Z, X) when A >= Z -> X;
compress(A, Z, X) ->
  I = find_x(A, X),
  {J, V} = find_v(Z, X),
  NX = array:set(I, V, array:set(J, -1, X)),
  compress(I + 1, J - 1, NX).

find_x(I, X) ->
  case array:get(I, X) of
    -1 -> I;
    _ -> find_x(I + 1, X)
  end.

find_v(J, X) ->
  case array:get(J, X) of
    -1 -> find_v(J - 1, X);
    V -> {J, V}
  end.

checksum(P, X, Acc) ->
  case array:get(P, X) of
    -1 -> Acc;
    V -> checksum(P + 1, X, Acc + P * V)
  end.

solve_second(I) ->
  E = expand2(I, 0, 0, false, array:new([{default, x}])),
  C = compress2(E),
  checksum2(0, C, 0).

expand2([], N, _, _, A) -> {N, A};
expand2([H|T], I, Id, Free, A0) ->
  {V, NFree, NId} =
    case Free of
      true -> {-H, false, Id};
      false -> {{Id, H}, true, Id + 1}
    end,
  Fold = fun(P, A) -> array:set(P, V, A) end,
  NA = lists:foldl(Fold, A0, lists:seq(I, I + H - 1)),
  expand2(T, I + H, NId, NFree, NA).

compress2({N, X}) ->
  compress2(N - 1, X).

compress2(-1, X) -> X;
compress2(Z, X) ->
  {J, V, L} = find_v2(Z, x, X),
  NX = try_move(L, V, J, X),
  compress2(J - 1, NX).

find_v2(-1, {V, L}, _) -> {0, V, L};
find_v2(J, P, X) ->
  case array:get(J, X) of
    N when P == x, N < 0 -> find_v2(J - 1, P, X);
    V when P == x -> find_v2(J - 1, V, X);
    P -> find_v2(J - 1, P, X);
    _ ->
      {I, L} = P,
      {J + 1, I, L}
  end.

try_move(L, V, J, X) ->
  case find_i(0, -L, J, X) of
    {true, I, G} ->
      Ws =
        [{K, {V, L}} || K <- lists:seq(I, I + L - 1)] ++
        [{K, -(G - L)} || K <- lists:seq(I + L, I + G - 1)] ++
        [{K, -1} || K <- lists:seq(J, J + L - 1)],
      Fold = fun({P, W}, AX) -> array:set(P, W, AX) end,
      lists:foldl(Fold, X, Ws);
    false -> X
  end.

find_i(I, _L, J, _X) when I >= J -> false;
find_i(I, L, J, X) ->
  case array:get(I, X) of
    G when G < 0 ->
      case L >= G of
        true -> {true, I, -G};
        false -> find_i(I - G, L, J, X)
      end;
    {_, P} -> find_i(I + P, L, J, X)
  end.

checksum2(I, X, Acc) ->
  case array:get(I, X) of
    x -> Acc;
    {V, _} -> checksum2(I + 1, X, Acc + I * V);
    _ -> checksum2(I + 1, X, Acc)
  end.

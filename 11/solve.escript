#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/11

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
  lists:append(read_many("~d")).

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

solve_first(I) ->
  F = blink(25, I),
  msize(F).

blink(0, I) -> I;
blink(N, I) ->
  blink(N - 1, blink(I)).

blink(I) when is_list(I) -> [blink(E) || E <- I];
blink({L, R}) -> {blink(L), blink(R)};
blink(0) -> 1;
blink(N) ->
  L = trunc(math:ceil(math:log10(N + 1))),
  case L rem 2 of
    1 -> N * 2024;
    0 ->
      P = L div 2,
      B = trunc(math:pow(10, P)),
      {N div B, N rem B}
  end.

msize(I) when is_list(I) -> lists:sum([msize(E) || E <- I]);
msize({L, R}) -> msize(L) + msize(R);
msize(_) -> 1.

solve_second(I) ->
  Fold =
    fun(S, {Acc, Mem}) ->
        {V, NMem} = blink2({S, 75}, Mem),
        {V + Acc, NMem}
    end,
  {S, _} = lists:foldl(Fold, {0, #{}}, I),
  S.

blink2({_, 0}, Mem) -> {1, Mem};
blink2({N, Gen} = S, Mem) ->
  case maps:get(S, Mem, x) of
    x ->
      {V, NMem} =
        case N == 0 of
          true -> blink2({1, Gen - 1}, Mem);
          false ->
            L = trunc(math:ceil(math:log10(N + 1))),
            case L rem 2 of
              1 -> blink2({N * 2024, Gen - 1}, Mem);
              0 ->
                P = L div 2,
                B = trunc(math:pow(10, P)),
                {V1, NMem1} = blink2({N div B, Gen - 1}, Mem),
                {V2, NMem2} = blink2({N rem B, Gen - 1}, NMem1),
                {V1 + V2, NMem2}
            end
        end,
      {V, NMem#{S => V}};
    V -> {V, Mem}
  end.

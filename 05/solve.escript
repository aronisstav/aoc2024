#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/5

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
  read_lines(1, #{}, #{}).

read_lines(1, Graph, Input) ->
  case io:get_line("") of
    "\n" ->
      read_lines(2, [], Input#{graph => Graph});
    Res ->
      [T|_] = string:split(Res, "\n"),
      [N1, N2] = [list_to_integer(N) || N <- string:split(T, "|")],
      Es = maps:get(N1, Graph, []),
      read_lines(1, Graph#{N1 => [N2|Es]}, Input)
  end;
read_lines(2, Acc, Input) ->
  case io:get_line("") of
    eof -> Input#{upd => lists:reverse(Acc)};
    Res ->
      [T|_] = string:split(Res, "\n"),
      Ns = [list_to_integer(N) || N <- string:split(T, ",", all)],
      read_lines(2, [Ns|Acc], Input)
  end.

solve_first(#{graph := Graph, upd := Upds}) ->
  Fold = fun(Upd, Acc) -> Acc + mid_if(fun is_ok/2, Upd, Graph) end,
  lists:foldl(Fold, 0, Upds).

mid_if(Fun, Upd, Graph) ->
  case Fun(Upd, Graph) of
    false -> 0;
    {true, NUpd} ->
      L = length(NUpd),
      {_,[N|_]} = lists:split(L div 2, NUpd),
      N
  end.

is_ok([H|T] = Upd, Graph) ->
  case is_ok(H, T, Graph) of
    false -> false;
    true -> {true, Upd}
  end.      

is_ok(_, [], _) -> true;
is_ok(F, [S|T], Graph) ->
  Ns = maps:get(S, Graph, []),
  case not lists:member(F, Ns) of
    true -> is_ok(S, T, Graph);
    false -> false
  end.

solve_second(#{graph := Graph, upd := Upds}) ->
  Fold = fun(Upd, Acc) -> Acc + mid_if(fun is_not_ok/2, Upd, Graph) end,
  lists:foldl(Fold, 0, Upds).

is_not_ok(Upd, Graph) ->
  Sort =
    fun(F, S) ->
        Ns = maps:get(F, Graph, []),
        lists:member(S, Ns)
    end,
  case lists:sort(Sort, Upd) of
    Upd -> false;
    Srt -> {true, Srt}
  end.

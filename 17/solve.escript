#!/usr/bin/env escript

%% https://adventofcode.com/2024/day/17

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
    "Register A: ~d\n"
    "Register B: ~d\n"
    "Register C: ~d\n"
    "\n"
    "Program: ~s\n",
  {ok, [A, B, C, PS]} = io:fread("", Pat),
  P = [list_to_integer(E) || E <- string:split(PS, ",", all)],
  {A, B, C, P}.

solve_first(Input) ->
  run(load(Input)).

load({A, B, C, P}) ->
  Fold = fun(Op, {N, Acc}) -> {N + 1, [{N,Op}|Acc]} end,
  {_, PL} = lists:foldl(Fold, {0, []}, P),
  #{ a => A
   , b => B
   , c => C
   , pc => 0
   , prog => maps:from_list(PL)
   , out => []
   , goal => lists:reverse(P)
   }.

run(State) ->
  #{ a := A
   , b := B
   , c := C
   , pc := PC
   , prog := Prog
   , out := Out
   } = State,
  case maps:get(PC, Prog, x) of
    x -> Out;
    OpC ->
      OpCombo = maps:get(PC + 1, Prog),
      Combo =
        case OpCombo of
          4 -> A;
          5 -> B;
          6 -> C;
          E -> E
        end,
      NState = State#{pc => PC + 2},
      FState =
        case OpC of
          DV when DV == 0; DV == 6; DV == 7 ->
            Den = trunc(math:pow(2, Combo)),
            Res = A div Den,
            case DV of
              0 -> NState#{a => Res};
              6 -> NState#{b => Res};
              7 -> NState#{c => Res}
            end;
          1 ->
            Res = B bxor OpCombo,
            NState#{b => Res};
          2 ->
            Res = Combo rem 8,
            NState#{b => Res};
          3 ->
            case A == 0 of
              true -> NState;
              false -> State#{pc => OpCombo}
            end;
          4 ->
            Res = B bxor C,
            NState#{b => Res};
          5 ->
            NState#{out => [(Combo rem 8)|Out]}
        end,
      run(FState)
  end.

solve_second(Input) ->
  State = load(Input),
  search(0, 0, State).

search(8, _, _) -> false;
search(N, Acc, #{goal := G} = State) ->
  A = 8 * Acc + N,
  O = run(State#{a => A}),
  case O =:= G of
    true -> A;
    false ->
      case lists:prefix(O, G) of
        true ->
          case search(0, A, State) of
            false -> search(N + 1, Acc, State);
            Else -> Else
          end;
        false ->
          search(N + 1, Acc, State)
      end
  end.
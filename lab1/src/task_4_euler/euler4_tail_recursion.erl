-module(euler4_tail_recursion).
-author("DmitryL").

%% API
-export([
  solution/0
]).


solution() -> largest_pal_tail_rec(999, 999, 0).

is_pal(Num) ->
  StrNum = integer_to_list(Num),
  StrNum =:= lists:reverse(Num).


get_new_max(Product, Max) ->
  case is_pal(Product) of
    true -> max(Product, Max);
    _ -> Max
  end.


largest_pal_tail_rec(I, _, Max) when I < 100 -> Max;
largest_pal_tail_rec(I, J, Max) when J < 100 -> largest_pal_tail_rec(I - 1, 999, Max);
largest_pal_tail_rec(I, J, Max) ->
  Product = I * J,
  NewMax = get_new_max(Product, Max),
  largest_pal_tail_rec(I, J - 1, NewMax).
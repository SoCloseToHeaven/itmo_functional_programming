-module(euler4_recursion).
-author("DmitryL").

%% API
-export([
  solution/0
]).


solution() -> largest_pal_rec(999, 999).

is_pal(Num) ->
  StrNum = integer_to_list(Num),
  StrNum =:= lists:reverse(StrNum).


get_new_max(Product, Max) ->
  case is_pal(Product) of
    true -> max(Product, Max);
    _ -> Max
  end.


largest_pal_rec(I, _) when I < 100 -> 0;
largest_pal_rec(I, J) when J < 100 -> largest_pal_rec(I - 1, 999);
largest_pal_rec(I, J) ->
  Product = I * J,
  get_new_max(Product, largest_pal_rec(I, J - 1)).
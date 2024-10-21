-module(euler4_map).
-author("DmitryL").

%% API
-export([
  solution/0
]).

solution() ->
  Products = generate_products_via_map(100, 999),
  find_max_pal(Products).

is_pal(Num) ->
  StrNum = integer_to_list(Num),
  StrNum =:= lists:reverse(Num).

%% Генерация последовательности при помощи отображения
generate_products_via_map(Min, Max) ->
  lists:flatmap(fun(I) ->
    lists:map(fun(J) -> I * J end, lists:seq(Min, Max))
                end, lists:seq(Min, Max)).

find_max_pal(Products) -> lists:max(lists:filter(fun(X) -> is_pal(X) end, Products)).
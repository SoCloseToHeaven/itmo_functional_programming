-module(euler4_modular).
-author("DmitryL").

%% API
-export([
  solution/0
]).

solution() ->
  Products = generate_products(),
  Pals = filter_pals(Products),
  find_max(Pals).

is_pal(Num) ->
  StrNum = integer_to_list(Num),
  StrNum =:= lists:reverse(Num).

%% Генерация последовательности
generate_products() -> [I * J || I <- lists:seq(100, 999), J <- lists:seq(100, 999)].

%% Фильтрация последовательности
filter_pals(Products) -> lists:filter(fun(X) -> is_pal(X) end, Products).

%% Поиск максимума в последовательности
find_max(Products) -> lists:max(Products).

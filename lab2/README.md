# Лабораторная работа по функциональному программированию №2
## Вариант: rb-dict-lazy

---

* Студент: `Лянгузов Дмитрий Максимович`
* Группа: `P3332`
* ИСУ: `368459`
* Функциональный язык: `Erlang`

---

## Требования

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая); (В данном случае только одна свертка от auxiliary element)
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

--- 

## Ключевые элементы реализации

Добавление и удаление элементов:

```erlang
%% store(Key, Val, Dict) -> Dict.

store(K, V, T) ->
    {_, L, K1, V1, R} = store1(K, V, T),
    %setelement(1, b, T1).
    {b, L, K1, V1, R}.

store1(K, V, empty) ->
    {r, empty, K, V, empty};
store1(K, V, {C, Left, K1, V1, Right}) when K < K1 ->
    lbalance(C, store1(K, V, Left), K1, V1, Right);
store1(K, V, {C, Left, K1, V1, Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, store1(K, V, Right));
store1(K, V, {C, L, _, _, R}) ->
    {C, L, K, V, R}.

%% erase(Key, Dict) -> Dict.

erase(K, T) ->
  {T1, _} = erase_aux(K, T),
  T1.

```

Фильтрация (filter):

```erlang
filter(F, T) -> filter(F, T, new()).


filter(_, empty, New) ->
    New;
filter(F, {_, A, Xk, Xv, B}, New0) ->
    New1 = filter(F, A, New0),
    New2 =
        case F(Xk, Xv) of
            true -> store(Xk, Xv, New1);
            false -> New1
        end,
    filter(F, B, New2).
```

Отображение (map):

```erlang
%% map(Fun, Dict) -> Dict.

map(_, empty) -> empty;
map(F, {RB, A, Xk, Xv, B}) -> {RB, map(F, A), Xk, F(Xk, Xv), map(F, B)}.
```

Свертка:
```erlang
%% fold(Fun, Acc, Dict) -> Acc.

fold(_, Acc, empty) -> Acc;
fold(F, Acc, {_, A, Xk, Xv, B}) -> fold(F, F(Xk, Xv, fold(F, Acc, B)), A).
```

Слияние:

```erlang

%% merge(Fun, Dict, Dict) -> Dict.

merge(F, D1, D2) ->
    fold(
        fun(K, V2, D) ->
            update(K, fun(V1) -> F(K, V1, V2) end, V2, D)
        end,
        D1,
        D2
    ).
```

## Эмуляция ленивых вычислений
```erlang
-define(SCHEDULE_OPERATION(Fun, Pid), Pid ! {Fun, self()}).

new(Dict) -> spawn(fun() -> loop({Dict, []}) end).

current(Pid) ->
    Pid ! {force, self()},
    receive
        {Dict, Pid} -> Dict
    end.

current_lazy(Pid) ->
    Pid ! {lazy, self()},
    receive
        {Dict, Pid} -> Dict
    end.

loop({Dict, Operations} = State) ->
    NewState =
        receive
            {Operation, From} when is_function(Operation) andalso is_pid(From) ->
                {Dict, [Operation | Operations]};
            {force, From} ->
                NewDict = force(Dict, Operations),
                From ! {NewDict, self()},
                {NewDict, []};
            {lazy, From} ->
                From ! {Dict, self()},
                State
        end,
    loop(NewState).

force(Dict, []) -> Dict;
force(Dict, [Operation | T]) -> force(Operation(Dict), T).

store(K, V, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:store(K, V, Dict) end, Pid).

filter(Fun, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:filter(Fun, Dict) end, Pid).

%% And so on...
```

## Соответствие свойству моноида

- Определен нейтральный элемент - rbdict:new()

- Определена бинарная операция union()

```erlang
union(D1, D2) -> merge(fun(_, V1, V2) -> V1 + V2 end, D1, D2).
```

---

## Тестирование

В данной лабораторной работе я использовал два инструмента для тестирования:

- EUnit - для модульного тестирования
- Proper - для тестирования свойств (property-based)

---


## Вывод

В данной лабораторной я работе реализовал структуру данных Red Black Tree Dict,
при этом попробовал эмулировать ленивые вычисления на Erlang при помощи процессов, при этом ленивую реализация
представляет собой некую обертку над базовой реализацией rb-dict для операций изменения данных, 
Имхо, было достаточно интересно попытаться в лень в erlang.

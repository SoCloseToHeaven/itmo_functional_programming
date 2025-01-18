# Лабораторная работа №3 (Interpolation processes)

---

* Студент: `Лянгузов Дмитрий Максимович`
* Группа: `P3332`
* ИСУ: `368162`
* Функциональный язык: `Erlang`

--- 

## Требования

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

    - какие алгоритмы использовать (в том числе два сразу);
    - частота дискретизации результирующих данных;
    - и т.п.;

- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;


В рамках лабораторной работы было реализовано 2 варината:

- Со стандартными процессами в Erlang
- С использованием `OTP`

## Реализация для стандартных процессов:

Linear Interpolation:

```erlang
linear_interpolation(Step, [[X1, Y1], [X2, Y2]]) ->
    case X1 =:= X2 of
        true ->
            {error, linear};
        false ->
            K = (Y2 - Y1) / (X2 - X1),
            B = Y1 - K * X1,
            GenerateValues = interpolation:points_generator(Step, X1, X2),
            Res = [GenerateValues, lists:map(fun(X) -> K * X + B end, GenerateValues)],
            {ok, linear, Res}
    end.
```

Lagrange Interpolation: 

```erlang
lagrange_multiplier(X, Xi, Points) ->
    lists:foldl(
        fun
            ([Xj, _], Acc) when Xj =/= Xi ->
                Acc * (X - Xj) / (Xi - Xj);
            (_, Acc) ->
                Acc
        end,
        1,
        Points
    ).
lagrange_polynomial(X, Points) ->
    lists:foldl(
        fun([Xi, Yi], Acc) ->
            Acc + Yi * lagrange_multiplier(X, Xi, Points)
        end,
        0,
        Points
    ).
evaluate_lagrange(Step, Points) ->
    [X1, _] = hd(Points),
    [X2, _] = lists:last(Points),
    GeneratedDots = interpolation:points_generator(Step, X1, X2),
    [GeneratedDots, [lagrange_polynomial(X, Points) || X <- GeneratedDots]].
```

Supervisor basic spawn/receive example:

```erlang
supervisor_loop(ChildPids) ->
    receive
        {'EXIT', From, Reason} ->
            case Reason of
                ok ->
                    ok;
                _ ->
                    io:fwrite(
                        "Child process ~p exited with reason: ~p~n",
                        [From, Reason]
                    ),

                    RestartedChildPids = restart_child(From, ChildPids),

                    supervisor_loop(RestartedChildPids)
            end;
        {exit, Reason} ->
            io:fwrite("Shutdown requested: ~p~n", [Reason]),
            terminate_children(ChildPids),
            wait_for_children(length(ChildPids));
        _Other ->
            supervisor_loop(ChildPids)
    end.
```

```erlang
restart_child(From, ChildPids) ->
    case lists:keyfind(From, 2, ChildPids) of
        {Tag, _Pid} ->
            NewPid =
                case Tag of
                    interpolation ->
                        {Freq, Window, Methods} = main:parse_config(),
                        spawn_link(
                            interpolation_module,
                            start,
                            [{Freq, Window, Methods}]
                        );
                    io_input ->
                        {_, InterPid} = lists:keyfind(interpolation, 1, ChildPids),
                        spawn_link(io_server, start_input, [self(), InterPid]);
                    io_output ->
                        spawn_link(io_server, start_output, [])
                end,
            lists:keyreplace(Tag, 1, ChildPids, {Tag, NewPid});
        false ->
            io:fwrite("Unknown process ~p, not restarting.~n", [From]),
            ChildPids
    end.
```

### IO

Use run.sh script to run lab3

OR

`erl -noshell -s main start -freq 1 -w 4 -methods linear lagrange -s init stop`

С ключами:
- `freq` - частота дискретизации
- `w` - размер окна в методе Лагранжа
- `methods` - используемые методы


## OTP
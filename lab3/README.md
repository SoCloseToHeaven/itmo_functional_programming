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

## Spawn/Receive processes:

### Fundamentals 
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

### Fundamentals

Linear interpolation:

```erlang
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Callbacks
init([]) ->
  {ok, []}.

handle_cast({linear, Input, Freq}, State) ->
  %% Add the new point to the state
  NewState =
    case length(State) of
      2 -> tl(State) ++ [Input];
      _ -> State ++ [Input]
    end,

  %% Perform interpolation if there are exactly two points
  NewState =
    case NewState of
      [P1, P2] ->
        Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, [P1, P2]),
        Result = linear_interpolation(Freq, Sorted),
        case Result of
          {ok, linear, Res} ->
            gen_server:cast(interpolation_server, {ok, linear, Res}),
            NewState;
          {error, linear, Msg} ->
            gen_server:cast(interpolation_server, {error, linear, Msg}),
            NewState
        end;
      _ ->
        NewState
    end,

  {noreply, NewState};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call(_, _From, State) ->
  {reply, ok, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

% Logic

linear_interpolation(Step, [[X1, Y1], [X2, Y2]]) ->
  case X1 =:= X2 of
    true ->
      {error, linear, "Значения X совпадают, введите другое значение X"};
    false ->
      K = (Y2 - Y1) / (X2 - X1),
      B = Y1 - K * X1,
      GenerateValues = interpolation_server:points_generator(Step, X1, X2),
      Res = [GenerateValues, lists:map(fun(X) -> K * X + B end, GenerateValues)],
      {ok, linear, Res}
  end.
```

Lagrange Interpolation:

```erlang
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init([]) ->
  {ok, []}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({lagrange, Input, Window, Freq}, State) ->
  UpdatedPoints =
    case length(State) of
      Window ->
        tl(State) ++ [Input];
      _ ->
        State ++ [Input]
    end,
  case length(UpdatedPoints) of
    Window ->
      Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, UpdatedPoints),
      InterpolatedValues = evaluate_lagrange(Freq, Sorted),
      gen_server:cast(interpolation_server, {ok, lagrange, InterpolatedValues});
    _ ->
      ok
  end,
  {noreply, UpdatedPoints}.

handle_info(_, State) ->
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Lagrange multiplier
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

% Lagrange polynomial
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
  GeneratedDots = interpolation_server:points_generator(Step, X1, X2),
  [GeneratedDots, [lagrange_polynomial(X, Points) || X <- GeneratedDots]].
```

Interpolation server:

```erlang
start_link(InitialValues) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [InitialValues], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Callbacks
init([{Freq, Window}]) ->
  {ok, {Freq, Window}}.

handle_cast({interpolate, Input}, State) ->
  gen_server:cast(linear_server, {linear, Input, erlang:element(1, State)}),
  gen_server:cast(
    lagrange_server,
    {lagrange, Input, erlang:element(2, State), erlang:element(1, State)}
  ),
  {noreply, State};
handle_cast({ok, linear, Result}, State) ->
  gen_server:cast(output_server, {ok, linear, Result}),
  {noreply, State};
handle_cast({ok, lagrange, Result}, State) ->
  gen_server:cast(output_server, {ok, lagrange, Result}),
  {noreply, State};
handle_cast({error, Method, Msg}, State) ->
  gen_server:cast(output_server, {error, Method, Msg}),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call(_, _From, State) ->
  {reply, ok, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
```

### IO

## Conclusion
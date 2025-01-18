# Лабораторная работа по функциональному программированию №1
## Проект Эйлера №4, №27

---

* Студент: `Лянгузов Дмитрий Максимович`
* Группа: `P3332`
* ИСУ: `368459`
* Функциональный язык: `Erlang`

---

## Проблема №4

* **Название**: `Largest Palindrome Product`
* **Описание**: A palindromic number reads the same both ways. The largest palindrome made from the product of two $2$-digit numbers is $9009 = 91 \times 99$.
* **Задание**: Find the largest palindrome made from the product of two $3$-digit numbers.

---

### Объяснение решения
Чтобы найти самый большой палиндром, который получается при умножении двух трехзначных чисел, мы будем использовать простой метод:

1. Начнем с умножения наибольшего трехзначного числа (999) на 998, 997 и так далее, уменьшая множитель до 100.
2. Для каждого произведения проверим, является ли оно палиндромом (число, которое читается одинаково слева направо и справа налево, например, 121).
3. Если мы нашли палиндром, сравним его с уже найденным наибольшим палиндромом и выберем из них больший.
4. Продолжаем проверять произведения, пока не дойдем до 100.
5. В итоге мы получим самый большой палиндром, который можно получить умножением двух трехзначных чисел.
---

### Решение через хвостовую рекурсию

```erlang
solution() -> largest_pal_tail_rec(999, 999, 0).

is_pal(Num) ->
  StrNum = integer_to_list(Num),
  StrNum =:= lists:reverse(StrNum).


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
```

### Решение через обычную рекурсиюяи

```erlang
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
```

### Решение через модульность (reduce, filter, map)

```erlang
solution() ->
  Products = generate_products(),
  Pals = filter_pals(Products),
  find_max(Pals).

is_pal(Num) ->
  StrNum = integer_to_list(Num),
  StrNum =:= lists:reverse(StrNum).

%% Генерация последовательности
generate_products() -> [I * J || I <- lists:seq(100, 999), J <- lists:seq(100, 999)].

%% Фильтрация последовательности
filter_pals(Products) -> lists:filter(fun(X) -> is_pal(X) end, Products).

%% Поиск максимума в последовательности
find_max(Products) -> lists:max(Products).
```

### Решение через генерацию последовательности при помощи отображения (map)

```erlang
solution() ->
  Products = generate_products_via_map(100, 999),
  find_max_pal(Products).

is_pal(Num) ->
  StrNum = integer_to_list(Num),
  StrNum =:= lists:reverse(StrNum).

%% Генерация последовательности при помощи отображения
generate_products_via_map(Min, Max) ->
  lists:flatmap(fun(I) ->
    lists:map(fun(J) -> I * J end, lists:seq(Min, Max))
                end, lists:seq(Min, Max)).

find_max_pal(Products) -> lists:max(lists:filter(fun(X) -> is_pal(X) end, Products)).
```


### Решение через удобный для меня традиционный язык программирования для сравнения (JS)

```javascript
function isPalindrome(num) {
    const strNum = num.toString();
    return strNum === strNum.split('').reverse().join('');
}

(function findLargestPalindrome() {
    let maxPal = 0;
    for (let i = 999; i > 99; i--) {
        for (let j = 999; j > 99; j--) {
            const product = i * j;
            if (product > maxPal && isPalindrome(product)) {
                maxPal = product
            }
        }
    }

    console.log(maxPal);
})();
```

---

## Проблема №27

* **Название**: `Quadratic Primes`
* **Описание**:
  <p>Euler discovered the remarkable quadratic formula:</p>
  <p class="center">$n^2 + n + 41$</p>
  <p>It turns out that the formula will produce $40$ primes for the consecutive integer values $0 \le n \le 39$. However, when $n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41$ is divisible by $41$, and certainly when $n = 41, 41^2 + 41 + 41$ is clearly divisible by $41$.</p>
  <p>The incredible formula $n^2 - 79n + 1601$ was discovered, which produces $80$ primes for the consecutive values $0 \le n \le 79$. The product of the coefficients, $-79$ and $1601$, is $-126479$.</p>
  <p>Considering quadratics of the form:</p>
  <blockquote>
  $n^2 + an + b$, where $|a| &lt; 1000$ and $|b| \le 1000$<br><br><div>where $|n|$ is the modulus/absolute value of $n$<br>e.g. $|11| = 11$ and $|-4| = 4$</div>
  </blockquote> 

* **Задание**: Find the product of the coefficients, $a$ and $b$, for the quadratic expression that produces the maximum number of primes for consecutive values of $n$, starting with $n = 0$.

---

### Объяснение решения
1. Перебираем все варианты: Проходим по всем возможным значениям *a* и *b* в заданном диапазоне.
2. Считаем простые числа: Для каждой пары *a* и *b* подставляем в формулу из задачи разные значения *n* подряд, и смотрим, сколько из этих чисел окажутся "простыми".
3. Запоминаем рекорд: Сравниваем количество найденных простых чисел с лучшим результатом, который мы получили раньше. Если текущее количество больше, запоминаем новую пару *a* и *b*.
4. Ищем максимум: Повторяем шаги 2 и 3 для всех вариантов *a* и *b*. В итоге у нас останется пара чисел, для которых количество "простых" чисел *n* будет максимальным.
5. Результат: Перемножаем *a* и *b* – это и будет наш ответ.
---

### Решение через хвостовую рекурсию

```erlang
is_prime(N) when N < 2 -> false;
is_prime(N) -> prime_helper(N, 2).

prime_helper(N, I) when I * I > N -> true;
prime_helper(N, I) ->
  case N rem I of
    0 -> false;
    _ -> prime_helper(N, I + 1)
  end.

quadratic(A, B, N) ->
  N * N + A * N + B.

count_primes(A, B) ->
  count_primes_helper(A, B, 0).

count_primes_helper(A, B, Count) ->
  Quadratic = quadratic(A, B, Count),
  case is_prime(Quadratic) of
    true -> count_primes_helper(A, B, Count + 1);
    _ -> Count
  end.

solution() ->
  quadratic_primes_tail_recur(-999, -1000, 0, {0, 0}).

quadratic_primes_tail_recur(A, _, MaxCount, MaxOdds) when A > 1000 ->
  {element(1, MaxOdds) * element(2, MaxOdds), element(1, MaxOdds), element(2, MaxOdds), MaxCount};

quadratic_primes_tail_recur(A, B, MaxCount, MaxOdds) when B > 1001 ->
  quadratic_primes_tail_recur(A + 1, -1000, MaxCount, MaxOdds);

quadratic_primes_tail_recur(A, B, MaxCount, MaxOdds) ->
  Count = count_primes(A, B),
  case Count > MaxCount of
    true ->
      quadratic_primes_tail_recur(A, B + 1, Count, {A, B});
    false ->
      quadratic_primes_tail_recur(A, B + 1, MaxCount, MaxOdds)
  end.
```

### Решение через обычную рекурсию

```erlang
is_prime(N) when N < 2 -> false;
is_prime(N) -> prime_helper(N, 2).

prime_helper(N, I) when I * I > N -> true;
prime_helper(N, I) ->
  case N rem I of
    0 -> false;
    _ -> prime_helper(N, I + 1)
  end.

quadratic(A, B, N) ->
  N * N + A * N + B.

count_primes(A, B) ->
  count_primes_helper(A, B, 0).

count_primes_helper(A, B, Count) ->
  Quadratic = quadratic(A, B, Count),
  case is_prime(Quadratic) of
    true -> count_primes_helper(A, B, Count + 1);
    _ -> Count
  end.

solution() -> quadratic_primes_usual_recur(-999, -1000).


quadratic_primes_usual_recur(A, _) when A > 1000 ->
  {0, 0, 0, 0};

quadratic_primes_usual_recur(A, B) when B > 1001 ->
  quadratic_primes_usual_recur(A + 1, -1000);

quadratic_primes_usual_recur(A, B) ->
  Count = count_primes(A, B),
  NextResult = quadratic_primes_usual_recur(A, B + 1),
  if
    Count > element(4, NextResult) -> {A * B, A, B, Count};
    true -> NextResult
  end.
```

### Решение через модульность (reduce, filter, map)

```erlang
is_prime(N) when N < 2 -> false;
is_prime(N) -> prime_helper(N, 2).

prime_helper(N, I) when I * I > N -> true;
prime_helper(N, I) ->
  case N rem I of
    0 -> false;
    _ -> prime_helper(N, I + 1)
  end.

quadratic(A, B, N) ->
  N * N + A * N + B.

count_primes(A, B) ->
  count_primes_helper(A, B, 0).

count_primes_helper(A, B, Count) ->
  Quadratic = quadratic(A, B, Count),
  case is_prime(Quadratic) of
    true -> count_primes_helper(A, B, Count + 1);
    _ -> Count
  end.

solution() -> quadratic_primes_modular().

coefficients() -> [{A, B} || A <- lists:seq(-999, 999), B <- lists:seq(-1000, 1000)].

generate_results(Combinations) -> [{A * B, A, B, count_primes(A, B)} || {A, B} <- Combinations].

filter_results(Results) -> lists:filter(fun(Result) -> element(4, Result) > 0 end, Results).

find_max(Results) -> lists:foldl(
  fun({_, _, _, A} = Left, {_, _, _, B} = Right) ->
    if A > B -> Left;
      true -> Right
    end
  end, {0, 0, 0, 0}, Results).

quadratic_primes_modular() ->
  Coefficients = coefficients(),
  Results = generate_results(Coefficients),
  FilteredResults = filter_results(Results),
  find_max(FilteredResults).
```

### Решение через генерацию последовательности при помощи отображения (map)

```erlang
is_prime(N) when N < 2 -> false;
is_prime(N) -> prime_helper(N, 2).

prime_helper(N, I) when I * I > N -> true;
prime_helper(N, I) ->
  case N rem I of
    0 -> false;
    _ -> prime_helper(N, I + 1)
  end.

quadratic(A, B, N) ->
  N * N + A * N + B.

count_primes(A, B) ->
  count_primes_helper(A, B, 0).

count_primes_helper(A, B, Count) ->
  Quadratic = quadratic(A, B, Count),
  case is_prime(Quadratic) of
    true -> count_primes_helper(A, B, Count + 1);
    _ -> Count
  end.

solution() -> quadratic_primes_map().

find_max(Results) -> lists:foldl(
  fun({_, _, _, A} = Left, {_, _, _, B} = Right) ->
    if A > B -> Left;
      true -> Right
    end
  end, {0, 0, 0, 0}, Results).

combinations() -> [{A, B} || A <- lists:seq(-999, 999), B <- lists:seq(-1000, 1000)].

generate_results(Combinations) -> lists:map(fun({A, B}) -> {A * B, A, B, count_primes(A, B)} end, Combinations).

quadratic_primes_map() ->
  Combinations = combinations(),
  Results = generate_results(Combinations),
  FilteredResults = lists:filter(fun(Result) -> element(4, Result) > 0 end, Results),
  find_max(FilteredResults).
```


### Решение через удобный для меня традиционный язык программирования для сравнения (JS)

```javascript
function isPrime(n) {
    if (n < 2) {
        return false;
    }
    for (let i = 2; i <= Math.sqrt(n); i++) {
        if (n % i === 0) {
            return false;
        }
    }
    return true;
}

function countPrimes(a, b) {
    let n = 0;
    while (isPrime(n * n + a * n + b)) {
        n++;
    }
    return n;
}

function findMaxCoeffs() {
    let maxCount = 0;
    let maxCoeffs = [0, 0];

    for (let a = -999; a < 1000; a++) {
        for (let b = -1000; b <= 1000; b++) {
            let count = countPrimes(a, b);
            if (count > maxCount) {
                maxCount = count;
                maxCoeffs = [a, b];
            }
        }
    }

    return maxCoeffs
}

// Example usage:
const [a, b] = findMaxCoeffs();
console.log(`Max coefficients: a=${a}, b=${b}`);
console.log(`Coefficients product: ${a * b}`)
```

---

## Вывод

Решая задачи Project Euler, я углубился в функциональное программирование, 
освоив основные концепции и подходы. 

Использование хвостовой рекурсии и ленивых коллекций позволило мне эффективно работать с большими наборами данных, избегая переполнения стека и оптимизируя потребление памяти. Функции высшего порядка (map, filter, reduce) значительно упростили обработку данных, 
делая код более лаконичным и читаемым.

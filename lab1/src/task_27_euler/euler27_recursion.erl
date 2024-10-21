-module(euler27_recursion).
-author("dmitry").

%% API
-export([solution/0]).

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

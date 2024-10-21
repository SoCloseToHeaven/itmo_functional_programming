-module(euler27_tail_recursion).
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
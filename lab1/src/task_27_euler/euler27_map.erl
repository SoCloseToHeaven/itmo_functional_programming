-module(euler27_map).
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
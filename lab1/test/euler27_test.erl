-module(euler27_test).
-author("dmitry").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ANSWER, {-59231, -61, 971, 71}).

euler27_recursion_test() -> ?assertEqual(?ANSWER, euler27_recursion:solution()).

euler27_tail_recursion_test() -> ?assertEqual(?ANSWER, euler27_tail_recursion:solution()).

euler27_modular_test() -> ?assertEqual(?ANSWER, euler27_modular:solution()).

euler27_map_test() -> ?assertEqual(?ANSWER, euler27_map:solution()).

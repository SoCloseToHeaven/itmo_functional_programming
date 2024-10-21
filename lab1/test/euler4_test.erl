-module(euler4_test).
-author("dmitry").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ANSWER, 906609).

euler4_recursion_test() -> ?assertEqual(?ANSWER, euler4_recursion:solution()).

euler4_tail_recursion_test() -> ?assertEqual(?ANSWER, euler4_tail_recursion:solution()).

euler4_modular_test() -> ?assertEqual(?ANSWER, euler4_modular:solution()).

euler4_map_test() -> ?assertEqual(?ANSWER, euler4_map:solution()).
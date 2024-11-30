-module(rbdict_unit_test).
-author("dmitry").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

new_dict_test() ->
  Dict = rbdict:new(),
  ?assertEqual(empty, Dict).

append_test() ->
  Dict = rbdict:append('1', '2', rbdict:new()),
  HasKey = rbdict:is_key('1', Dict),
  ?assertEqual(true, HasKey).
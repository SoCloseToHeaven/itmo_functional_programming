-module(rbdict_unit_test).
-author("dmitry").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

new_dict_test() ->
    Dict = rbdict:new(),
    ?assertEqual(empty, Dict).

store_test() ->
    Dict = rbdict:store('1', '2', rbdict:new()),
    HasKey = rbdict:is_key('1', Dict),
    ?assertEqual(true, HasKey).

equals_test() ->
    Dict = rbdict:store('1', '1', empty),
    Dict1 = rbdict:store('1', '2', empty),

    Result = rbdict:equals(Dict, Dict),
    Result1 = rbdict:equals(Dict, Dict1),

    ?assertEqual(true, Result),
    ?assertEqual(false, Result1).

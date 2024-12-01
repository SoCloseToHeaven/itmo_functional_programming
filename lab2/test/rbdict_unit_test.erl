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

fetch_test() ->
    Dict = rbdict:store('1', '1', rbdict:new()),

    Result = rbdict:fetch('1', Dict),

    ?assertEqual('1', Result).

find_test() ->
    Dict = rbdict:store('1', '1', rbdict:new()),

    Result1 = rbdict:find('1', Dict),
    Result2 = rbdict:find('2', Dict),

    ?assertEqual('1', Result1),
    ?assertEqual(error, Result2).

map_test() ->
    Dict = rbdict:store('1', 1, rbdict:new()),

    MappedDict = rbdict:map(fun(_K, V) -> V + 1 end, Dict),

    Value = rbdict:fetch('1', MappedDict),

    ?assertEqual(2, Value).

filter_test() ->
    Dict = rbdict:store('1', 1, rbdict:new()),
    FilteredDict = rbdict:filter(fun(_K, V) -> false end, Dict),

    ?assertEqual(empty, FilteredDict).
equals_test() ->
    Dict = rbdict:store('1', '1', empty),
    Dict1 = rbdict:store('1', '2', empty),

    Result = rbdict:equals(Dict, Dict),
    Result1 = rbdict:equals(Dict, Dict1),

    ?assertEqual(true, Result),
    ?assertEqual(false, Result1).

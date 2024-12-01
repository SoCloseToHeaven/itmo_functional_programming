-module(prop_rbdict_tests).
-include_lib("proper/include/proper.hrl").

-define(KEY_FILL_VALUE, 2).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_associativity_test() ->
    ?FORALL(
        {Keys1, Keys2, Keys3},
        {list(int()), list(int()), list(int())},
        begin
            Dict1 = store_keys_example(Keys1),
            Dict2 = store_keys_example(Keys2),
            Dict3 = store_keys_example(Keys3),

            Union1 = rbdict:union(
                rbdict:union(Dict1, Dict2), Dict3
            ),

            Union2 = rbdict:union(
                rbdict:union(Dict1, Dict3), Dict2
            ),

            rbdict:equals(Union1, Union2)
        end
    ).

store_keys_example(Keys) -> store_keys_example(Keys, rbdict:new()).
store_keys_example([], Dict) ->
    Dict;
store_keys_example([H | T], Dict) ->
    NewDict = rbdict:store(H, ?KEY_FILL_VALUE, Dict),
    store_keys_example(T, NewDict).

-module(rbdict_lazy).

%% Extended API
-export([new/1, current/1, current_lazy/1]).
%% Extended lazy API for rbdict
-export([store/3, erase/2, update/3, update_val/3]).
-export([map/2, filter/2, fold/3, merge/3]).

-define(SCHEDULE_OPERATION(Fun, Pid), Pid ! {Fun, self()}).

new(Dict) -> spawn(fun() -> loop({Dict, []}) end).

current(Pid) ->
    Pid ! {force, self()},
    receive
        {Dict, Pid} -> Dict
    end.

current_lazy(Pid) ->
    Pid ! {lazy, self()},
    receive
        {Dict, Pid} -> Dict
    end.

loop({Dict, Operations} = State) ->
    NewState =
        receive
            {Operation, From} when is_function(Operation) andalso is_pid(From) ->
                {Dict, [Operation | Operations]};
            {force, From} ->
                NewDict = force(Dict, Operations),
                From ! {NewDict, self()},
                {NewDict, []};
            {lazy, From} ->
                From ! {Dict, self()},
                State
        end,
    loop(NewState).

force(Dict, []) -> Dict;
force(Dict, [Operation | T]) -> force(Operation(Dict), T).

store(K, V, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:store(K, V, Dict) end, Pid).

filter(Fun, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:filter(Fun, Dict) end, Pid).

map(Fun, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:map(Fun, Dict) end, Pid).

fold(Fun, Acc, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:fold(Fun, Acc, Dict) end, Pid).

erase(K, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:erase(K, Dict) end, Pid).

merge(Fun, Dict1, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:merge(Fun, Dict1, Dict) end, Pid).

update(Key, Fun, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:update(Key, Fun, Dict) end, Pid).

update_val(Key, Value, Pid) ->
    ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:update_val(Key, Value, Dict) end, Pid).

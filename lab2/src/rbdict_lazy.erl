-module(rbdict_lazy).

%% API
-export([
  new/1,
  current/1,
  filter/2,
  store/3
]).
%% fold/3, map/2, filter/2, merge/3
%% update_val/3
%% erase
%% store

-define(SCHEDULE_OPERATION(Fun, Pid), Pid ! {Fun, self()}).

new(Dict) -> spawn(fun() -> loop(Dict, []) end).

current(Pid) ->
  Pid ! {force, self()},
  receive
    {Dict, Pid} -> Dict
  end.

loop(Dict, Operations) ->
  receive
    {Operation, From} when is_function(Operation) andalso is_pid(From) -> loop(Dict, [Operation | Operations]);
    {force, From} -> From ! {force(Dict, Operations), self()}
  end.

force(Dict, []) -> Dict;
force(Dict, [Operation | T]) -> force(Operation(Dict), T).


store(K, V, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:store(K, V, Dict) end, Pid).

filter(Fun, Pid) -> ?SCHEDULE_OPERATION(fun(Dict) -> rbdict:filter(Fun, Dict) end, Pid).


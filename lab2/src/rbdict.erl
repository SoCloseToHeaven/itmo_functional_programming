-module(rbdict).

%% Standard interface.
-export([new/0, is_key/2, to_list/1, from_list/1, size/1]).
-export([fetch/2, find/2, fetch_keys/1, erase/2]).
-export([store/3]).
-export([update_val/3, update/3]).
-export([fold/3, map/2, filter/2, merge/3, union/2]).
-export([equals/2]).

new() -> empty.

%% is_key(Key, Dict) -> true | false.

is_key(_, empty) ->
    false;
is_key(K, {_, Left, K1, _, _}) when K < K1 ->
    is_key(K, Left);
is_key(K, {_, _, K1, _, Right}) when K > K1 ->
    is_key(K, Right);
is_key(_, {_, _, _, _, _}) ->
    true.

%% to_list(Dict) -> [{Key,Value}].

to_list(T) -> to_list(T, []).

to_list(empty, List) -> List;
to_list({_, A, Xk, Xv, B}, List) -> to_list(A, [{Xk, Xv} | to_list(B, List)]).

%% from_list([{Key,Value}]) -> Dict.

from_list(L) ->
    lists:foldl(fun({K, V}, D) -> store(K, V, D) end, new(), L).

%% size(Dict) -> int().

size(T) -> size1(T).

size1(empty) -> 0;
size1({_, L, _, _, R}) -> size1(L) + size1(R) + 1.

%% fetch(Key, Dict) -> Value.

fetch(K, {_, Left, K1, _, _}) when K < K1 ->
    fetch(K, Left);
fetch(K, {_, _, K1, _, Right}) when K > K1 ->
    fetch(K, Right);
fetch(_, {_, _, _, Val, _}) ->
    Val.

%% find(Key, Dict) -> {ok,Value} | error.

find(_, empty) ->
    error;
find(K, {_, Left, K1, _, _}) when K < K1 ->
    find(K, Left);
find(K, {_, _, K1, _, Right}) when K > K1 ->
    find(K, Right);
find(_, {_, _, _, Val, _}) ->
    Val.

%% fetch_keys(Dict) -> [Key].

fetch_keys(T) -> fetch_keys(T, []).

fetch_keys(empty, Tail) -> Tail;
fetch_keys({_, L, K, _, R}, Tail) -> fetch_keys(L, [K | fetch_keys(R, Tail)]).

%% store(Key, Val, Dict) -> Dict.

store(K, V, T) ->
    {_, L, K1, V1, R} = store1(K, V, T),
    %setelement(1, b, T1).
    {b, L, K1, V1, R}.

store1(K, V, empty) ->
    {r, empty, K, V, empty};
store1(K, V, {C, Left, K1, V1, Right}) when K < K1 ->
    lbalance(C, store1(K, V, Left), K1, V1, Right);
store1(K, V, {C, Left, K1, V1, Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, store1(K, V, Right));
store1(K, V, {C, L, _, _, R}) ->
    {C, L, K, V, R}.

%% update_val(Key, Val, Dict) -> Dict.

update_val(K, V, {RB, A, Xk, Xv, B}) when K < Xk ->
    {RB, update_val(K, V, A), Xk, Xv, B};
update_val(K, V, {RB, A, Xk, Xv, B}) when K > Xk ->
    {RB, A, Xk, Xv, update_val(K, V, B)};
update_val(_, V, {RB, A, Xk, _, B}) ->
    {RB, A, Xk, V, B}.

%% update(Key, Fun, Dict) -> Dict.

update(K, F, {RB, A, Xk, Xv, B}) when K < Xk ->
    {RB, update(K, F, A), Xk, Xv, B};
update(K, F, {RB, A, Xk, Xv, B}) when K > Xk ->
    {RB, A, Xk, Xv, update(K, F, B)};
update(_, F, {RB, A, Xk, Xv, B}) ->
    {RB, A, Xk, F(Xv), B}.

%% update(Key, Fun, Init, Dict) -> Dict.

update(K, F, I, T) ->
    {_, L, K1, V1, R} = update1(K, F, I, T),
    %setelement(1, b, T1).
    {b, L, K1, V1, R}.

update1(K, _, I, empty) ->
    {r, empty, K, I, empty};
update1(K, F, I, {RB, A, Xk, Xv, B}) when K < Xk ->
    lbalance(RB, update1(K, F, I, A), Xk, Xv, B);
update1(K, F, I, {RB, A, Xk, Xv, B}) when K > Xk ->
    rbalance(RB, A, Xk, Xv, update1(K, F, I, B));
update1(_, F, _, {RB, A, Xk, Xv, B}) ->
    {RB, A, Xk, F(Xv), B}.

%% lbalance(Colour, Left, Key, Val, Right).
%% rbalance(Colour, Left, Key, Val, Right).
%% Balance a tree afer (possibly) adding a node to the left/right.

lbalance(b, {r, {r, A, Xk, Xv, B}, Yk, Yv, C}, Zk, Zv, D) ->
    {r, {b, A, Xk, Xv, B}, Yk, Yv, {b, C, Zk, Zv, D}};
lbalance(b, {r, A, Xk, Xv, {r, B, Yk, Yv, C}}, Zk, Zv, D) ->
    {r, {b, A, Xk, Xv, B}, Yk, Yv, {b, C, Zk, Zv, D}};
lbalance(C, A, Xk, Xv, B) ->
    {C, A, Xk, Xv, B}.

rbalance(b, A, Xk, Xv, {r, {r, B, Yk, Yv, C}, Zk, Zv, D}) ->
    {r, {b, A, Xk, Xv, B}, Yk, Yv, {b, C, Zk, Zv, D}};
rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, {r, C, Zk, Zv, D}}) ->
    {r, {b, A, Xk, Xv, B}, Yk, Yv, {b, C, Zk, Zv, D}};
rbalance(C, A, Xk, Xv, B) ->
    {C, A, Xk, Xv, B}.

%% erase(Key, Dict) -> Dict.

erase(K, T) ->
    {T1, _} = erase_aux(K, T),
    T1.

%% erase_aux(Key, Node) -> {Node,Decreased}.

erase_aux(_, empty) ->
    {empty, false};
erase_aux(K, {b, A, Xk, Xv, B}) ->
    if
        K < Xk ->
            {A1, Dec} = erase_aux(K, A),
            if
                Dec -> unbalright(b, A1, Xk, Xv, B);
                true -> {{b, A1, Xk, Xv, B}, false}
            end;
        K > Xk ->
            {B1, Dec} = erase_aux(K, B),
            if
                Dec -> unballeft(b, A, Xk, Xv, B1);
                true -> {{b, A, Xk, Xv, B1}, false}
            end;
        true ->
            case B of
                empty ->
                    blackify(A);
                _ ->
                    {B1, {Mk, Mv}, Dec} = erase_min(B),
                    if
                        Dec -> unballeft(b, A, Mk, Mv, B1);
                        true -> {{b, A, Mk, Mv, B1}, false}
                    end
            end
    end;
erase_aux(K, {r, A, Xk, Xv, B}) ->
    if
        K < Xk ->
            {A1, Dec} = erase_aux(K, A),
            if
                Dec -> unbalright(r, A1, Xk, Xv, B);
                true -> {{r, A1, Xk, Xv, B}, false}
            end;
        K > Xk ->
            {B1, Dec} = erase_aux(K, B),
            if
                Dec -> unballeft(r, A, Xk, Xv, B1);
                true -> {{r, A, Xk, Xv, B1}, false}
            end;
        true ->
            case B of
                empty ->
                    {A, false};
                _ ->
                    {B1, {Mk, Mv}, Dec} = erase_min(B),
                    if
                        Dec -> unballeft(r, A, Mk, Mv, B1);
                        true -> {{r, A, Mk, Mv, B1}, false}
                    end
            end
    end.

%% erase_min(Node) -> {Node,{NodeKey,NodeVal},Decreased}.

erase_min({b, empty, Xk, Xv, empty}) ->
    {empty, {Xk, Xv}, true};
erase_min({b, empty, Xk, Xv, {r, A, Yk, Yv, B}}) ->
    {{b, A, Yk, Yv, B}, {Xk, Xv}, false};
erase_min({b, empty, _, _, {b, _, _, _, _}}) ->
    exit(boom);
erase_min({r, empty, Xk, Xv, A}) ->
    {A, {Xk, Xv}, false};
%% Rec from left
erase_min({b, A, Xk, Xv, B}) ->
    {A1, Min, Dec} = erase_min(A),
    if
        Dec ->
            {T, Dec1} = unbalright(b, A1, Xk, Xv, B),
            {T, Min, Dec1};
        true ->
            {{b, A1, Xk, Xv, B}, Min, false}
    end;
erase_min({r, A, Xk, Xv, B}) ->
    {A1, Min, Dec} = erase_min(A),
    if
        Dec ->
            {T, Dec1} = unbalright(r, A1, Xk, Xv, B),
            {T, Min, Dec1};
        true ->
            {{r, A1, Xk, Xv, B}, Min, false}
    end.

blackify({r, A, K, V, B}) -> {{b, A, K, V, B}, false};
blackify(Node) -> {Node, true}.

unballeft(r, {b, A, Xk, Xv, B}, Yk, Yv, C) ->
    {lbalance(b, {r, A, Xk, Xv, B}, Yk, Yv, C), false};
unballeft(b, {b, A, Xk, Xv, B}, Yk, Yv, C) ->
    {lbalance(b, {r, A, Xk, Xv, B}, Yk, Yv, C), true};
unballeft(b, {r, A, Xk, Xv, {b, B, Yk, Yv, C}}, Zk, Zv, D) ->
    {{b, A, Xk, Xv, lbalance(b, {r, B, Yk, Yv, C}, Zk, Zv, D)}, false}.

unbalright(r, A, Xk, Xv, {b, B, Yk, Yv, C}) ->
    {rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C}), false};
unbalright(b, A, Xk, Xv, {b, B, Yk, Yv, C}) ->
    {rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C}), true};
unbalright(b, A, Xk, Xv, {r, {b, B, Yk, Yv, C}, Zk, Zv, D}) ->
    {{b, rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C}), Zk, Zv, D}, false}.

%% fold(Fun, Acc, Dict) -> Acc.

fold(_, Acc, empty) -> Acc;
fold(F, Acc, {_, A, Xk, Xv, B}) -> fold(F, F(Xk, Xv, fold(F, Acc, B)), A).

%% map(Fun, Dict) -> Dict.

map(_, empty) -> empty;
map(F, {RB, A, Xk, Xv, B}) -> {RB, map(F, A), Xk, F(Xk, Xv), map(F, B)}.

%% filter(Fun, Dict) -> Dict.

filter(F, T) -> filter(F, T, new()).

filter(_, empty, New) ->
    New;
filter(F, {_, A, Xk, Xv, B}, New0) ->
    New1 = filter(F, A, New0),
    New2 =
        case F(Xk, Xv) of
            true -> store(Xk, Xv, New1);
            false -> New1
        end,
    filter(F, B, New2).

%% merge(Fun, Dict, Dict) -> Dict.

merge(F, D1, D2) ->
    fold(
        fun(K, V2, D) ->
            update(K, fun(V1) -> F(K, V1, V2) end, V2, D)
        end,
        D1,
        D2
    ).

%% Union is defined only for data structures which also support unions

union(D1, D2) -> merge(fun(_, V1, V2) -> V1 + V2 end, D1, D2).

equals(D1, D2) ->
    KeysD1 = fetch_keys(D1),
    KeysD2 = fetch_keys(D2),

    Comparator = fun(Key) ->
        rbdict:find(Key, D1) == rbdict:find(Key, D2)
    end,

    lists:all(Comparator, KeysD1) andalso lists:all(Comparator, KeysD2).

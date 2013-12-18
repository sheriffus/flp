% -*- mode: prolog; mode: folding -*-

:- module(avlTest).

:- source.
:- add_to_path( './src/' ). % the path to plqc
:- reconsult(plqc).

:- reconsult(avl).


% {{{ testing a copy of Yap's avl.yap module (copy in current directory)
%% we have an interface with three predicates: avl_new/1, avl_insert/4 and avl_lookup/3
%% we are supposed to create the tree and insert and lookup values

  % {{{ generator for avl commands

%  a generator of avl.yap uses (sequences of operations) 
genAvl(Calls, Size) :-
% it always starts with creating a tree
% this implies that when size is 0 the tree is still created
        Calls = [(avl:avl_new) | Cs1],
% and then is just a bunch of calls to insert and lookup values
        genAvlCmds([], Cs1, Size).

% generating random insertions and randomly choose some of those to lookup after, using difference lists for better append sublists
% zero size means zero calls
genAvlCmds(_Lookups, [], 0) :- !. % cut - no other case if the size is 0
genAvlCmds(Lookups, Cs, Size) :-
        avlTest:genCmdHead(Lookups, X, Size),
        (X = {i, Val},
% commands are missing the continuations, these  will be controled by the property itself
        avlTest:genKey(Key, Size),
        Cs = [ {i, avl:avl_insert(Key, Val)} |CsS],
% update the possible correct lookups
        NewLookups = [ {Key,Val} | Lookups]
    ;
        X = {fl, {FK, FV}},
% just put the failing lookup
        Cs = [ {fl, avl:avl_lookup(FK, FV)} |CsS]
    ;
        (X = {l1, {K, V}}, Lx=l1 ; X = {l2, {K, V}}, Lx=l2),
        Cs = [ {Lx, avl:avl_lookup(K, V)} |CsS]
        ),
        MinSize is Size // 2,
        MaxSize is Size-1,
        choose(MinSize, MaxSize, NewSize, Size),
        avlTest:genAvlCmds(NewLookups, CsS, NewSize)
.

genCmdHead([], X, Size) :- !,
        Insert = plqc:structure({plqc:value(i), avlTest:genVal}),
        FailLookup = plqc:structure({plqc:value(fl), {avlTest:genKey, avlTest:fGen}}),
        plqc:frequency([{9,Insert}, {1, FailLookup}], X, Size).
genCmdHead(Lookups, X, Size) :-
        Insert = plqc:structure({plqc:value(i), avlTest:genVal}),
        FailLookup = plqc:structure({plqc:value(fl), {avlTest:genKey, avlTest:fGen}}),
        Lookup1 = plqc:structure({plqc:value(l1), plqc:elements(Lookups)}),
        Lookup2 = plqc:structure({plqc:value(l2), plqc:elements(Lookups)}),
        plqc:frequency([{6,Insert}, {2,Lookup1}, {2,Lookup2}, {1, FailLookup}], X, Size).

    % {{{ auxiliar generators
genKey(Key, Size) :-
        plqc:choose(0, 50000, Key, Size).

genVal(Val, Size) :-
        plqc:choose(0, Size, Val, Size).

fGen(X, Size) :-
        plqc:structure([avlTest:genVal],X,Size).
    % }}}

  % }}}

  % {{{ accessing and checking tree fields
get_key(avl(_Left, Key, _Val, _Balance, _Right), Key).
cmp_keys(K1, K2, Cmp) :-
        print({K1, K2}),
(
        K1 < K2, Cmp = lt
    ;
        K1 =< K2, Cmp = lte
    ;
        K1 =:= K2, Cmp = eq
    ;
        K1 >= K2, Cmp = gte
    ;
        K1 > K2, Cmp = gt
)    .
left(avl(Left, _Key, _Val, _Balance, _Right), Left).
right(avl(_Left, _Key, _Val, _Balance, Right), Right).
is_nil(avl(_Left, _Key, _Val, _Balance, _Right), false).
is_nil([], true).

  % }}}
%% we take the generator of the avl module uses and... use it :)

pcprop(avlUses1) :-
        pcforall(avlTest:genAvl, Calls, pcprop({avlUses1, Calls})).

pcprop({avlUses1, [(avl:avl_new)|Calls]}) :-
        call((avl:avl_new(Tree)))
          pc_and
        pcif(
          pcprop({avl, Tree, 0})
        , (pcprop({avlUses, Tree, Calls}))
        , (print(avl_new_fail_invariant), nl, fail))
        .


pcprop({avlUses, Tree, []}).
pcprop({avlUses, Tree, [{i,avl:avl_insert(Key,Val)}|Calls]}) :-
        ( call(avl:avl_insert(Key,Val), Tree, ContinuationTree)
        %; nl, print(failed_insert), nl, print({Tree, Key, Val}), nl, nl, fail)
        )
          pc_and
        pcif(
          pcprop({avl, ContinuationTree, H})
        , (pcprop({avlUses, ContinuationTree, Calls}))
        , (print(avl_insert_fail_invariant), nl,
          print({Tree, ContinuationTree}), nl, nl,
          fail))
        .
pcprop({avlUses, Tree, [{l1,avl:avl_lookup(Key,Val)}|Calls]}) :-
        (call(avl:avl_lookup(Key,Val), Tree); print(avl_lookup_not_found_fail), nl, fail)
          pc_and
        pcprop({avl, Tree, _H})
          pc_and
        pcprop({avlUses, Tree, Calls}).
pcprop({avlUses, Tree, [{l2,avl:avl_lookup(Key,Val)}|Calls]}) :-
        ((call(avl:avl_lookup(Key,XVal), Tree), !
        ; print(avl_lookup_not_bound_fail), nl, fail), XVal = Val)
          pc_and
        pcprop({avl, Tree, _H})
          pc_and
        pcprop({avlUses, Tree, Calls}).
pcprop({avlUses, Tree, [{fl,avl:avl_lookup(Key,Val)}|Calls]}) :-
        (call(avl:avl_lookup(Key,Val), Tree), print(avl_lookup_found_fail), nl, !, fail
    ;   true)
          pc_and
        pcprop({avl, Tree, _H})
          pc_and
        pcprop({avlUses, Tree, Calls}).

  % {{{ test
%% [(avl:avl_new(_A)'|'[{i,avl:avl_insert(21915,15)},{i,avl:avl_insert(44184,12)},{fl,avl:avl_lookup(25986,[0])},{i,avl:avl_insert(38292,3)},{i,avl:avl_insert(12413,0)},{fl,avl:avl_lookup(46865,[1])}])],
%%    ?- plqc:quickcheck( avlTest:(pcprop(avlUses)), [{numtests, 1000}] ).
%% ...................................................................................avl_lookup_not_bound_fail
%% !
%% Failed: After 732 test(s).
%% Counterexample found: [[avl:avl_new,{i,avl:avl_insert(45157,19)},{i,avl:avl_insert(44162,17)},{i,avl:avl_insert(40204,6)},{l1,avl:avl_lookup(40204,6)},{i,avl:avl_insert(40988,8)},{i,avl:avl_insert(21486,9)},{i,avl:avl_insert(34355,2)},{l2,avl:avl_lookup(40988,8)},{i,avl:avl_insert(45157,0)},{i,avl:avl_insert(7342,0)},{l2,avl:avl_lookup(45157,0)}]] 
%% yes
  % }}}

% }}}

% {{{ avl initial property (gives out height, but should it?)
          %% tree, height, 
%% (curr_key, avlTest:get_key), (cmp_key, avlTest:cmp_keys)
%% (left, avlTest:left), (right, avlTest:right), (is_nil, avlTest:is_nil)
%% (curr_key, GetKey), (cmp_key, CmpKeys),
%% (left, L), (right, R), (is_nil, IsNil)
pcprop({avl, T, H}) :- 
        pcif(
            (avlTest:is_nil(T, V), V = false)
        ,(
            (avlTest:get_key(T, Key), avlTest:left(T, LT), avlTest:right(T, RT))
          pc_and
            %% subtrees are avl where Key will be in the correct key order
            pcprop({avl, [Key], [], LT, LH})
          pc_and
            pcprop({avl, [], [Key], RT, RH})
          pc_and
            ((abs(LH-RH) =< 1, !; 
print({LH,RH, LT, RT, T}), nl,
print(height_mismatch1), nl, fail),
            H is 1+ max(LH,RH))
        ),
            %% if this is an empty tree, it is an avl of height 0
            (H = 0) % V=false
        )
.
% }}}
% {{{ avl property with accumulators
%% gt, lt, tree, height
pcprop({avl, GTS, LTS, T, H}) :- 
        pcif(
            (avlTest:is_nil(T, V), V=false)
        ,(
            (avlTest:get_key(T, Key), avlTest:left(T, LT), avlTest:right(T, RT),
            %% Key is greater then or equal to all lesser values (LTS)
            (forall(member(X, LTS), avlTest:cmp_keys(X, Key, lte)), !
            ;  print(avl_key_not_inorder1), nl, fail),
            %% Key is less then all greater values (GTS)
            (forall(member(X, GTS), avlTest:cmp_keys(X, Key, gt)), !
            ; print(avl_key_not_inorder2), nl, fail))
          pc_and
            %% subtrees are avl where Key will be in the correct key order
            pcprop({avl, [Key|GTS], LTS, LT, LH})
          pc_and
            pcprop({avl, GTS, [Key|LTS], RT, RH})
          pc_and
            ((abs(LH-RH) =< 1, !; print(height_mismatch_acc), nl, fail),
            H is 1+ max(LH,RH))
        ),
            %% if this is an empty tree, it is an avl of height 0
            (H = 0) % V=false
        ).
% }}}

 %%    pc_and
 %% print({Tree, ContinuationTree}),nl

% {{{ 
% }}}

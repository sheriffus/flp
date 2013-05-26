% -*- mode: prolog; mode: folding -*-

:- module(avlTest).

:- source.
:- add_to_path( './src/' ).
:- reconsult(plqc).

:- reconsult(avl).

%% bst - T is a binary search tree
%% meaning a node's key is greater than all keys in the left subtree
%% and is smaller than all keys in the right subtree
%% assumes T is the tree object
%% K is an arity 2 predicate that given the tree returns its current node key
%% left and right are also /2 and return the corresponding subtree
%% is_nil is the empty tree test
%% Cmp should gve a comparison between two key values in terms of lt, gt, lte, gte, eq
% {{{ bst initial property
qcprop(bst, {(tree, T), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,  call(GetKey, T, Key),  call(L, T, LT),  call(R, T, RT),
            qcprop(bst, {(gt, [Key]),(lt, []),(tree, LT),(curr_key, GetKey), (cmp_key, CmpKeys),
                          (left, L),(right, R),(is_nil, IsN)}),
            qcprop(bst, {(gt, []),(lt, [Key]),(tree, RT),(curr_key, GetKey), (cmp_key, CmpKeys),
                          (left, L),(right, R),(is_nil, IsN)})
        ;
            V=true, true
        ).
% }}}
% {{{ bst actual property with accumulators
qcprop(bst, {(gt, GTS), (lt, LTS), (tree, RT), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,  call(GetKey, T, Key),  call(L, T, LT),   call(R, T, RT),
            %% Key is greater then or equal to all lesser values (LTS)
            (forall(member(X, LTS), call(CmpKeys, X, Key, lte)), !
            ;  print(bst_key_not_inorder1), nl, fail),
            %% Key is less then all greater values (GTS)
            (forall(member(X, GTS), call(CmpKeys, X, Key, gt)), !
            ; print(bst_key_not_inorder2), nl, fail),
            %% subtrees are bst where Key will be in the correct key order
            qcprop(bst, {(gt, [Key|GTS]), (lt, LTS), (tree, LT),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)}),
            qcprop(bst, {(gt, GTS), (lt, [Key|LTS]), (tree, RT),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)})
        ;
            %% if this is an empty tree, it is a bst
            V=false, true   %% Height = 0
        ).
% }}}


% {{{ avl initial property (gives out height, but should it?)
qcprop(avl, {(tree, T), (height, H), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,  call(GetKey, T, Key),  call(L, T, LT),  call(R, T, RT),
            %% subtrees are avl where Key will be in the correct key order
            qcprop(bst, {(gt, [Key]), (lt, []), (tree, LT), (height, LH),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsNil)}),
            qcprop(bst, {(gt, []), (lt, [Key]), (tree, RT), (height, RH),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsNil)}),
            (abs(LH-RH) =< 1, !; print(height_mismatch), fail),
            H is 1+ max(LH,RH)
        ;
            %% if this is an empty tree, it is an avl of height 0
            V=false, H = 0
        ).
% }}}
% {{{ avl actual property with accumulators
qcprop(avl, {(gt, GTS), (lt, LTS), (tree, T), (height, H), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,
            call(GetKey, T, Key),
            call(L, T, LT),
            call(R, T, RT),
            %% Key is greater then or equal to all lesser values (LTS)
            (forall(member(X, LTS), call(CmpKeys, X, Key, lte)), !
            ;  print(bst_key_not_inorder1), nl, fail),
            %% Key is less then all greater values (GTS)
            (forall(member(X, GTS), call(CmpKeys, X, Key, gt)), !
            ; print(bst_key_not_inorder2), nl, fail),
            %% subtrees are avl where Key will be in the correct key order
            qcprop(bst, {(gt, [Key|GTS]), (lt, LTS), (tree, LT), (height, LH),
                           (curr_key, K), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsNil)}),
            qcprop(bst, {(gt, GTS), (lt, [Key|LTS]), (tree, RT), (height, RH),
                           (curr_key, K), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsNil)}),
            (abs(LH-RH) =< 1, !; print(height_mismatch), fail),
            H is 1+ max(LH,RH)
        ;
            %% if this is an empty tree, it is an avl of height 0
            V=false, H = 0
        ).
% }}}


% {{{ rbt initial property
qcprop(rbt, {(tree, T), (colour, GetColour), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        %% the root is black  and  all leafs are black
        call(GetColour, T, Colour),  (Colour = black;  print(root_node_not_black)),
        (
            %% a rbt must be a bst
            qcprop(bst, {(tree, T), (curr_key, GetKey), (cmp_key, CmpKeys),
             (left, L), (right, R), (is_nil, IsNil)}),
            V=false, !,  call(GetKey, T, Key),  call(L, T, LT),  call(R, T, RT),
            qcprop(rbt, {(tree, LT), (parent, Colour), (black_path, LN), (colour, GetColour),
             (left, L), (right, R), (is_nil, IsNil)}),
            qcprop(rbt, {(tree, RT), (parent, Colour), (black_path, RN), (colour, GetColour),
             (left, L), (right, R), (is_nil, IsNil)}),
            (LN=RN, !; print(rbt_black_node_path_not_uniform), fail)
        ;
            V=true, true
        ).
        
% }}}
% {{{ rbt actual property with accumulators
qcprop(rbt, {(tree, T), (parent, PColour), (black_path, N), (colour, GetColour), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,  call(GetKey, T, Key),  call(GetColour, T, Colour),
            %% children of red nodes are black
            (PColour = red, !, Colour = black;  print(found_red_node_with_black_child), fail),
            call(L, T, LT),  call(R, T, RT),
            qcprop(rbt, {(tree, LT), (parent, Colour), (black_path, LN), (colour, GetColour),
             (left, L), (right, R), (is_nil, IsNil)}),
            qcprop(rbt, {(tree, RT), (parent, Colour), (black_path, RN), (colour, GetColour),
             (left, L), (right, R), (is_nil, IsNil)}),
            %% every path to leaf contains same number of black nodes
            (LN=RN, !; print(rbt_black_node_path_not_uniform), fail),
            (Colour = black, !, N = LN+1; Colour = red, !, N = LN;
                print(nor_red_nor_black_node))  % nodes are black or red
        ;
            V=true,  call(GetColour, T, Colour),  Colour = black  % all leafs are black
        ).
% }}}


% {{{ heap initial property
% }}}
% {{{ heap actual property with accumulators
% }}}


% {{{ testing a copy of Yap's avl.yap module (copy in current directory)
%% we have an interface with three predicates: avl_new/1, avl_insert/4 and avl_lookup/3
%% we are supposed to create the tree and insert and lookup values

  % {{{ generator for avl commands

% we must create a generator of avl.yap uses (sequences of operations)
% this is parameterised by two disjoint generators that will make the elements for the tree and the elements for failing lookups
genAvl(KeyGen, ValGen, FGen, Calls, Size) :-
% it always starts with creating a tree
% this implies that when size is 0 the tree is still created
        Calls = [(avl:avl_new) | Cs1],
% and then is just a bunch of calls to insert and lookup values
        genAvlCmds(KeyGen, ValGen, FGen, [], Cs1, Size).

% generating random insertions and randomly choose some of those to lookup after, using difference lists for better append sublists
% zero size means zero calls
genAvlCmds(KeyGen, _ValGen, _FGen, _Lookups, [], 0) :- !. % cut - no other case if the size is 0
genAvlCmds(KeyGen, ValGen, FGen, Lookups, Cs, Size) :-
        genCmdHead(KeyGen, ValGen, FGen, Lookups, X, Size),
        call(KeyGen, Key, Size),
        (X = {i, Val},
% commands are missing the continuations, these  will be controled by the property itself
        Cs = [ {i, avl:avl_insert(Key, Val)} |CsS],
% update the possible correct lookups
        NewLookups = [ {Key,Val} | Lookups]
    ;
        X = {fl, {FK, FV}},
% just put the failing lookup
        Cs = [ {fl, avl:avl_lookup(FK, FV)} |CsS]
    ;
        X = {l, {K, V}},
        Cs = [ {l, avl:avl_lookup(K, V)} |CsS]
        ),
        MinSize is Size // 2,
        MaxSize is Size-1,
        choose(MinSize, MaxSize, NewSize, Size),
        genAvlCmds(KeyGen, ValGen, FGen, NewLookups, CsS, NewSize)
.

genCmdHead(KeyGen, ValGen, none, [], X, Size) :- !,
        plqc:structure({plqc:value(i), ValGen}, X, Size).
genCmdHead(KeyGen, ValGen, none, Lookups, X, Size) :- !,
        Insert = plqc:structure({plqc:value(i), ValGen}),
        FailLookup = plqc:structure({plqc:value(fl), {KeyGen, FGen}}),
        Lookup = plqc:structure({plqc:value(l), plqc:elements(Lookups)}),
        plqc:frequency([{6,Insert}, {4,Lookup}, {1, FailLookup}], X, Size).
genCmdHead(KeyGen, ValGen, FGen, [], X, Size) :- !,
        Insert = plqc:structure({plqc:value(i), ValGen}),
        FailLookup = plqc:structure({plqc:value(fl), {KeyGen, FGen}}),
        plqc:frequency([{9,Insert}, {1, FailLookup}], X, Size).
genCmdHead(KeyGen, ValGen, FGen, Lookups, X, Size) :-
        Insert = plqc:structure({plqc:value(i), ValGen}),
        FailLookup = plqc:structure({plqc:value(fl), {KeyGen, FGen}}),
        Lookup = plqc:structure({plqc:value(l), plqc:elements(Lookups)}),
        plqc:frequency([{6,Insert}, {4,Lookup}, {1, FailLookup}], X, Size).


genKey(Key, Size) :-
        plqc:choose(0, 50000, Key, Size).

genVal(Val, Size) :-
        plqc:choose(0, Size, Val, Size).

% qcprop(avl, {(tree, T), (height, H), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)})
  % }}}

%% we take the generator of the avl module uses and... use it :)

qcprop(avlUses) :-
        qcforall(avlTest:genAvl(genKey, genVal, structure([genVal])), Calls, qcprop({avlUses, Calls})).

qcprop({avlUses, [(avl:avl_new(Tree))|Calls]}) :-
        call(avl:avl_new(Tree)),
        qcprop({avlUses, Tree, Calls}).

qcprop({avlUses, Tree, [(Call)|Calls]}) :-
        call(avl:avl_new(Tree)),
        qcprop({avlUses, Tree, Calls}).

%% [(avl:avl_new(_A)'|'[{i,avl:avl_insert(21915,15)},{i,avl:avl_insert(44184,12)},{fl,avl:avl_lookup(25986,[0])},{i,avl:avl_insert(38292,3)},{i,avl:avl_insert(12413,0)},{fl,avl:avl_lookup(46865,[1])}])],

% }}}

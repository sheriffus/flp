% -*- mode: prolog; mode: folding -*-

:- module(avlTest).

:- source.
:- add_to_path( './src/' ).
:- reconsult(plqc).

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
            V=false, !, call(K, T Key), call(L, T, LT), call(R, T, RT),
            qcprop(bst, {(gt, [Key]), (lt, []),(tree, LT),(curr_key, K), (cmp_key, CmpKeys),
                          (left, L),(right, R),(is_nil, IsN)}),
            qcprop(bst, {(gt, []),(lt, [Key]),(tree, RT),(curr_key, K), (cmp_key, CmpKeys),
                          (left, L),(right, R),(is_nil, IsN)})
        ;
            V=true, true
        ).
% }}}
% {{{ bst actual property with accumulators
qcprop(bst, {(gt, GTS), (lt, LTS), (tree, RT), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,
            call(GetKey, T Key),
            call(L, T, LT),
            call(R, T, RT),
            %% Key is greater then or equal to all lesser values (LTS)
            (forall(member(X, LTS), call(CmpKeys, X, Key, lte)), !
            ;  print(bst_key_not_inorder1), nl, fail),
            %% Key is less then all greater values (GTS)
            (forall(member(X, GTS), call(CmpKeys, X, Key, gt)), !
            ; print(bst_key_not_inorder2), nl, fail),
            %% subtrees are bst where Key will be in the correct key order
            qcprop(bst, {(gt, [Key|GTS]), (lt, LTS), (tree, LT),
                           (curr_key, K), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)}),
            qcprop(bst, {(gt, GTS), (lt, [Key|LTS]), (tree, RT),
                           (curr_key, K), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)})
        ;
            %% if this is an empty tree, it is a bst
            V=false, true   %% Height = 0
        ).
% }}}


% {{{ avl initial property (gives out height, but should it?)
qcprop(avl, {(tree, RT), (height, H), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !, call(K, T Key), call(L, T, LT), call(R, T, RT),
            %% subtrees are avl where Key will be in the correct key order
            qcprop(bst, {(gt, [Key]), (lt, []), (tree, LT), (height, LH),
                           (curr_key, K), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)}),
            qcprop(bst, {(gt, []), (lt, [Key]), (tree, RT), (height, RH),
                           (curr_key, K), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)})
            (abs(LH-RH) <= 1, !; print(height_mismatch), fail),
            H is 1+ max(LH,RH)
        ;
            %% if this is an empty tree, it is an avl of height 0
            V=false, H = 0

            qcprop(bst, {(gt, [Key]), (lt, []),(tree, LT),(curr_key, K), (cmp_key, CmpKeys),
                          (left, L),(right, R),(is_nil, IsN)}),
            qcprop(bst, {(gt, []),(lt, [Key]),(tree, RT),(curr_key, K), (cmp_key, CmpKeys),
                          (left, L),(right, R),(is_nil, IsN)})
        ;
            V=true, true
        ).
% }}}
% {{{ avl actual property with accumulators
qcprop(avl, {(gt, GTS), (lt, LTS), (tree, RT), (height, H), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,
            call(GetKey, T Key),
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
                           (left, L),(right, R),(is_nil, IsN)}),
            qcprop(bst, {(gt, GTS), (lt, [Key|LTS]), (tree, RT), (height, RH),
                           (curr_key, K), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)})
            (abs(LH-RH) <= 1, !; print(height_mismatch), fail),
            H is 1+ max(LH,RH)
        ;
            %% if this is an empty tree, it is an avl of height 0
            V=false, H = 0
        ).
% }}}


% {{{ rbt initial property
% }}}
% {{{ rbt actual property with accumulators
% }}}


% {{{ heap initial property
% }}}
% {{{ heap actual property with accumulators
% }}}



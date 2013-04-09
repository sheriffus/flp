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
qcprop(bst, {(tree, T), (curr_key, K), (left, L), (right, R),
             (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (V=true,
        call(K, T Key),
        call(L, T, LT),
        call(R, T, RT),
        qcprop(bst,   {(gt, [Key]),
                       (lt, []),
                       (tree, LT),
                       (curr_key, K),
                       (left, L),
                       (right, R),
                       (is_nil, IsN)}),
        qcprop(bst,   {(gt, []),
                       (lt, [Key]),
                       (tree, RT),
                       (curr_key, K),
                       (left, L),
                       (right, R),
                       (is_nil, IsN)})
                   ;
        V=false,
        true).

qcprop(bst, {(gt, GTS), (lt, LTS), (tree, RT), (curr_key, K), (left, L), (right, R), (is_nil, IsN)}) :- 
        call(IsN, T, V),
        (V=true, !,
        call(K, T Key),
        
        call(L, T, LT),
        call(R, T, RT),
        qcprop(bst,   {(gt, [Key]),
                       (lt, []),
                       (tree, LT),
                       (curr_key, K),
                       (left, L),
                       (right, R),
                       (is_nil, IsN)}),
        qcprop(bst,   {(gt, []),
                       (lt, [Key]),
                       (tree, RT),
                       (curr_key, K),
                       (left, L),
                       (right, R),
                       (is_nil, IsN)})
        

A binary tree is an AVL tree if
 - Each node satisfies height-balanced-1-tree
property – the difference between the heights
of the left subtree and right subtree of the node
does not exceed one
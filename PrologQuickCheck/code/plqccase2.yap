% -*- mode: prolog; mode: folding -*-



rev(L,LR) :- rev(L,[],LR).

rev([], LR, LR).
rev([X|XS], Acc, LR) :- rev(XS, [X|Acc], LR).

prop_double_rev(Res) :-
        plqc:pcforall(list(int,L, 50), (rev(L,LR), rev(LR,L)), Res).


%% ----------------------------------------------------------

% {{{ some tests
%% tA(R1, R2, R3, R4, R5) :- plqc:ta1(R1), plqc:ta2(R2), plqc:ta3(R3), plqc:ta4(R4), plqc:ta5(R5).
%% ta1(Res) :- plqc:run( (plqc:pcforall(list(int), L, (rev(L,L), rev(LR,L)) )), Res ).
%% ta2(Res) :- plqc:run( (plqc:pcforall(list(int), L, (rev(L,LR), rev(LR,L)) )), Res ).
%% ta3(Res) :- plqc:run( (plqc:pcforall(list(int), L, (not(rev([1,2,3,4],LR)); rev(LR,L)) )), Res ).
%% ta4(Res) :- plqc:run( (plqc:pcforall(list(int), L, (rev([1,2,3,4],LR); rev(LR,L)) )), Res ).
%% ta5(Res) :- plqc:run( (plqc:pcforall(list(int), L, (rev(L,LR), rev(LR,L)) )), Res ).

tB :- tb1, tb2, tb3, tb4, tb5, tb6, tb7.

tb1  :- plqc:quickcheck( (plqc:pcforall(list(int), L, (rev(L,L), rev(LR,L)) )) ).
tb2  :- plqc:quickcheck( (plqc:pcforall(list(int), L, (rev(L,LR), rev(LR,L)) )) ).
tb3  :- plqc:quickcheck( (plqc:pcforall(list(int), L, (not(rev([1,2,3,4],LR)); rev(LR,L)) )) ).
tb4  :- plqc:quickcheck( (plqc:pcforall(list(int), L, (rev([1,2,3,4],LR); rev(LR,L)) )) ).
tb5  :- plqc:quickcheck( (plqc:pcforall(list(int), L, (rev(L,LR), rev(LR,L)) )) ).
tb6  :- plqc:quickcheck( (plqc:pcforall(list(int), L, (rev(L,LR), rev(LR,L2), !, L==L2) )) ).
tb7  :- plqc:quickcheck( (plqc:pcforall(int, I, (I<12) )) ).


tB(R1, R2, R3, R4, R5) :- plqc:tb1(R1), plqc:tb2(R2), plqc:tb3(R3), plqc:tb4(R4), plqc:tb5(R5).
tb1(Res)  :- plqc:quickcheckResult( (plqc:pcforall(list(int), L, (rev(L,L), rev(LR,L)) )), Res ).
tb2(Res)  :- plqc:quickcheckResult( (plqc:pcforall(list(int), L, (rev(L,LR), rev(LR,L)) )), Res ).
tb3(Res)  :- plqc:quickcheckResult( (plqc:pcforall(list(int), L, (not(rev([1,2,3,4],LR)); rev(LR,L)) )), Res ).
tb4(Res)  :- plqc:quickcheckResult( (plqc:pcforall(list(int), L, (rev([1,2,3,4],LR); rev(LR,L)) )), Res ).
tb5(Res)  :- plqc:quickcheckResult( (plqc:pcforall(list(int), L, (rev(L,LR), rev(LR,L)) )), Res ).

tb6(Res)  :- plqc:quickcheckResult( (plqc:pcforall(list(int), L, (rev(L,LR), rev(LR,L2), !, L==L2) )), Res ).

tb7(Res)  :- plqc:quickcheckResult( (plqc:pcforall(int, I, (I<12) )), Res ).
%% tb_prop  :- plqc:( (plqc:pcforall(int, I, (I<12) )) ).

%% hoe to expand the head of a clause into its body?

% }}}

%% bst - T is a binary search tree
%% meaning a node's key is greater than all keys in the left subtree
%% and is smaller than all keys in the right subtree
%% assumes T is the tree object
%% K is an arity 2 predicate that given the tree returns its current node key
%% left and right are also /2 and return the corresponding subtree
%% is_nil is the empty tree test
%% Cmp should gve a comparison between two key values in terms of lt, gt, lte, gte, eq
% {{{ bst initial property
pcprop({bst, (tree, T), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,  call(GetKey, T, Key),  call(L, T, LT),  call(R, T, RT),
            pcprop(bst, {(gt, [Key]),(lt, []),(tree, LT),(curr_key, GetKey), (cmp_key, CmpKeys),
                          (left, L),(right, R),(is_nil, IsN)}),
            pcprop(bst, {(gt, []),(lt, [Key]),(tree, RT),(curr_key, GetKey), (cmp_key, CmpKeys),
                          (left, L),(right, R),(is_nil, IsN)})
        ;
            V=true, true
        ).
% }}}
% {{{ bst actual property with accumulators

pcprop({bst, (gt, GTS), (lt, LTS), (tree, RT), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
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
            pcprop({bst, (gt, [Key|GTS]), (lt, LTS), (tree, LT),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)}),
            pcprop({bst, (gt, GTS), (lt, [Key|LTS]), (tree, RT),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsN)})
        ;
            %% if this is an empty tree, it is a bst
            V=false, true   %% Height = 0
        ).

% }}}


% {{{ avl initial property (gives out height, but should it?)
pcprop({avl, (tree, T), (height, H), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        pcif( (call(IsNil, T, V), V = false)
        ,(
              (call(GetKey, T, Key),  call(L, T, LT),  call(R, T, RT))
          pc_and
            %% subtrees are avl where Key will be in the correct key order
            pcprop({avl, (gt, [Key]), (lt, []), (tree, LT), (height, LH),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsNil)})
          pc_and
            pcprop({avl, (gt, []), (lt, [Key]), (tree, RT), (height, RH),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsNil)})
          pc_and
            ((abs(LH-RH) =< 1, !; print(height_mismatch), nl, fail),
            H is 1+ max(LH,RH))
        ),
            %% if this is an empty tree, it is an avl of height 0
            (V=false, H = 0)
        ).
% }}}
% {{{ avl actual property with accumulators
pcprop({avl, (gt, GTS), (lt, LTS), (tree, T), (height, H), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        pcif(
            (call(IsNil, T, V), V=false)
        ,(
            (call(GetKey, T, Key), call(L, T, LT), call(R, T, RT),
            %% Key is greater then or equal to all lesser values (LTS)
            (forall(member(X, LTS), call(CmpKeys, X, Key, lte)), !
            ;  print(avl_key_not_inorder1), nl, fail),
            %% Key is less then all greater values (GTS)
            (forall(member(X, GTS), call(CmpKeys, X, Key, gt)), !
            ; print(avl_key_not_inorder2), nl, fail))
          pc_and
            %% subtrees are avl where Key will be in the correct key order
            pcprop({avl, (gt, [Key|GTS]), (lt, LTS), (tree, LT), (height, LH),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsNil)})
          pc_and
            pcprop({avl, (gt, GTS), (lt, [Key|LTS]), (tree, RT), (height, RH),
                           (curr_key, GetKey), (cmp_key, CmpKeys),
                           (left, L),(right, R),(is_nil, IsNil)})
          pc_and
            ((abs(LH-RH) =< 1, !; print(height_mismatch), nl, fail),
            H is 1+ max(LH,RH))
        ),
            %% if this is an empty tree, it is an avl of height 0
            (V=false, H = 0)
        ).
% }}}


% {{{ rbt initial property
pcprop({rbt, (tree, T), (colour, GetColour), (curr_key, GetKey), (cmp_key, CmpKeys), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        %% the root is black  and  all leafs are black
        call(GetColour, T, Colour),  (Colour = black;  print(root_node_not_black)),
        (
            %% a rbt must be a bst
            pcprop(bst, {(tree, T), (curr_key, GetKey), (cmp_key, CmpKeys),
             (left, L), (right, R), (is_nil, IsNil)}),
            V=false, !,  call(GetKey, T, Key),  call(L, T, LT),  call(R, T, RT),
            pcprop(rbt, {(tree, LT), (parent, Colour), (black_path, LN), (colour, GetColour),
             (left, L), (right, R), (is_nil, IsNil)}),
            pcprop(rbt, {(tree, RT), (parent, Colour), (black_path, RN), (colour, GetColour),
             (left, L), (right, R), (is_nil, IsNil)}),
            (LN=RN, !; print(rbt_black_node_path_not_uniform), fail)
        ;
            V=true, true
        ).
        
% }}}
% {{{ rbt actual property with accumulators
pcprop({rbt, (tree, T), (parent, PColour), (black_path, N), (colour, GetColour), (left, L), (right, R), (is_nil, IsNil)}) :- 
        call(IsNil, T, V),
        (
            V=false, !,  call(GetKey, T, Key),  call(GetColour, T, Colour),
            %% children of red nodes are black
            (PColour = red, !, Colour = black;  print(found_red_node_with_black_child), fail),
            call(L, T, LT),  call(R, T, RT),
            pcprop(rbt, {(tree, LT), (parent, Colour), (black_path, LN), (colour, GetColour),
             (left, L), (right, R), (is_nil, IsNil)}),
            pcprop(rbt, {(tree, RT), (parent, Colour), (black_path, RN), (colour, GetColour),
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

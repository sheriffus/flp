% -*- mode: prolog; mode: folding -*-


:- module(plqccase1).
%% :- module(plqc,[quickcheck/1, quickcheck/2, zx/2]).

:- source.

:- add_to_path( './src/' ).

:- reconsult(plqc).
%% :- ensure_loaded(plqc).
%% :- use_module(plqc).



% {{{ convert between regular and difference lists
%% difference list to list
dl2l(L-[], L).
dl2l(T-T, []).
dl2l([X|XS]-T, [X|YS]) :- dl2l(XS-T, YS).

%% list to difference list
l2dl([], T-T).
%% l2dl([], [a|T]-T).  % Failed: After 1 test(s).
%% l2dl([1], [a|T]-T).  % Failed: After 71 test(s).
l2dl([X|XS], [X|L]-T) :- l2dl(XS, L-T).

%% property saying these are 'mirror' relations
%% the cuts enforce the first result they give
qcprop(l2dl2l) :- 
        plqc:qcforall( listOf(int), L, (plqccase1:l2dl(L,DL-T), !,
                                        plqccase1:dl2l(DL-T,L1), !,
                                        L == L1), 50).
% }}}

% {{{ appending regular and difference lists
append([], YS, YS).
append([X|XS], YS, [X|AS]) :-
        append(XS, YS, AS).

%% preserves variables in original lists
dappend(A-AS,B-BS,C-CS) :- duplicate_term({A-AS, B-BS},{C-X,X-CS}).
% }}}


% {{{ rotate list by one
rotate1([H|T],R) :- append(T,[H],R).

drotate1([H|T]-[H|L],T-L).

%% rotate property merge
qcprop(rotate_prop) :- 
        plqc:qcforall( listOf(int), L, plqccase1:(qcprop({rotate_permute, L}),
                                        %% qcprop({rotate_order, L}),
                                        qcprop({rotate_drotate_eq, L})
                                        )).

%% property saying rotate operation preserves size and
%% elements (the result is a permutation of the input)
qcprop({rotate_permute, L}) :- 
        plqc:qcforall( elements(L), X, 
            plqccase1:(
            rotate1(L, L1),
            same_count(X, L, L1),
            lists:length(L, N),
            lists:length(L1, N1),
            !, N = N1
            )
        ).
% {{{ same_count checks if two lists are bag-equal w.r.t. an element X
same_count(_, [], []).
same_count(X, [X|L1], L2) :- !, count_off1(X, L1, L2).
same_count(X, L1, [X|L2]) :- !, count_off1(X, L2, L1).
same_count(X, [_|L1], [_|L2]) :- same_count(X, L1, L2).

count_off1(X, L1, [X|L2]) :- !, same_count(X, L1, L2).
count_off1(X, L1, [_|L2]) :- count_off1(X, L1, L2).
% }}}

%% %% property saying rotate operation preserves circular order and
%% %% that the head of the input is the last element of the result
%% qcprop({rotate_order, L}) :- 
        

%% property stating rotate and drotate equivalence
qcprop({rotate_drotate_eq, L}) :- 
        plqccase1:rotate1(L, L1),
        plqccase1:drotate1(L, L1x),
        !,
        L1 = L1x.
% }}}




rev_app([],[]).
rev_app([X|XS], YS) :- rev_app(XS,ZS), append(ZS, [X], YS).

rev_dapp(A-A,B-B) :- var(A).
rev_dapp([X|XS]-XT, YS-YT) :- rev_dapp(XS-XR,ZS-ZR), dappend(ZS-ZR, [X]-[], YS-YR).

rev_acc([], LR, LR).
rev_acc([X|XS], Acc, LR) :- rev_acc(XS, [X|Acc], LR).

rev_dl([],T-T).
rev_dl([X|Xs],Rs-T) :- rev_dl(Xs,Rs-[X|T]).

rev_acc(L, LR) :- rev_acc(L, [], LR).


qcprop(double_rev_app) :-
        plqc:qcforall( listOf(int), L, plqccase1:qcprop({double_rev_app_body, L}), 10).

qcprop({double_rev_app_body, L}) :- 
        plqccase1:rev_app(L, LR), !, % first solution
        plqccase1:rev_app(LR, L2), !, L == L2.

qcprop(equiv_acc_app_dapp_rev) :-
        plqc:qcforall( listOf(int), L, (plqccase1:rev_acc(L,LR), plqccase1:rev_acc(LR,L))).

%% ?- plqc:quickcheck(plqccase1:qcprop(l2dl2l)).
%% ?- plqc:quickcheck(plqccase1:qcprop(double_rev)).

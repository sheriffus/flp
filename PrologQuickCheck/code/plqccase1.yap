% -*- mode: prolog; mode: folding -*-


:- module(plqccase1).
%% :- module(plqc,[quickcheck/1, quickcheck/2, zx/2]).

:- source.

:- add_to_path( './src/' ).

:- reconsult(plqc).
:- use_module(plqc).
%% :- ensure_loaded(plqc).
%% :- use_module(plqc).

:- meta_predicate qcprop(:).

% {{{ convert between regular and difference lists
%% difference list to list
dl2l(DL-R, L) :- var(R), !, duplicate_term(DL-R, L-[]).
%% dl2l(L-[], L) :- !.
%% buggy - when the tail of the dl is of the form [X|XS] where XS is var
%% and X unifies with dl's head, the tail unifies in a loop
%% dl2l([X|R]-[X|R], []) :- !.
dl2l([X|XS]-[X|R], L) :- dl2l_(XS-R, L), !.
dl2l([Y|YS]-R, [Y|YS2]) :- dl2l(YS-R, YS2).
%% dl2l(T-T, []) :- var(T).
%% dl2l([X|XS]-T, [X|YS]) :- dl2l(XS-T, YS).
dl2l_(A-B, []) :-
        (var(A), var(B))
    ;
        (nonvar(A), nonvar(B), A=[X|XS], B=[X|YS], dl2l_(XS-YS, [])).

%% list to difference list
l2dl([], T-T).
%% l2dl([], [a|T]-T).  % Failed: After 1 test(s).
%% l2dl([1], [a|T]-T).  % Failed: After 71 test(s).
l2dl([X|XS], [X|L]-T) :- l2dl(XS, L-T).

  % {{{ properties
    % {{{ prop mirror
%% property saying these are 'mirror' relations
%% the cuts enforce the first result they give
%% resulting in a functional use of the predicates
qcprop(l2dl2l) :- 
        plqc:qcforall( listOf(int), L, (plqccase1:l2dl(L,DL-T), !,
                                        plqccase1:dl2l(DL-T,L1), !,
                                        L == L1), 50).
    % }}}
    % {{{ pred l2dl2l_mirror(L)
%% a predicate specifying a generic (argument L) instance of the
%% dual nature of the l2dl/dl2l predicates
l2dl2l_mirror(L) :-
        l2dl(L,DL-T), !,
        dl2l(DL-T,L1), !,
        L == L1.
    % }}}
    % {{{ spec l2dl2l_mirror(L)
%% specification of l2dl2l_mirror predicate has the same
%% effect as qcprop(l2dl2l)
{plqccase1:l2dl2l_mirror, 1}
    of_type (L-(plqc:listOf(int)))
    where (i(g), o(g))
    has_range {1,1} % default 1-inf
    .
    % }}}
    % {{{ l2dl x
{plqccase1:l2dl, x}
    of_type (L-(plqc:listOf(int)), DL-(plqc:variable))
    where (i(g, v), o(g, g)) % this 'out' fails
    has_range {1,1} % default 1-inf
    .
    % }}}
    % {{{ l2dl 1
%% individual specification of l2dl and dl2l
{plqccase1:l2dl, 1}
    of_type (L-(plqc:listOf(int)), DL-variable)
    where (i(g, v), o(g, ngv)) % an open dif list is neither ground or variable
    has_range {1,1} % default 1-inf
    .
    % }}}
    % {{{ %% a difference list generator
difListOf(GenA, DL, S) :-
        %% choose a number of elements to generate
        plqc:choose(0, S, K1, S),
        %% choose the number of elements that will be subtracted
        %% plqc:choose(0, K1, K2, S),
        plqc:choose(0, K1, Tmp, S),
        plqc:choose(0, Tmp, K2, S), % this double step makes a smaller list
        %% leaving the actual list members
        K is K1 -K2,
        %% get K1 elements
        plqc:vectorOf(K1, GenA, AS, S),
        %% and 'make a difference'
        mk_dif_aux(K, AS, DL).

mk_dif_aux(0, AS, DL) :- !, open_list(AS, L), DL = L-L.
mk_dif_aux(K, [A|AS], [A|DL]-R) :-
        K1 is K-1,
        mk_dif_aux(K1, AS, DL-R).

open_list([], L).
open_list([A|AS], [A|L]) :- open_list(AS, L).
    % }}}
    % {{{ dl2l 1
{plqccase1:dl2l, 1}
    of_type (DL-(plqc:variable), L-(plqc:listOf(int)))
    where (i(v, g), o(g, g))
    has_range {1,1} % default 1-inf
    .
    % }}}
    % {{{ dl2l 2
{plqccase1:dl2l, 2}
    of_type (L-(plqccase1:difListOf(int)), DL-(plqc:variable))
    where (i(ngv, v), o(ngv, g))
    %% where (i(ngv, v), o(g, g), o(ngv, g))
    has_range {1,1} % default 1-inf
    post_cond l2dl(L, DL)
    .
    % }}}
    % {{{ dl2l 3
{plqccase1:dl2l, 3}
    of_type (DL-(plqccase1:difListOf(int)), L-(plqc:variable))
    where (i(ngv, v), o(ngv, g))
    %% where (i(ngv, v), o(g, g), o(ngv, g))
    has_range {1,1} % default 1-inf
    pre_cond (var(L), 1\=2)
    post_cond (l2dl(L, DL1), !, DL=DL1)
    .
    % }}}
  % }}}

% }}}

% {{{ appending regular and difference lists
append([], YS, YS).
append([X|XS], YS, [X|AS]) :-
        append(XS, YS, AS).

    % {{{ append _
 plqccase1:append
    of_type (A-(plqc:listOf(int)), B-(plqc:listOf(int)), C-(plqc:variable))
    where (i(g, g, v), o(g, g, g))
    has_range {1,1}.
    % }}}

    % {{{ append 1
{plqccase1:append, 1}
    of_type (A-(plqc:listOf(int)), B-(plqc:variable), C-(plqc:variable))
    where (i(g, v, v), o(g, v, ngv), o(g, v, v)) % when L1 is [], LApp is L2, i.e. var
    has_range {1,1} % default 1-inf
    .
    % }}}
    % {{{ append 2
{plqccase1:append, 2}
    of_type (A-(plqc:variable), B-(plqc:listOf(int)), C-(plqc:variable))
    where (i(v, g, v), o(g, g, g), o(ngv, g, ngv))
    has_range {1,inf} % default 1-inf
    .
    % }}}
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


% {{{ double reverse is id

qcprop(double_rev_app) :-
        plqc:qcforall( listOf(int), L, plqccase1:qcprop({double_rev_app_body, L})).

qcprop({double_rev_app_body, L}) :- 
        plqccase1:rev_app(L, LR), !, % first solution
        plqccase1:rev_app(LR, L2), !, L == L2.

qcprop(double_rev_acc) :-
        plqc:qcforall( listOf(int), L, plqccase1:qcprop({double_rev_acc_body, L})).

qcprop({double_rev_acc_body, L}) :- 
        plqccase1:rev_acc(L, LR), !, % first solution
        plqccase1:rev_acc(LR, L2), !, L == L2.

% }}}

qcprop(wrong_drev) :-
        plqc:qcforall( listOf(int), XS, (plqccase1:rev_acc(XS,RX), plqccase1:rev_app(RX,RX))).

% {{{ reverse implementations should be equiv

qcprop(equiv_acc_app) :-
        plqc:qcforall( listOf(int), L, (plqccase1:rev_acc(L,LR), plqccase1:rev_app(L,LR))).

% }}}

% {{{ index check on reversed list

qcprop(rev_app_index) :-
        plqc:qcforall( suchThat(structure({listOf(int), int}), plqccase1:valid_index), {L,I},
                       plqccase1:qcprop({double_rev_index_body, L, I})).
valid_index({L, I}) :-
        length(L,X), I<X. 

qcprop({double_rev_index_body, L, I}) :- 
        plqccase1:rev_app(L, LR), !, % first solution
        length(L,X),
        Index is I+1, RevIndex is X-I,
        lists:nth(Index, L, Val),
        lists:nth(RevIndex, LR, Val).

% }}}

%% ?- plqc:quickcheck(plqccase1:qcprop(l2dl2l)).
%% ?- plqc:quickcheck(plqccase1:qcprop(double_rev)).

test(Arg1, Arg2) :- var(Arg2).%, print({t1, Arg1, Arg2}), nl.
test(Arg1, [Arg2]) :- true.%print({t2, Arg1, Arg2}), nl.
test(Arg1, [Arg2]) :- Arg2=0. %, print({t3, Arg1, Arg2}), nl.
%% test(Arg1, [X]).
%% test(Arg1, [X, Y|XS]) :- test(Arg1, [Y|XS]).

%% TODO - infer module for the specifications co-located with their predicates
{plqccase1:test, 1} of_type (plqc:listOf(int), plqc:variable).
{plqccase1:test, 2} of_type (L-(plqc:listOf(int)), plqc:variable) such_that plqccase1:odd_arg1.
{plqccase1:test, 3} of_type (L-(plqc:listOf(int)), plqc:variable) such_that plqccase1:odd_arg1 post_cond (L \= []).
{plqccase1:test, 4} of_type (L-(plqc:listOf(int)), plqc:variable) such_that plqccase1:odd_arg1 has_range {1,1} post_cond (L \= []).

{plqccase1:test, 5}
    of_type (L-(plqc:listOf(int)), plqc:variable)
    such_that plqccase1:odd_arg1
    where (i(g,v), o(g,v), o(g,ng), o(g,g))
    has_range {0,inf} % default 1-inf
    limit 3
    post_cond (L \= []).

%% t1 :- {plqccase1:test, 1} of_type (plqc:listOf(int), plqc:variable).
%% t2 :- {plqccase1:test, 2} of_type (L-(plqc:listOf(int)), plqc:variable) such_that plqccase1:odd_arg1.
%% t3 :- {plqccase1:test, 3} of_type (L-(plqc:listOf(int)), plqc:variable) such_that plqccase1:odd_arg1 post_cond (L \= []).
%% t4 :- {plqccase1:test, 4} of_type (L-(plqc:listOf(int)), plqc:variable) such_that plqccase1:odd_arg1 has_range {1,1} post_cond (L \= []).
%% t5 :- {plqccase1:test, 5}
%%     of_type (L-(plqc:listOf(int)), plqc:variable)
%%     such_that plqccase1:odd_arg1
%%     where (i(g,v), o(g,v), o(g,ng), o(g,g))
%%     has_range {0,inf} % default 1-inf
%%     limit 2
%%     post_cond (L \= []).


odd_arg1([L, _V]) :- odd_list(L).

odd_list([]).
odd_list([X|XS]) :- 0 is X mod 2, odd_list(XS).

%% {rev_app, 1} of_type (listOf(int), variable).
%% {rev_app, a} of_type (listOf(int), variable) such_that true has_range {1,1}.
%% {rev_app, 2} of_type (variable, listOf(int)) such_that true has_range {1,1}.

%% coiso of_type _A has_range _B.

%    ?- opts:parse([], Opts), state:init(Opts, IState), trace, plqc:test(true, Opts, IState, OState, Result).


%% ?- plqc:quickcheck(plqccase1:qcprop(spec_rev_app_1)).
%% ?- plqc:quickcheck(plqccase1:qcprop(spec_test_1)).
%% ?- plqccase1:qcprop(spec_rev_app_1).
%% ?- plqccase1:qcprop(spec_test_1).
   %% ?- plqc:clause(plqccase1:qcprop(spec_test_1), L).
   %% ?- clause(plqccase1:qcprop(spec_test_1), L).
   %% ?- plqccase1:qcprop(spec_test_1).
   %% ?- qcprop(spec_test_1).
   %% ?- clause(plqccase1:t, X), user:expand_term(X, Y).

   %% ?- reconsult(plqccase1).
   %% ?- clause(plqccase1:t, X), user:expand_term(X, Y).
   %% ?- plqccase1:qcprop(spec_test_1).
   %% ?- clause(plqccase1:qcprop(spec_test_1),X).
   %% ?- clause(plqccase1:t, X), user:expand_term(X, Y).
   %% ?- plqc:quickcheck(plqccase1:qcprop(spec_test_1)).
   %% ?- plqc:quickcheck(plqccase1:qcprop(spec_test_1)).
   %% ?- call(call(plqccase1:test, a),b).
   %% ?- plqccase1:test(a,b).

qcprop(int10) :- 
        plqc:qcforall( int, I, (I =< 10)).

%% plqc:quickcheck(plqccase1:qcprop(spec_rev_app_1)).
%% plqc:quickcheck(plqccase1:qcprop(spec_test_1)).
%% plqc:quickcheck(plqccase1:qcprop(spec_test_2)).
%% plqc:quickcheck(plqccase1:qcprop(spec_test_3)).
%% plqc:quickcheck(plqccase1:qcprop(spec_test_4)).
%% plqc:quickcheck(plqccase1:qcprop(spec_test_5)).
%% plqc:quickcheck((plqccase1:qcprop(double_rev_app)) qcand (plqccase1:qcprop(double_rev_acc))).

%% $ yap
%%    ?- reconsult(plqccase1).
%%    ?- use_module(plqccase1).
%%    ?- plqc:quickcheck(plqccase1:qcprop(double_rev_app)).
%% ....................................................................................................
%% OK: Passed 100 test(s).
%% yes
%%    ?- plqc:quickcheck(plqccase1:qcprop(double_rev_acc)).
%% ....................................................................................................
%% OK: Passed 100 test(s).
%% yes
   %% ?- plqc:quickcheck(plqccase1:qcprop(wrong_drev)).
   %% | quickcheck(qcprop(double_rev_acc)).
   %% | reconsult(plqc).
   %% | reconsult(plqccase1).
   %% | quickcheck(plqccase1:qcprop(double_rev_acc)).
   %% | use_module(plqccase1).
   %% | oneof([int, value(a)],X,10).
   %% | use_module(plqc).
   %% | elements(int,X,10).
   %% | plqc:elements(int,X,10).

%%      plqc:quickcheck(plqccase1:qcprop(double_rev)).
%%      quickcheck(plqccase1:qcprop(double_rev)).
%%      quickcheck(qcprop(double_rev)).

qcprop(double_rev) :-
        qcforall( listOf(int), L, qcprop({drev, L})).

qcprop({drev, L}) :- 
        rev_app(L, LR), !, % first solution
        rev_app(LR, L2), !, L == L2.

qcprop(w_drev) :-
        qcforall( listOf(int), XS, (rev_acc(XS,RX), rev_app(RX,RX))).


%% data Var = V String

%% data Term = Var Var
%%           | Func String [Term]

%% data Atom = PredC String [Term]

%% -- idempotent mapping
%% -- identity at all but finitely many points
%% data Subst = S [(Var,Term)]
%%            | F

%% compose :: Subst -> Subst -> Subst

%% -- s1 more general than s2 if there is s3 s.t.
%% -- s2 = s3.s1

%% -- t1, t2 are unifiable if there exists a subst s s.t.
%% -- s(t1) = s(t2)
%% -- s is the unifier

%% -- if t1, t2 have a unifier, they have a most general unifier mgu(t1,t2)
%% -- unique up to variable renaming

% Program = Prog [PredDef]

% PredicateDefinition = {String, [Clause]}

%-- a clause has one positive and a list of negative atoms
%-- a.k.a. head and body
% Clause = {Atom, [Atom]}

% Goal = [Atom]

% Frame = {Goal, [Var]}

% Stream a = nil | bottom |  [ a | Stream a ]


stream(bottom, _, bottom).
stream([], S, S).
stream([X|XS], S, [X|YS]) :- stream(XS, S, YS).

%interp :: [( [Frame], Subst, [Clause] )] -> [Clause] -> Stream Subst
interp( [],  _ , [] ) :-
    !, print(1), nl.
interp([ ( [],                    S,  _)     |St], Prog,  [S|SS]) :-
    !, print(2), nl,
    interp( St, Prog, SS).
interp([ ( [ F0             |F1], S,  [])    |St], Prog, SS) :-
    !, print(3), nl,
    interp( St, Prog, SS).
interp([ ( [ ([G|GS], Vars) |F0], S, [C|CS] ) |St0], Prog, SS) :-
    print(xxx),
    (G = C,
    !, print(4), nl
, print([rename(C,dom_S),
    unify(s(G), C, teta),
    interp([ ([ (body(C), dom_S), (GS, Vars) |F0],S,CS), ([([G|GS], Vars)|F0],teta(S),CS) |St0], Prog, SS) ])
) ; (
    !, print(5), nl
, print( [ interp([([([G|GS], Vars)|F0],S,CS)|St0], Prog, SS) ] )
)
. %% ,
    %% unify c (h goal) ==   interp ((f2:f1:fs,s1,cs1):st) prog,
    %% interp([|St1], Prog).
interp([ ( [ ([],     Vars) |F0], S,  CS    ) |St0], Prog, SS) :-
    !, print(6), nl. %% ,
    %% interp([(F0,(S\Vars,P))|St], Prog, SS).
interp([X|St0], Prog, SS) :-
    print(none), nl,
    print(X)
.


semantic_of(GS) :-
    print( interp( [ ([ (GS, vars_of_goal) |[]], empty, prog) |[]], prog, SS) )
,nl.
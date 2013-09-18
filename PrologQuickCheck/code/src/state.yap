% -*- mode: prolog; mode: folding -*-

:- module(state).

% {{{ creating state

  % {{{ default(State)
default(State) :-
        opts:default(Opts),
        init(Opts, State).
  % }}}

  % {{{ init(Opts, State)
init(Opts, State) :-
        opts:start_size(Opts, StartSize),
        opts:constraint_tries(Opts, CTries),
        opts:any_type(Opts, AnyType),
        %% opts:seed(Opts, Seed),
        OSize is StartSize - 1,
        put_size(OSize, [], State1),
        put_left(0, State1, State2),
        grow_size(Opts, State2, State3),
        put_constraint_tries(CTries, State3, State4),
        put_any_type(AnyType, State4, State5),
        % {{{ TODO when choosing a seed is possible
        % }}}
        State = State5.
  % }}}

% }}}


% {{{ changing state

  % {{{ grow_size
grow_size(Opts, IState, OState) :-
        opts:max_size(Opts,MaxSize),
        get_size(IState, Size),
        ( (
            Size < MaxSize, !,
            get_left(IState, Left),
            (
                Left == 0, !,
                OSize is Size + 1,
                put_size(OSize, IState, OState1),
                tests_at_size(OSize, Opts, K),
                (
                    K == 0, !,
                    grow_size(Opts, OState1, OState)
                ;
                    N is K -1,
                    put_left(N, OState1, OState)
                )
            ;
                OLeft is Left -1,
                put_left(OLeft, IState, OState)
            )
          )
        ;
            IState = OState
        ).

    % {{{ tests_at_size(Size, Opts, K)

tests_at_size(Size, Opts, K) :-
        opts:numtests( Opts, NumTests ),
        opts:start_size( Opts, StartSize ),
        opts:max_size( Opts, MaxSize ),
        SizesToTest is MaxSize - StartSize + 1,
        (
            NumTests >= SizesToTest, !,
	    TotalOverflow is NumTests rem SizesToTest,
            (
                MaxSize - Size < TotalOverflow, !,
                Overflow = 1
            ;
                Overflow = 0
            ),
	    K is NumTests // SizesToTest + Overflow
        ;
            EverySoManySizes is SizesToTest // NumTests,
            (
                (Size - StartSize) rem EverySoManySizes == 0, !,
                K = 1
            ;
                K = 0
            )
        ).

    % }}}
  % }}}

  % {{{ put_X

put_left(Left, [], [{left, Left}]).
put_left(Left, [{left, _} | RSt], [{left, Left} | RSt]) :- !.
put_left(Left, [X | RSt], [X | NewRSt]) :-
        put_left(Left, RSt, NewRSt).

put_size(Size, [], [{size, Size}]).
put_size(Size, [{size, _} | RSt], [{size, Size} | RSt]) :- !.
put_size(Size, [X | RSt], [X | NewRSt]) :-
        put_size(Size, RSt, NewRSt).

put_constraint_tries(CTries, [], [{constraint_tries, CTries}]).
put_constraint_tries(CTries, [{constraint_tries, _} | RSt],
                             [{constraint_tries, CTries} | RSt]) :- !.
put_constraint_tries(CTries, [X | RSt], [X | NewRSt]) :-
        put_constraint_tries(CTries, RSt, NewRSt).

put_any_type(AnyType, [], [{any_type, AnyType}]).
put_any_type(AnyType, [{any_type, _} | RSt], [{any_type, AnyType} | RSt]) :- !.
put_any_type(AnyType, [X | RSt], [X | NewRSt]) :-
        put_any_type(AnyType, RSt, NewRSt).

  % }}}


% }}}


% {{{ accessing state

  % {{{ get_X

get_left([{left, Left} | _], Left) :- !.
get_left([_ | RSt], Left) :- get_left(RSt, Left).
get_left([], _) :- print(error_get_left).

get_size([{size, Size} | _], Size) :- !.
get_size([_ | RSt], Size) :- get_size(RSt, Size).
get_size([], _) :- print(error_get_size).

get_constraint_tries([{constraint_tries, CTries} | _], CTries) :- !.
get_constraint_tries([_ | RSt], CTries) :- get_constraint_tries(RSt, CTries).
get_constraint_tries([], _) :- print(error_get_constraint_tries).

get_any_type([{any_type, AnyType} | _], AnyType) :- !.
get_any_type([_ | RSt], AnyType) :- get_any_type(RSt, AnyType).
get_any_type([], _) :- print(error_get_any_type).

  % }}}

% }}}

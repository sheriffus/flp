% -*- mode: prolog; mode: folding -*-


:- module(plqc).
%% :- module(plqc,[quickcheck/1, quickcheck/2, zx/2]).

:- reconsult(opts).
:- reconsult(result).
:- reconsult(ctx).
:- reconsult(state).

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(timeout)).

:- source.

:- op(910, xfx, of_type).
:- op(700, xfx, such_that).
:- op(750, xfx, where).
:- op(800, xfx, has_range).
:- op(850, xfx, limit).
:- op(900, xfx, obbeys).

% {{{ qc top predicates

%% | Tests a property and prints the results to 'stdout'.
quickcheck(Property) :-
        quickcheck(Property, []).

%% | Tests a property, using test arguments, and prints the results to 'stdout'.
quickcheck(Property, UserOpts)  :-
        quickcheckResult(Property, UserOpts, _Result).

%% | Tests a property, produces a test result, and prints the results to 'stdout'.
quickcheckResult(Property, Result) :-
        quickcheckResult(Property, [], Result).

%% | Tests a property, using test arguments, produces a test result, and prints the results to 'stdout'.
quickcheckResult(Property, UserOpts, Result) :-
        opts:parse(UserOpts, Opts),
        state:init(Opts, IState),
        test(Property, Opts, IState, OState, Result)
.

% }}}

% {{{ test

  % {{{ test(Property, Opts, IState, OState, Result)

%% | Tests a property, based on options, carrying a state, produces a test result, and prints the results to 'stdout'.
test(Property, Opts, IState, OState, Result) :-
        %% retrieve specific attributes for testing behaviour
        opts:numtests(Opts,  NumTests),
        opts:long_result(Opts, ReturnLong),
        opts:output_fun(Opts, Print),
        max_tries_factoring(NumTests, N),
        %% perform tests on Property
        perform(0, NumTests, N, Property, Opts, IState, OState, Result1),
        %% report result with printing Print predicate
        call_with_args(Print,"~n", []),
        report_result(Result1, Opts),
        %% polish gross result into long/short testing result
        refine_result(Result1, Property, Opts, ShortRes, LongRes),
        (ReturnLong == true, !,
        Result = LongRes
    ;
        Result = ShortRes
        )
.

  % }}}

  % {{{ refine_result(Result1, Property, Opts, ShortRes, LongRes)

refine_result(Result, Property, Opts, ShortRes, LongRes) :-
        (result:is_pass_res(Result), !,
        ShortRes = true,
        LongRes = true
    ;
        result:is_fail_res(Result), !,
        result:reason_fail(Result, Reason),
        result:bound_fail(Result, Bound),
        %% TODO - shrink here if counterexample (expected pass)
        ShortRes = false,
        LongRes = Bound
    ;
        ShortRes = Result,
        LongRes = Result
        )
    .

  % }}}

  % {{{ report_result
    % {{{ report_result

report_result( Result, Opts ) :-
        opts:expect_fail(Opts, ExpectF),
        opts:output_fun(Opts, Print),
        (
         ( %% passing
             result:is_pass_res(Result), !,
             result:printers_pass(Result, Printers),
             result:performed_pass(Result, Performed),
             (
              ( %% expected fail
                  ExpectF = true, !,
                  call_with_args(Print, "Failed: All ~d tests passed when a failure was expected.~n", [Performed])
              )
             ;
              ( %% expected pass
                  call_with_args(Print, "OK: Passed ~d test(s).~n", [Performed])
              )
             )
         )
        ;
         ( %% failing
             result:is_fail_res(Result), !,
             result:performed_fail(Result, Performed),
             (
              ( %% expected fail
                  ExpectF = true, !,
                  call_with_args(Print, "OK: Failed as expected, after ~d test(s).~n", [Performed])
              )
             ;
              ( %% expected pass
                  call_with_args(Print, "Failed: After ~d test(s).\n", [Performed]),
                  report_fail_reason(Reason, "", Print),
                  result:bound_fail(Result, Bound),
                  print_testcase(Bound, Print)%% ,
                  %% execute_actions(Actions)
              )
             )
         )
        ;
         (
             Result = {error, Reason}, !,
             report_error(Reason, Print)
         )
        )
    .

    % }}}

    % {{{ print_testcase

%% TODO: print each quantified variable separately
print_testcase([], _Print).% :- !.
print_testcase(Bound, Print) :-
        call_with_args(Print, "Counterexample found: ~w \n", [Bound]).

    % }}}

    % {{{ TODO report_error

report_error(Reason, Print) :- print(report_error_predicate_missing).

   % }}}

    % {{{ report_fail_reason

report_fail_reason(false_prop, _Prefix, _Print) :- !.
report_fail_reason(time_out, Prefix, Print) :-
        lists:append(Prefix, "Test execution timed out.~n", Msg),
        call_with_args(Print, Msg, []).
report_fail_reason({trapped,ExcReason}, Prefix, Print) :-
        lists:append(Prefix, "A linked process died with reason ~a.~n", Msg),
        call_with_args(Print, Msg, [ExcReason]).
report_fail_reason({exception,ExcKind,ExcReason,StackTrace}, Prefix, Print) :-
        lists:append(Prefix, "An exception was raised: ~a:~a.~n", Msg1),
        call_with_args(Print, Msg1, [ExcKind,ExcReason]),
        lists:append(Prefix, "Stacktrace: ~a.~n", Msg2),
        call_with_args(Print, Msg2, [StackTrace]).
%% report_fail_reason({sub_props,SubReasons}, Prefix, Print) ->
%%     Report =
%% 	fun({Tag,Reason}) ->
%% 	    Print(Prefix ++ "Sub-property ~w failed.~n", [Tag]),
%% 	    report_fail_reason(Reason, ">> " ++ Prefix, Print)
%% 	end,
%%     lists:foreach(Report, SubReasons),
%%     ok.

    % }}}
  % }}}

% }}}

% {{{ perform(NumTestsPassed, NumSuccessTests, TriesLeft, Property, ...)

%% main test loop

% reached limit of tries and passed no test - can't satisfy
perform(0, _ToPass, 0, _Property, _Opts, _IState, _OState, Result) :-
        !, Result = {error, cant_satisfy}.
% reached limit of tries and passed some tests - make a partial passing result
perform(Passed, _ToPass, 0, _Property, _Opts, _IState, _OState, Pass) :-
        !, result:mk_pass([{performed,Passed}], Pass).
% reached intended number of successfull tests
perform(ToPass, ToPass, _TriesLeft, _Property, _Opts, _IState, _OState, Pass) :-
        !, result:mk_pass([{performed,ToPass}], Pass).    
perform(Passed, ToPass, TriesLeft, Property, Opts, IState, OState, Result) :-
        duplicate_term(Property, Test),
        run(Test, Opts, IState, State1, Res),
      (
        (
            result:is_pass_res(Res),
            result:reason_pass(Res, true_prop), !,
            opts:output_fun(Opts, Print),
            call_with_args(Print,".", []),
            state:grow_size(Opts, State1, State2),
            Passed1 is Passed + 1,
            TriesLeft1 is TriesLeft - 1,
            perform(Passed1, ToPass, TriesLeft1, Property, Opts, State2, OState, Result)
        );(
            result:is_fail_res(Res), !,
            opts:output_fun(Opts, Print),
            call_with_args(Print,"!", []),
            Passed1 is Passed + 1,
            result:new_performed_fail(Res, Passed1, Result),
            OState = State1
        );(
            Res = {error, rejected}, !,
            opts:output_fun(Opts, Print),
            call_with_args(Print,"x", []),
            state:grow_size(Opts, State1, State2),
            TriesLeft1 is TriesLeft - 1,
            perform(Passed, ToPass, TriesLeft1, Test, Samples, Printers, Opts, State2, OState, Result)
        );(
            (   Res = {error, arity_limit}
            ;   Res = {error, cant_generate}
            ;   Res = {error, non_boolean_result}
            ;   Res = {error, type_mismatch}
            ;   Res = {error, {typeserver,SubReason}}
            ), !,
            Res = Result,
            OState = State1
        );(
            Result = {error, {unexpected,Other}},
            OState = State1
        )
      ).

% }}}

% {{{ run

run(Test, Result) :-
        opts:default(Opts),
        run(Test, Opts, Result).
run(Test, Opts, Result) :-
        ctx:default(Ctx),
        run(Test, Opts, Ctx, Result).
run(Test, Opts, Ctx, Result) :-
        state:init(Opts, IState),
        run(Test, Opts, Ctx, IState, OState, Result).

run(Test, Opts, IState, OState, Result) :-
        ctx:default(Ctx),
        run(Test, Opts, Ctx, IState, OState, Result).
%% current module
run(Mod:Test, Opts, Ctx, IState, OState, Result) :- 
        ctx:new_module(Ctx, Mod, Ctx1),
        run(Test, Opts, Ctx1, IState, OState, Result).
%% universal quantification
run(qcforall(Gen, Var, Test), Opts, Ctx, IState, OState, Result) :- 
        !,
        state:get_size(IState,Size),
        bind_forall(Gen, Ctx, Var, Size),
        ctx:bind(Ctx, Var, Ctx1),
        run(Test, Opts, Ctx1, IState, OState, Result).
%% explicitly sized quantification
run(qcforall(Gen, Var, Test, Size), Opts, Ctx, IState, OState, Result) :- 
        !,
        bind_forall(Gen, Ctx, Var, Size),
        ctx:bind(Ctx, Var, Ctx1),
        run(Test, Opts, Ctx1, IState, OState, Result).
%% a labeled property to be unfolded; requires compilation with 'source'
%% Label is either the identifier (atom that identifies the property)
%%  or a tuple with the identifier and the arguments
run(qcprop(Label), Opts, Ctx, IState, OState, Result) :-
        !,
        ctx:module(Ctx, M),
        clause(M:qcprop(Label), Body),
        run(Body, Opts, Ctx, IState, OState, Result).
%% conjunction - individual calls
run((Test, Tests), Opts, Ctx, IState, OState, Result) :- 
        run(Test, Opts, Ctx, IState, State1, Result1),
        cond_run(Result1, Tests, Opts, Ctx, State1, OState, Result).
%% a leaf in the property syntax tree - a predicate call
run(Test, Opts, Ctx, State, State, Result) :- 
        ctx:module(Ctx, M),
        (call(M:Test), !,
        create_pass_result(Ctx, true_prop, Result)
        )
    ;
        (!,
        create_fail_result(Ctx, false_prop, Result)
        ).
%% TODO: prolog conjunction and disjunction
%% TODO: collect information for user analysis 

cond_run(Result1, Tests, Opts, Ctx, IState, OState, Result) :-
        (
            result:is_pass_res(Result), !,
            run(Tests, Opts, Ctx, IState, OState, Result)
        ) ; (
            result:is_fail_res(Result), !,
            IState = Ostate,
            Result = Result1
        ).

  % {{{ bind_forall(Gen, Ctx, Var, Size)
bind_forall(M:Gen, _Ctx, Var, Size) :- 
        call_with_args(M:Gen, Var, Size).
bind_forall(Gen, Ctx, Var, Size) :- 
        ctx:module(Ctx, M),
        call_with_args(M:Gen, Var, Size).
  % }}}

% }}}

% {{{ create result

create_pass_result(Ctx, Reason, Pass) :-
        result:mk_pass([{reason, Reason}], Pass).

create_fail_result(Ctx, Reason, Fail) :-
        ctx:bound(Ctx, Bound),
        result:mk_fail([{reason, Reason}, {bound, Bound}], Fail).

% }}}

% {{{ generators and properties -- should have their own module or at least file

  % {{{ integers
%% generator
int(I,Size) :- choose(0, Size, I, Size). %% random:random(0,Size,I).

%% shrink interface
int(0, _, []) :- !.
int(I, shrink, Shrs) :- shr_int(I, Shrs).
int(I, shrink, [0]).

%% %% shrinker
%% shr_int(I, L) :-
%%         I =< 0, !,
%%         L = []
%%     ;
%%         random:random(0,I,I2),
%%         I3 is I-1,
%%         L = [I2, I3]
%%     .
  % }}}

  % {{{ common generator stuff

%% %% | Used to construct generators that depend on the size parameter.
%% sized(SGenA, A, Size) :- call_with_args(SGenA, A, Size).

%% | Overrides the size parameter. Returns a generator which uses
%% the given size instead of the runtime-size parameter.
resize(NewSize, GenA, A, _Size) :- call_with_args(GenA, A, NewSize).

%% | Generates a random element in the given inclusive range.
%% TODO - make this for other 'rangeable' types
%% TODO - define what can be 'ranged' for the previous point
choose(Min,Max, A, _Size) :-
        Cap is Max+1,
        random:random(Min,Cap,A). % a value between Min and Cap-1

%% | Generates some example values.
sampleDefaultSize(20).
sampleDefaultK(100).
sampleSizeStep(Size, S1) :- S1 is Size. % constant size sampling

sample(GenA, L) :-
        sampleDefaultK(K),
        sampleK(K, GenA, L).

sampleK(K, GenA, L) :-
        sampleDefaultSize(S),
        sampleKSized(K, GenA, S, L).

sampleKSized(0, _GenA, _Size, []) :- !.
sampleKSized(K, GenA, Size, [A|AS]) :-
        call_with_args(GenA, A, Size),
        sampleSizeStep(Size, S1),
        K1 is K-1,
        sampleKSized(K1, GenA, S1, AS).


%% | Generates some example values and prints them to 'stdout'.
showSample(GenA) :- 
        sample(GenA,L),
        nl, print(L), nl.

%%------------------------------------------------------------------------
%% ** Common generator combinators

%% | Generates a value that satisfies a predicate.
%% use with caution, may take a long time if PredA is hard to satisfy
stDefaultSize(2).
stSizeStep(S, S1) :- S1 is S+1. % growing size sampling

suchThat(GenA, PredA, A) :-
        stDefaultSize(S),
        suchThat(GenA, PredA, A, S).
suchThat(GenA, PredA, A, S) :-
        (
            suchThatMaybe(GenA, PredA, A, S), !
        ;
            stSizeStep(S, S1),
            suchThat(GenA, PredA, A, S1)            
        ).

%% | Tries to generate a value that satisfies a predicate.
suchThatMaybe(GenA, PredA, A) :-
        stDefaultSize(S),
        suchThatMaybe(GenA, PredA, A, S).
suchThatMaybe(GenA, PredA, A, S) :-
        call_with_args(GenA, A, S),
        call_with_args(PredA, A).

%% | Randomly uses one of the given generators. The input list
%% must be non-empty.
%% TODO - oneof [] = error "plqc:oneof used with empty list"
oneof(LGenA, A, S) :-
        length(LGenA, Len),
        Cap is Len-1,
        choose(0,Cap,I,S),
        lists:nth0(I, LGenA, GenA),
        call_with_args(GenA, A, S).

%% | Chooses one of the given generators, with a weighted random distribution.
%% The input list must be non-empty. The weights must be positive integers.
%% TODO - frequency [] = error "plqc:frequency used with empty list"
frequency(FGL, A, S) :- % Frequency-Generator List
        %% make freq-index list according to weights and calculate choosing cap
        checkFreqWeights(FGL, FIL, Cap),
        choose(1,Cap,I,S), % choose an index
        lists:nth0(I, FIL, GenA),
        call_with_args(GenA, A, S).

checkFreqWeights([], [], -1).
checkFreqWeights([{W,Gen}|FGS], FIL, Cap) :-
        checkFreqWeights(FGS, FIL1, Cap1),
        addHeads(W,Gen, FIL1, FIL),
        Cap is Cap1 + W.

addHeads(0, _Gen, L, L).
addHeads(K, Gen, LI, LO) :-
        K1 is K-1,
        addHeads(K1, Gen, [Gen|LI], LO).


%% | Generates one of the given values. The input list must be non-empty.
%% TODO - elements [] = error "plqc:elements used with empty list"
elements(AS, A, S) :-
        length(AS, Cap),
        choose(1,Cap,I,S),
        lists:nth1(I, AS, A).
        
%% %% | Takes a list of elements of increasing size, and chooses
%% %% among an initial segment of the list. The size of this initial
%% %% segment increases with the size parameter.
%% %% The input list must be non-empty.
%% growingElements :: [a] -> Gen a
%% growingElements [] = error "QuickCheck.growingElements used with empty list"
%% growingElements xs = sized $ \n -> elements (take (1 `max` size n) xs)
%%   where
%%    k      = length xs
%%    mx     = 100
%%    log'   = round . log . fromIntegral
%%    size n = (log' n + 1) * k `div` log' mx

%% {- WAS:                                                                              
%% growingElements xs = sized $ \n -> elements (take (1 `max` (n * k `div` 100)) xs)ctr
%%  where
%%   k = length xs
%% -}
%% newtype Gen a = MkGen{ unGen :: StdGen -> Int -> a }

%% | Generates a list of random length. The maximum length depends on the
%% size parameter.
listOf(GenA, AS, S) :-
        choose(0, S, K, S),
        vectorOf(K, GenA, AS, S).

%% | Generates a non-empty list of random length. The maximum length 
%% depends on the size parameter.
listOf1(GenA, AS, S) :-
        max_list([1,S],Cap),
        choose(1, Cap, K, S),
        vectorOf(K, GenA, AS, S).

%% | Generates a list of the given length. Length must be non-negative
vectorOf(0, _GenA, [], _Size) :- !.
vectorOf(K, GenA, [A|AS], Size) :-
        call_with_args(GenA, A, Size),
        K1 is K-1,
        vectorOf(K1, GenA, AS, Size).

%% | Generates the given value, discarding the size.
value(A, A, _Size).

%% | Generates a variable, discarding the size.
variable(X, _Size) :- var(X).

%% | Generates values with a certain structure
structure(X, Y, Size) :- var(X), !, var(Y), X=Y.
structure([], [], Size) :- !.
structure([SX|SXS], [X|XS], Size) :-
        !,
        structure(SX, X, Size),
        structure(SXS, XS, Size).
structure({ST}, {T}, Size) :-
        !,
        structure(ST, T, Size),
        structure(SXS, XS, Size).
structure( (SX, SXS), (X, XS), Size) :-
        !,
        structure(SX, X, Size),
        structure(SXS, XS, Size).
structure(GenX, X, Size) :-
        call(GenX, X, Size).

  % }}}

  % {{{ generator combination samples
%% plqc:sampleK(10, (frequency([{8, choose(11,20)}, {2,listOf( (choose(0,10)) )}])), L).
%% plqc:sampleK(10, (oneof([ choose(11,20), listOf( value(y) ), value(x)])), L).
  % }}}


% --==================================================--

qcforall(Gen, Var, Prop, Size) :- call_with_args(Gen, Var, Size), call(Prop).

% }}}

% {{{ predicate specification language

user:term_expansion( PredicateId of_type Typing,
                     (qcprop(PropName) :- PLQCProperty)
 ) :-
        pred_spec_name(PredicateId, Predicate, PropName),
        spec_expand(Predicate, Typing, PLQCProperty)
.

  % {{{ pred_spec_name(Predicate, PropName) 
pred_spec_name({Predicate,X}, Predicate, PropName) :-
        !,
        (name(X, [95|PostfixL]), ! ; name(X, PostfixL)),
        pred_spec_name_aux(Predicate, [95|PostfixL], PropName).
        
pred_spec_name(Predicate, Predicate, PropName) :-
        pred_spec_name_aux(Predicate, [], PropName).

pred_spec_name_aux(PredicateId, PostfixL, PropName) :-
        (PredicateId = Mod:Predicate, !;
         PredicateId = Predicate),
        name(Predicate, PredL),
        lists:append(PredL, PostfixL, PropNameL1),
        name(spec_, PrefixL),
        lists:append(PrefixL, PropNameL1, PropNameL),
        name(PropName, PropNameL).
  % }}}

  % {{{ expand the specification by accumulating property/generator modifiers
    % {{{ spec_expand(Predicate, TypingSpec, Property)
spec_expand(Predicate, TypingSpec, Property) :- 
        %% Call and Args are holes for unifying later
        spec_expand([(dummy-mp)], Predicate, TypingSpec, Property).

spec_expand(Modifiers, Pred, DomainRange obbeys Prop, Property) :-
        !,
        spec_expand([(Prop-prop)|Modifiers], Pred, DomainRange, Property).
spec_expand(Modifiers, Pred, DomDirRange limit Limit, Property) :-
        !,
        %% modify mp to check range
        limit_mp(Modifiers, Limit, NewMs),
        spec_expand(NewMs, Pred, DomDirRange, Property)
        .
spec_expand(Modifiers, Pred, DomainDir has_range Range, Property) :-
        !,
        %% modify mp to check range
        range_mp(Modifiers, Range, NewMs),
        spec_expand(NewMs, Pred, DomainDir, Property)
        .
spec_expand(Modifiers, Pred, Domain where Directionality, Property) :-
        !,
        dir_mp(Modifiers, Directionality, NewMs),
        spec_expand(NewMs, Pred, Domain, Property).
spec_expand(Modifiers, Pred, Typing such_that DomPrecond, Property) :-
        !, % st - such that (generator modifier for conditional generation)
        spec_expand([(DomPrecond-st)|Modifiers], Pred, Typing, Property).
spec_expand(Modifiers, Pred, Typing, Property) :-
        pp_typing(Typing, TS, Args), % pre-process typing for explicit arguments
        modify_gen(Modifiers, structure(TS), ArgGen), % modify generator if needed
        spec_prop(Modifiers, Pred, Args, InnerProperty), % build property
        Property = plqc:qcforall(ArgGen, Args, InnerProperty).
    % }}}

    % {{{ limit_mp(Modifiers, Range, NewModifiers)
limit_mp([(Call-mp)|MS], Lim, NewMs) :-
        !, NewMs = [((Lim-limit)-Call-mp)|MS].
limit_mp([M|MS], Lim, [M|NewMs]) :-
        limit_mp(MS, Lim, NewMs).
    % }}}

    % {{{ range_mp(Modifiers, Range, NewModifiers)
range_mp([(Call-mp)|MS], Range, NewMs) :-
        !, NewMs = [((Range-range)-Call-mp)|MS].
range_mp([M|MS], Range, [M|NewMs]) :-
        range_mp(MS, Range, NewMs).
    % }}}

    % {{{ dir_mp(Modifiers, Directionalities, NewModifiers)
dir_mp([(MP-mp)|MS], Dir, NewMs) :-
        !, NewMs = [((Dir-dir)-MP-mp)|MS].
dir_mp([M|MS], Dir, [M|NewMs]) :-
        dir_mp(MS, Dir, NewMs).
    % }}}

      % {{{ check_range(Input, TimeoutRes, NumberOfAnswers, Infimum, Supremum)
%% TODO - better messages
%% check_range(time_out, K, Inf, inf) :-
%%         !,
%%         print(time_out_on_infinity_Range_OK), nl.
%% check_range(time_out, K, Inf, Sup) :-
%%         !,
%%         print(time_out_on_bound_Range_NOT_OK), nl.
%% check_range(success, K, inf, Sup).
%%         !,
%%         print(terminate_on_infinity_Range_NOT_OK), nl.
%% check_range(success, K, Inf, inf).
%%         !,
%%         (K >= Inf, print(terminate_on_lower_bound_Range_OK);
%%         print(terminate_on_lower_bound_Range_NOT_OK)), nl.
%% check_range(Res, K, Inf, Sup) :-
%%         print({check_range_bad_pattern, Res}).
check_range(Input, Res, K, Inf, Sup) :-
        print({range_for, Input, Inf, {Res, K}, Sup}), nl.
      % }}}
      % {{{ defaul_timeout
%% .5 seconds default timeout
%% do not forget this will be applied in every test
defaul_timeout(500).
      % }}}

    % {{{ pp_typing(Typing, TypingAsList, NamedArgVarList)
pp_typing((Arg-T, TS), [T|XS], [Arg|YS]) :-
        !, pp_typing(TS, XS, YS).
pp_typing((T, TS), [T|XS], [Y|YS]) :-
        !, pp_typing(TS, XS, YS).
pp_typing(Arg-T, [T], [Arg]) :- !.
pp_typing(TS, [T], [Y]).
    % }}}

    % {{{ modify_gen(Modifiers, OriginalGenerator, ModifiedGenerator)
modify_gen([(DomPrecond-st) | _], Gen, suchThat(Gen, DomPrecond)) :- !.
modify_gen([_M | MS], Gen, ArgGen) :- modify_gen(MS, Gen, ArgGen).
modify_gen([], Gen, Gen).
    % }}}

    % {{{ spec_prop(Modifiers, Pred, Args, Property)
spec_prop([], _, _, true).
spec_prop([(Prop-prop)|MS], Pred, Args, (Prop, PropS)) :-
        !, spec_prop(MS, Pred, Args, PropS).
spec_prop([(MPinfo-mp)|MS], Pred, Args, (MP, PropS)) :-
        !, apply_args(Pred, Args, Call),
        build_mp(MPinfo, Call, Args, MP),
        spec_prop(MS, Pred, Args, PropS).
spec_prop([_|MS], Pred, Args, PropS) :-
        spec_prop(MS, Pred, Args, PropS).

      % {{{ apply_args(Pred, [Arg|Args], Call)
%% arguments need to be applied in order and predicate calls revert the order
apply_args(Pred, [Arg|Args], Call) :-
        apply_args(Pred, Args, Call, call(Pred, Arg)).

apply_args(Pred, [], Acc, Acc).
apply_args(Pred, [Arg|Args], Call, Acc) :-
        apply_args(Pred, Args, Call, call(Acc, Arg)).
      % }}}

      % {{{ build_mp(MPinfo, Call, Args, Prop)
%% build_mp((Dir-dir)-R, Call, Args, Prop) :-
build_mp(MP, Call, Args, Prop) :-
        build_mp(MP, Args, In, Out, Range, Limit),
        merge_mp(In, Out, Range, Limit, Call, Args, Prop).
build_mp((Dir-dir)-R, Args, WIn, WOut, Range, Limit) :-
        in_out(Dir, InD, OutsD),
        build_in(InD, Args, In),
        build_out(OutsD, Args, Out),
        wrap_mode_check(in, InD, Args, In, WIn),
        wrap_mode_check(out, OutsD, Args, Out, WOut),
        build_mp(R, Args, _in, _out, Range, Limit).
build_mp((Range-range)-MP, Args, In, Out, Range, Limit) :-
        build_mp(MP, Args, In, Out, _range, Limit).
build_mp((Limit-limit)-dummy, Args, none, none, any, Limit).
build_mp(dummy, Args, none, none, any, Limit) :-
        default_limit(Limit).

        % {{{ in_out(Dir, In, Outs)
in_out((D1, D2), In, Outs) :-
        !,
        in_out(D1, I1, O1),
        in_out(D2, I2, O2),
        merge_in(I1, I2, In),
        lists:append(O1, O2, Outs).
in_out(D, In, Out) :-
        D =.. Dl,
        (Dl = [i|_], In = Dl, Out = [];
         Dl = [o|_], In = none, Out = [Dl]   ).

%% note that this fails if more than one 'in' is given
merge_in(none, I, I).
merge_in(I, none, I).
        % }}}

        % {{{ default_limit
default_limit(100).
        % }}}

        % {{{ wrap_mode_check(Dir, DProp, Args, Prop, WrappedProp)
wrap_mode_check(in, Modes, Args, Prop, (Prop, !; print({failed_in_mode, Modes, Args}), nl)).
wrap_mode_check(out, Modes, Args, Prop, (Prop, !; print({failed_out_modes, Modes, Args}), nl)).
        % }}}

        % {{{ build_Dir(Dirs, Args, DProp)
build_in(none, _Args, true).
build_in([i|Modes], Args, In) :-
        match_modes(Modes, Args, In).

%% build_out([], _Args, true, true).
%% build_in([ [i|Modes]|DS ], Args, In, Out) :-
%%         match_modes(Modes, Args, In),
%%         build_dir(DS, _, Out).
%% build_in([ [o|Modes] ], Args, True, Out) :-
%%         match_modes(Modes, Args, Out).
%% build_in([ [o|Modes]|DS ], Args, In, Out) :-
%%         match_modes(Modes, Args, O),
%%         build_dir(DS, In, O2),
%%         Out = (O1, O2).

build_out([], _Args, true).
build_out([ [o|Modes] ], Args, Out) :-
        !, match_modes(Modes, Args, Out).
build_out([ [o|Modes]|OS ], Args, Out) :-
        match_modes(Modes, Args, O1),
        build_out(OS, Args, O2),
        Out = (O1; O2).

          % {{{ match_modes(Modes, Args, Prop)
match_modes([M], [A], P) :-
        match_mode(M, A, P).
match_modes([M|MS], [A|AS], (P1, P2)) :-
        match_mode(M, A, P1),
        match_modes(MS, AS, P2).
%% match_modes([], [], true).

match_mode(g,   A,  ground(A)).
match_mode(v,   A,  var(A)).
match_mode(gv,  A,  (ground(A); var(A))).
match_mode(ng,  A,  (not ground(A))).
match_mode(nv,  A,  nonvar(A)).
match_mode(ngv, A,  (not (ground(A); var(A)))).
          % }}}
        % }}}

        % {{{ merge_mp(In, Out, Range, Limit, Call, Args, Prop),
merge_mp(true, Out, Range, Limit, Call, Args, Prop) :-
        !, merge_mp2(Out, Range, Limit, Call, Args, Prop).
merge_mp(none, Out, Range, Limit, Call, Args, Prop) :-
        !, merge_mp2(Out, Range, Limit, Call, Args, Prop).
merge_mp(In, Out, Range, Limit, Call, Args, Prop) :-
        merge_mp2(Out, Range, Limit, Call, Args, Prop1),
        Prop = (In, Prop1).

          % {{{ merge_mp2(Out, Range, Limit, Call, Args, Prop),
%% merge_mp2(true, Range, Limit, Call, Args, Prop) :-
%%         !,
%%         merge_mp3(Range, Limit, Call, Args, Prop).
merge_mp2(none, Range, Limit, Call, Args, Prop) :-
        !, merge_mp3(Range, Limit, (Call), Args, Prop).
merge_mp2(OutProp, Range, Limit, Call, Args, Prop) :-
        %% print(Call),
        merge_mp3(Range, Limit, (Call, OutProp), Args, Prop).

            % {{{ merge_mp3(Range, Limit, Call, Args, Prop),
merge_mp3(any, Limit, Call, Args, TheCall) :-
        !,
        TheCall = bound_call(1, -2, Limit, Call, Args).
merge_mp3(default, Limit, Call, Args, TheCall) :-
        !,
        TheCall = bound_call(1, -2, Limit, Call, Args).
merge_mp3({Min,Max}, Limit, Call, Args, TheCall) :-
        bound_min(Min, Limit, LowerBound),
        %% bound_max(Max, Limit, UpperBound),
        TheCall = bound_call(LowerBound, Max, Limit, Call, Args).

              % {{{ bound_min|max
bound_min(inf, Limit, Limit) :- !.
bound_min(A, B, C) :-
        C is min(A,B).
bound_max(inf, Limit, Limit) :- !.
bound_max(A, B, C) :-
        C is min(A,B).
              % }}}

              % {{{ %% bound_call(Lower, Upper, Limit, Call, Args).
bound_call(Lower, Upper, Limit, (Call, OutProp), Args) :-
        duplicate_term(Args, OriginalArguments),
        nb_setval(counter, 0),
        nb:nb_queue(Ref),
        (
            call(Call),
            nb_getval(counter, C1),
            C2 is C1 +1,
            nb_setval(counter, C2),
            nb:nb_queue_enqueue(Ref, Args),
            %% check counter
            (
                C2 > Upper,
                !,
                print('Number of answers exceeds range upper bound for'), nl,
                print(OriginalArguments), nl
            ;
                C2 > Limit,
                !,
                print('Reached limit for number of answers tested for'), nl,
                print(OriginalArguments), nl
            ;
                call(OutProp),
                fail
            )
        ;
            nb_getval(counter, Count),
            %% number of given answers is less than range lower bound
            Lower - Count > 0, !,
            print('Did not reach range lower bound for'), nl,
            print(OriginalArguments), nl
        ;
            %% we are OK here (exceeding upper bound and limit is on the fly)
            nb:nb_queue_close(Ref, Answers, []),
            nb_getval(counter, Count),
            Result = {Count, Answers}
        ).
              % }}}
            % }}}
          % }}}
        % }}}
      % }}}

    % }}}
  % }}}

% }}}

max_tries_factoring(N,X) :-
        X is 5 *N.


% -*- mode: prolog; mode: folding -*-


:- module(plqc).
%% :- module(plqc,[quickcheck/1, quickcheck/2, zx/2]).

:- reconsult(opts).
:- reconsult(result).
:- reconsult(ctx).
:- reconsult(state).

:- use_module(library(random)).
:- use_module(library(lists)).


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

run(plqc:qcforall(Gen, Var, Test), Opts, Ctx, IState, OState, Result) :- 
        !,
        state:get_size(IState,Size),
        call_with_args(Gen, Var, Size),
        ctx:bind(Ctx, Var, Ctx1),
        run(Test, Opts, Ctx1, IState, OState, Result).
%% a leaf in the property syntax tree - a predicate call
run(Test, Opts, Ctx, State, State, Result) :- 
        (call(Test), !,
        create_pass_result(Ctx, true_prop, Result)
        )
    ;
        (!,
        create_fail_result(Ctx, false_prop, Result)
        ).
%% TODO: prolog conjunction and disjunction
%% TODO: collect information for user analysis 

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
        suchThatSized(GenA, PredA, A, S).
suchThatSized(GenA, PredA, A, S) :-
        (
            suchThatMaybeSized(GenA, PredA, A, S), !
        ;
            stSizeStep(S, S1),
            suchThatSized(GenA, PredA, A, S1)            
        ).

%% | Tries to generate a value that satisfies a predicate.
suchThatMaybe(GenA, PredA, A) :-
        stDefaultSize(S),
        suchThatMaybeSized(GenA, PredA, A, S).
suchThatMaybeSized(GenA, PredA, A, S) :-
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
  % }}}

  % {{{ generator combination samples
%% plqc:sampleK(10, (frequency([{8, choose(11,20)}, {2,listOf( (choose(0,10)) )}])), L).
%% plqc:sampleK(10, (oneof([ choose(11,20), listOf( value(y) ), value(x)])), L).
  % }}}


% --==================================================--

qcforall(Gen, Var, Prop, Size) :- call_with_args(Gen, Var, Size), call(Prop).

% }}}


max_tries_factoring(N,X) :-
        X is 5 *N.


% -*- mode: prolog; mode: folding -*-

:- module(result).

% {{{ making pass records

%% {pass,  reason    :: pass_reason(),
%%         samples   :: [sample()],
%%         printers  :: [stats_printer()],
%%         performed :: pos_integer()}.
default_pass({pass, Reason, Samples, Printers, Performed}).

mk_pass(Params, Res) :-
        default_pass(Acc),
        mk_pass(Params, Acc, Res).

mk_pass([], Res, Res).
mk_pass([{reason, R} | Ps], Acc1, Res) :-
        new_reason_pass(Acc1, R, Acc2),
        mk_pass(Ps, Acc2, Res).
mk_pass([{samples, S} | Ps], Acc1, Res) :-
        new_samples_pass(Acc1, S, Acc2),
        mk_pass(Ps, Acc2, Res).
mk_pass([{printers, P} | Ps], Acc1, Res) :-
        new_printers_pass(Acc1, P, Acc2),
        mk_pass(Ps, Acc2, Res).
mk_pass([{performed, P} | Ps], Acc1, Res) :-
        new_performed_pass(Acc1, P, Acc2),
        mk_pass(Ps, Acc2, Res).

% }}}

% {{{ making fail records

%% {fail,  reason    :: fail_reason(),
%%         bound     :: imm_testcase() | counterexample(),
%%         actions   :: fail_actions(),
%%         performed :: pos_integer()}).
default_fail({fail, Reason, Samples, Printers, Performed}).

mk_fail(Params, Fail) :-
        default_fail(Acc),
        mk_fail(Params, Acc, Fail).

mk_fail([], Fail, Fail).
mk_fail([{reason, R} | Ps], Acc1, Fail) :-
        new_reason_fail(Acc1, R, Acc2),
        mk_fail(Ps, Acc2, Fail).
mk_fail([{bound, B} | Ps], Acc1, Fail) :-
        new_bound_fail(Acc1, B, Acc2),
        mk_fail(Ps, Acc2, Fail).
mk_fail([{actions, A} | Ps], Acc1, Fail) :-
        new_actions_fail(Acc1, A, Acc2),
        mk_fail(Ps, Acc2, Fail).
mk_fail([{performed, P} | Ps], Acc1, Fail) :-
        new_performed_fail(Acc1, P, Acc2),
        mk_fail(Ps, Acc2, Fail).

% }}}

% {{{ changing a pass record

new_reason_pass({pass, Reason, Samples, Printers, Performed},
                New,
                {pass, New, Samples, Printers, Performed}).
new_samples_pass({pass, Reason, Samples, Printers, Performed},
                 New,
                 {pass, Reason, New, Printers, Performed}).
new_printers_pass({pass, Reason, Samples, Printers, Performed},
                  New,
                  {pass, Reason, Samples, New, Performed}).
new_performed_pass({pass, Reason, Samples, Printers, Performed},
                   New,
                   {pass, Reason, Samples, Printers, New}).

% }}}

% {{{ changing a fail record

new_reason_fail({fail, Reason, Bound, Actions, Performed},
                New,
                {fail, New, Bound, Actions, Performed}).
new_bound_fail({fail, Reason, Bound, Actions, Performed},
                New,
                {fail, Reason, New, Actions, Performed}).
new_actions_fail({fail, Reason, Bound, Actions, Performed},
                  New,
                  {fail, Reason, Bound, New, Performed}).
new_performed_fail({fail, Reason, Bound, Actions, Performed},
                   New,
                   {fail, Reason, Bound, Actions, New}).

% }}}

% {{{ accessing a pass record

reason_pass({pass, Reason, _Samples, _Printers, _Performed},Reason).
samples_pass({pass, _Reason, Samples, _Printers, _Performed},Samples).
printers_pass({pass, _Reason, _Samples, Printers, _Performed},Printers).
performed_pass({pass, _Reason, _Samples, _Printers, Performed},Performed).

% }}}

% {{{ accessing a fail record

reason_fail({fail, Reason, _Bound, _Actions, _Performed},Reason).
bound_fail({fail, _Reason, Bound, _Actions, _Performed},Bound).
actions_fail({fail, _Reason, _Bound, Actions, _Performed},Actions).
performed_fail({fail, _Reason, _Bound, _Actions, Performed},Performed).

% }}}

is_pass_res({pass, _Reason, _Samples, _Printers, _Performed}).
is_fail_res({fail, _Reason, _Bound,   _Actions,  _Performed}).



%% -type pass_reason() :: 'true_prop' | 'didnt_crash'.
%% -type fail_reason() :: 'false_prop' | 'time_out' | {'trapped',exc_reason()}
%% 		     | exception() | {'sub_props',[{tag(),fail_reason()},...]}.
%% -type error_reason() :: 'arity_limit' | 'cant_generate' | 'cant_satisfy'
%% 		      | 'non_boolean_result' | 'rejected' | 'too_many_instances'
%% 		      | 'type_mismatch' | 'wrong_type' | {'typeserver',term()}
%% 		      | {'unexpected',any()} | {'unrecognized_option',term()}.
%% -type error() :: {'error', error_reason()}.

%% -type run_result() :: #pass{performed :: 'undefined'}
%% 		    | #fail{performed :: 'undefined'}
%% 		    | error().
%% -type imm_result() :: #pass{reason :: 'undefined'} | #fail{} | error().
%% -type long_result() :: 'true' | counterexample() | error().
%% -type short_result() :: boolean() | error().
%% -type result() :: long_result() | short_result().
%% -type shrinking_result() :: {non_neg_integer(),imm_testcase()}.


%% -spec get_result(imm_result(),test(),opts()) -> {short_result(),long_result()}.
%% get_result(#pass{}, _Test, _Opts) ->
%%     {true, true};
%% get_result(#fail{reason = Reason, bound = Bound}, Test, Opts) ->
%%     case shrink(Bound, Test, Reason, Opts) of
%% 	{ok,MinImmTestCase} ->
%% 	    MinTestCase = clean_testcase(MinImmTestCase),
%% 	    save_counterexample(MinTestCase),
%% 	    {false, MinTestCase};
%% 	{error,ErrorReason} = Error ->
%% 	    report_error(ErrorReason, Opts#opts.output_fun),
%% 	    {Error, Error}
%%     end;
%% get_result({error,_Reason} = ErrorResult, _Test, _Opts) ->
%%     {ErrorResult, ErrorResult}.

%% -spec get_rerun_result(run_result()) -> short_result().
%% get_rerun_result(#pass{}) ->
%%     true;
%% get_rerun_result(#fail{}) ->
%%     false;
%% get_rerun_result({error,_Reason} = ErrorResult) ->
%%     ErrorResult.



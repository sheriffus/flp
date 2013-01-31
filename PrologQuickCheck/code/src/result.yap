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


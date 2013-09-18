% -*- mode: prolog; mode: folding -*-

:- module(opts).

% {{{ defaults

%% fun io   :: output_fun()
%% false    :: boolean(),
%% 100      :: pos_integer(),
%% 1        :: size(),
%%          :: seed(),  % TODO
%% 42       :: size(),
%% 500      :: non_neg_integer(),
%% false    :: boolean(),
%% 50       :: pos_integer(),
%% false    :: boolean(),
%%          :: {'type', plqc_types:any_type()},
%% infinity :: timeout()}).

%% default options
default({opts, format, false, 100, 1, some_seed, 42, 500, false, 50, false, {type, plqc:int}, infinity}) :-
        %% plqc_types:any_type(T).
        T=fix_default_any__opts.

% }}}

% {{{ accessing options

output_fun(       {opts, OutputFun, _,_,_,_,_,_,_,_,_,_,_},        OutputFun).
long_result(      {opts, _, LongResult, _,_,_,_,_,_,_,_,_,_},      LongResult).
numtests(         {opts, _,_, NumTests, _,_,_,_,_,_,_,_,_},        NumTests).
start_size(       {opts, _,_,_, StartSize, _,_,_,_,_,_,_,_},       StartSize).
seed(             {opts, _,_,_,_, Seed, _, _,_,_,_,_,_},           Seed).
max_size(         {opts, _,_,_,_,_, MaxSize, _,_,_,_,_,_},         MaxSize).
max_shrinks(      {opts, _,_,_,_,_,_, MaxShrinks, _,_,_,_,_},      MaxShrinks).
noshrink(         {opts, _,_,_,_,_,_,_, NoShrink, _,_,_,_},        NoShrink).
constraint_tries( {opts, _,_,_,_,_,_,_,_, ConstraintTries, _,_,_}, ConstraintTries).
expect_fail(      {opts, _,_,_,_,_,_,_,_,_, ExpectFail, _,_},      ExpectFail).
any_type(         {opts, _,_,_,_,_,_,_,_,_,_, AnyType, _},         AnyType).
spec_timeout(     {opts, _,_,_,_,_,_,_,_,_,_,_, SpecTimeout},      SpecTimeout).

% }}}

% {{{ changing an option

new_output_fun(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        New, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout}
 ).
new_long_result(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, New, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout}
 ).
new_numtests(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, New, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout}
 ).
new_start_size(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, New, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout}
 ).
new_seed(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, StartSize, New, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout}
 ).
new_max_size(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, New, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout}
 ).
new_max_shrinks(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, New,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout}
 ).
new_noshrink(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        New, ConstraintTries, ExpectFail, AnyType, SpecTimeout}
 ).
new_constraint_tries(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, New, ExpectFail, AnyType, SpecTimeout}
 ).
new_expect_fail(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, New, AnyType, SpecTimeout}
 ).
new_any_type(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, New, SpecTimeout}
 ).
new_spec_timeout(
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, SpecTimeout},
        New,
        {opts,
        OutputFun, LongResult, NumTests, StartSize, Seed, MaxSize, MaxShrinks,
        NoShrink, ConstraintTries, ExpectFail, AnyType, New}
 ).

% }}}

% {{{ parsing options

parse(UserOpts, Opts) :- 
        default(DOs), parse_opts(UserOpts, DOs, Opts).


parse_opts([], Opts, Opts).
parse_opts([UserOpt | Rest], OptsAcc, Opts) :-
        parse_opt(UserOpt, OptsAcc, OptsAcc1),
        parse_opts(Rest, OptsAcc1, Opts).

quietfun(_,_).
verbosefun(M,XS) :- format(M,XS).
tofilefun(IoDev, M, XS) :- nl,print('TODO#tofilefun'),nl,nl.
tostreamfun(Stream, M, XS) :- format(Stream, M, XS).

parse_opt(quiet, OptsA, Opts) :-
        new_output_fun(OptsA, opts:quietfun, Opts).
parse_opt(verbose, OptsA, Opts) :-
        new_output_fun(OptsA, opts:verbosefun, Opts).
parse_opt({to_file,IoDev}, OptsA, Opts) :-
        new_output_fun(OptsA, opts:tofilefun(IoDev), Opts).    % TODO
        %% io:format(IoDev, S, F) 
parse_opt({to_strem,Strem}, OptsA, Opts) :-
        new_output_fun(OptsA, opts:tostreamfun(Stream), Opts).
parse_opt({on_output,Print}, OptsA, Opts) :-
        new_output_fun(OptsA, Print, Opts).
parse_opt(long_result, OptsA, Opts) :-
        new_long_result(OptsA, true, Opts).
parse_opt({numtests,N}, OptsA, Opts) :-
        new_numtests(OptsA, N, Opts).
parse_opt({start_size,Size}, OptsA, Opts) :-
        new_start_size(OptsA, Size, Opts).
parse_opt({max_size,Size}, OptsA, Opts) :-
        new_max_size(OptsA, Size, Opts).
parse_opt({max_shrinks,N}, OptsA, Opts) :-
        new_max_shrinks(OptsA, N, Opts).
parse_opt(noshrink, OptsA, Opts) :-
        new_noshrink(OptsA, true, Opts).
parse_opt({constraint_tries,N}, OptsA, Opts) :-
        new_constraint_tries(OptsA, N, Opts).
parse_opt(fails, OptsA, Opts) :-
        new_expect_fail(OptsA, true, Opts).
parse_opt(any_to_integer, OptsA, Opts) :-
        proper_types:integer(I),
        new_any_type(OptsA, {type,I}, Opts).
parse_opt({spec_timeout,N}, OptsA, Opts) :-
        new_spec_timeout(OptsA, N, Opts).

% }}}


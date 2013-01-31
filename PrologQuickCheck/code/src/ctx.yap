% -*- mode: prolog; mode: folding -*-

:- module(ctx).

%% {ctx, mode         :: 'new' | 'try_shrunk' | 'try_cexm',
%%       bound        :: imm_testcase() | counterexample(),
%%       actions      :: fail_actions(),
%%       samples      :: [sample()],
%%       printers     :: [stats_printer()]}

default({ctx, new, [], [], [], []}).

% {{{ accessing a context

mode({ctx, Mode, _Bound, _Actions, _Samples, _Printers}, Mode).
bound({ctx, _Mode, Bound, _Actions, _Samples, _Printers}, Bound).
actions({ctx, _Mode, _Bound, Actions, _Samples, _Printers}, Actions).
samples({ctx, _Mode, _Bound, _Actions, Samples, _Printers}, Samples).
printers({ctx, _Mode, _Bound, _Actions, _Samples, Printers}, Printers).

% }}}

% {{{ changing a context

new_mode({ctx, Mode, Bound, Actions, Samples, Printers},
         New,
         {ctx, New, Bound, Actions, Samples, Printers}).
new_bound({ctx, Mode, Bound, Actions, Samples, Printers},
          New,
          {ctx, Mode, New, Actions, Samples, Printers}).
new_actions({ctx, Mode, Bound, Actions, Samples, Printers},
            New,
            {ctx, Mode, Bound, New, Samples, Printers}).
new_samples({ctx, Mode, Bound, Actions, Samples, Printers},
            New,
            {ctx, Mode, Bound, Actions, New, Printers}).
new_printers({ctx, Mode, Bound, Actions, Samples, Printers},
             New,
             {ctx, Mode, Bound, Actions, Samples, New}).


bind({ctx, Mode, Bound, Actions, Samples, Printers},
     New,
     {ctx, Mode, [New|Bound], Actions, Samples, Printers}).


% }}}

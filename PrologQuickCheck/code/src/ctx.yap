% -*- mode: prolog; mode: folding -*-

:- module(ctx).

%% {ctx, mode         :: 'new' | 'try_shrunk' | 'try_cexm',
%%       bound        :: imm_testcase() | counterexample(),
%%       module       :: module_name(),
%%       actions      :: fail_actions(),                    % not used
%%       samples      :: [sample()],                        % not used
%%       printers     :: [stats_printer()]}                 % not used

default({ctx, new, plqc, [], [], [], []}).

% {{{ accessing a context

mode({ctx, Mode, _Bound, _Module, _Actions, _Samples, _Printers}, Mode).
bound({ctx, _Mode, Bound, _Module, _Actions, _Samples, _Printers}, Bound).
module({ctx, _Mode, _Bound, Module, _Actions, _Samples, _Printers}, Module).
actions({ctx, _Mode, _Bound, _Module, Actions, _Samples, _Printers}, Actions).
samples({ctx, _Mode, _Bound, _Module, _Actions, Samples, _Printers}, Samples).
printers({ctx, _Mode, _Bound, _Module, _Actions, _Samples, Printers}, Printers).

% }}}

% {{{ changing a context

new_mode({ctx, Mode, Bound, Module, Actions, Samples, Printers},
         New,
         {ctx, New, Bound, Module, Actions, Samples, Printers}).
new_bound({ctx, Mode, Bound, Module, Actions, Samples, Printers},
          New,
          {ctx, Mode, New, Module, Actions, Samples, Printers}).
new_module({ctx, Mode, Bound, Module, Actions, Samples, Printers},
            New,
            {ctx, Mode, Bound, New, Actions, Samples, Printers}).
new_actions({ctx, Mode, Bound, Module, Actions, Samples, Printers},
            New,
            {ctx, Mode, Bound, Module, New, Samples, Printers}).
new_samples({ctx, Mode, Bound, Module, Actions, Samples, Printers},
            New,
            {ctx, Mode, Bound, Module, Actions, New, Printers}).
new_printers({ctx, Mode, Bound, Module, Actions, Samples, Printers},
             New,
             {ctx, Mode, Bound, Module, Actions, Samples, New}).


bind({ctx, Mode, Bound, Module, Actions, Samples, Printers},
     New,
     {ctx, Mode, [New|Bound], Module, Actions, Samples, Printers}).


% }}}

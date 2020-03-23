:- use_module(library(clpfd)).

% Prolog playground

:- op(400, yfx, <<<).

<<<(A, B, R) :- R #= A << B, A #= R >> B.
%
%
%
% bigconst(X) :- X #=< 0x3FF, X #>= 0.
%
% asm(instruction('ADD', [A]), R) :-
%   bigconst(A),
%   R #= 0b0000 + (A << 4).
%
% asm(instruction('SUB', [A]), R) :-
%   bigconst(A),
%   R #= 0b0001 + (A << 4).

replace(Subterm0, Subterm, Term0, Term) :-
        (   Term0 == Subterm0 -> Term = Subterm
        ;   var(Term0) -> Term = Term0
        ;   Term0 =.. [F|Args0],
            maplist(replace(Subterm0,Subterm), Args0, Args),
            Term =.. [F|Args]
        ).

access_reg('A', Value, IState, State) :-
  IState = machine_state(IP, A, B, C, D, Stack, IOContext, Memory, Crashed, Halted),
  State  = machine_state(IP, Value, B, C, D, Stack, IOContext, Memory, Crashed, Halted).

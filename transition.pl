:- module(transition, [emulate_transition_list/1, even_transition_list/1, register_state_transition_list/5]).

:- use_module(library(clpfd)).
:- use_module(interpreter).

% The final condition and transition_list function for just a regular emulator
% Accepts states when the machine naturally halts (by a HLT instruction) or when it crashes
emulate_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, _IOContext, _Memory, _Crashed, 1)).
emulate_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, _IOContext, _Memory, 1, _Halted)).

emulate_transition_list([Last])            :- emulate_final_condition(Last).
emulate_transition_list([Current, Next|T]) :- trans(Current, Next), emulate_transition_list([Next|T]).

% transition_list for even output. Accepts states when the final state contains a single character as output and is halted
even_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, virtual([], [V]), _Memory, _Crashed, 1)) :-
  mod(V, 2) #= 0.

even_transition_list([Last])            :- even_final_condition(Last).
even_transition_list([Current, Next|T]) :- trans(Current, Next), even_transition_list([Next|T]).

% TODO check if we can somehow implicitly pass arguments around to avoid having to repeat the A, B, C, D arguments

register_state_final_condition(machine_state(_IP, A, B, C, D, _Stack, _IO, _Memory, _Crashed, 1), A_c, B_c, C_c, D_c) :-
  A #= A_c,
  B #= B_c,
  C #= C_c,
  D #= D_c.

  register_state_transition_list([Last], A, B, C, D)            :- register_state_final_condition(Last, A, B, C, D).
  register_state_transition_list([Current, Next|T], A, B, C, D) :- trans(Current, Next), register_state_transition_list([Next|T], A, B, C, D).
:- module(transition, [emulate_transition_list/1, even_transition_list/1, register_state_transition_list/5, reach_ip_transition_list/2, stack_state_transition_list/2, crashed_transition_list/1]).

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

% transition_list for a certain register configuration that is passed as arguments. Accepts when machine is halted and
%  register config is correct
register_state_final_condition(machine_state(_IP, A, B, C, D, _Stack, IO, _Memory, _Crashed, 1), A_c, B_c, C_c, D_c) :-
  A #= A_c,
  B #= B_c,
  C #= C_c,
  D #= D_c,
  IO = virtual([], _).

register_state_transition_list([Last], A, B, C, D)            :- register_state_final_condition(Last, A, B, C, D).
register_state_transition_list([Current, Next|T], A, B, C, D) :- trans(Current, Next), register_state_transition_list([Next|T], A, B, C, D).

% transition_list for when the machine reaches an instruction. Accepts when machine reaches the IP.

reach_ip_final_condition(machine_state(IP, _A, _B, _C, _D, _Stack, IO, _Memory, _Crashed, _Halted), IP_c) :-
  IP #= IP_c,
  IO = virtual([], _).

reach_ip_transition_list([Last], IP)            :- reach_ip_final_condition(Last, IP).
reach_ip_transition_list([Current, Next|T], IP) :- trans(Current, Next), reach_ip_transition_list([Next|T], IP).

% transition__list for a certain stack configuration that is passed as an argument. Accepts when machine is halted
%  and stack config is correct

stack_state_final_condition(machine_state(_IP, _A, _B, _C, _D, Stack, IO, _Memory, _Crashed, 1), DesiredStack) :-
  DesiredStack = Stack, IO = virtual([], _).

stack_state_transition_list([Last], DesiredStack)            :- stack_state_final_condition(Last, DesiredStack).
stack_state_transition_list([Current, Next|T], DesiredStack) :- trans(Current, Next), stack_state_transition_list([Next|T], DesiredStack).


% transition__list for crashing the machine. Accepts if the machine is crashed.

crashed_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, IO, _Memory, 1, _Halted)) :-
  IO = virtual([], _).

crashed_transition_list([Last])            :- crashed_final_condition(Last).
crashed_transition_list([Current, Next|T]) :- trans(Current, Next), crashed_transition_list([Next|T]).

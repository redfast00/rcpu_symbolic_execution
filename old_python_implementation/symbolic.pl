:- use_module(library(clpfd)).


init_array(_Value).
store_array(_Array, _Idx, _Value).

% Axioms about arrays

% The array is initialized with an init value
select_array(A, _, Value) :- A = init_array(Value).
% Accessing a element that was just written will result in that value
select_array(A, Idx, Value) :- A = store_array(_, Idx, Value).
% Overwriting an index will result in the value of that index changing to the new value
select_array(A, Idx, Value) :- A = store_array(B, OtherIdx, _), OtherIdx #\= Idx, select_array(B, Idx, Value).



machine_state(_IP, _A, _B, _C, _D, _Stack, _Inputlist, _Outputlist, _Memory, _Crashed, _Halted).


% transition_condition(machine_state(1, A, B, C, D, Stack, Inputstack, Outputstack, Memory, 0, 0),machine_state(2, A, B, C, A, Stack, Inputstack, Outputstack, Memory, 0, 0)).
%
% %
% %
% %

% %
% % solution(X) :- oscar([0|X]).
:- consult(machinedb).

% machine_state(_IP, _A, _B, _C, _D, _Stack, _Inputlist, _Outputlist, _Memory, _Crashed, _Halted)
final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, _Inputlist, [3], _Memory, 0, _Halted)).

oscar([Last]) :- final_condition(Last).
oscar([Current, Next|T]) :- transition_condition(Current, Next), oscar([Next|T]).


solution(Inputstack, Rest) :- oscar([machine_state(0, 0, 0, 0, 0, [], Inputstack, [], init_array(0), 0, 0)|Rest]).

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



machine_state(_IP, _A, _B, _C, _D, _Stack, _Inputstack, _Outputstack, _Memory).


% transition_condition(Initial, Next) :- Next #= Initial + 1.

final_condition(State) :- State #= 3.


oscar([Last]) :- final_condition(Last).
oscar([Current, Next|T]) :- transition_condition(Current, Next), oscar([Next|T]).

solution(X) :- oscar([0|X]).




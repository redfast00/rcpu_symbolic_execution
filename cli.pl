#!/usr/bin/env swipl
:- use_module(assembler).
:- use_module(interpreter).
:- use_module(transition).
:- use_module(library(clpfd)).

:- discontiguous main/1.
:- initialization(main, main).

% See test.sh for usage examples

main([assemble, InFilename, OutFilename]) :-
  write("Starting to assemble\n"),
  assemble_to_ast(Ast, InFilename),
  assemble_to_file(Ast, OutFilename),
  write("Done\n").

main([run_text, InFilename]) :-
  assemble_to_ast(AsmList, InFilename),
  run_real_mode(AsmList, Execution),
  write(Execution).

main([run_binary, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  run_real_mode(AsmList, Execution),
  write(Execution).


run_real_mode(AsmList, Execution) :-
  add_instructions(AsmList),
  set_stream(user_input, type(binary)),
  set_stream(user_output, type(binary)),
  set_stream(user_input, buffer(false)),
  emulate_transition_list([machine_state(0, 0, 0, 0, 0, [], real(user_input, user_output), init_array(0), 0, 0)|Execution]).

report(Result) :-
  write("END OF PROGRAM,\ninput = "),
  % Now there are still multiple possibilities, so label and get one of them
  label(Result),
  write(Result),nl,nl.

main([even, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  add_instructions(AsmList),
  write("Searching for input that results in output [V] where V is even\n"),
  even_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_]),
  report(Asked).

main([register_config, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  add_instructions(AsmList),
  write("Searching for input that results in a certain register configuration\n"),
  % TODO if there is time left, find a way to get the user to input these conditions instead of hardcoding them
  %  this will take some time, since we'd have to parse user input
  A #= 2*B,
  A #> 1,
  C #= 5,
  D #= A + 1,
  register_state_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_], A, B, C, D),
  report(Asked).

main([reach_ip, InFilename, ToReachStr]) :-
  atom_number(ToReachStr, ToReach),
  ast_from_file(InFilename, AsmList),
  add_instructions(AsmList),
  write("Trying to reach certain IP\n"),
  reach_ip_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_], ToReach),
  report(Asked).

main([stack_config, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  add_instructions(AsmList),
  write("Searching for input that results in a certain stack configuration\n"),
  [First, Second, Third | _] = DesiredStack,
  First #= Second,
  Third #> 1,
  stack_state_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_], DesiredStack),
  % TODO if there is time left, find a way to get the user to input these conditions instead of hardcoding them
  %  this will take some time, since we'd have to parse user input
  report(Asked).

main([crash, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  add_instructions(AsmList),
  write("Searching for input that crashes the machine\n"),
  crashed_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_]),
  report(Asked).

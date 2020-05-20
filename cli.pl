#!/usr/bin/env swipl

:- use_module(assembler).
:- use_module(interpreter).
:- use_module(transition).
:- use_module(library(clpfd)).

% > ./cli.pl assemble testcases/basic.asm test.out
% This assembles the source code into a binary file the interpreter can read


% > sha1sum testcases/basic.out test.out
% testcases/basic.out is the output of the original Python assembler
% This assembler gives the same output
:- discontiguous main/1.
:- initialization(main, main).

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


all_bytes([C]) :-
  0 #=< C,
  C #=< 0xFF.
all_bytes([C|R]) :-
  0 #=< C,
  C #=< 0xFF,
  all_bytes(R).

main([even, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  add_instructions(AsmList),
  write("Searching for input that results in output [V] where V is even\n"),
  all_bytes(Asked),
  even_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_]),
  write("\nEND OF PROGRAM,\ninput = "),
  % Now there are still multiple possibilities, so label and get one of them
  label(Asked),
  write(Asked),nl.

main([register_config, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  add_instructions(AsmList),
  write("Searching for input that results in a certain register configuration\n"),
  all_bytes(Asked),
  % TODO if there is time left, find a way to get the user to input these conditions instead of hardcoding them
  %  this will take some time, since we'd have to parse user input
  A #= 2*B,
  A #> 1,
  C #= 5,
  D #= A + 1,
  register_state_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_], A, B, C, D),
  write(Asked),
  write("\nEND OF PROGRAM,\ninput = "),
  % Now there are still multiple possibilities, so label and get one of them
  label(Asked),
  write(Asked),nl.

main([reach_ip, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  write(AsmList),
  add_instructions(AsmList),
  write("Trying to reach certain IP\n"),
  all_bytes(Asked),
  % TODO take this from cli
  ToReach #= 10,
  Asked = [5],
  reach_ip_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_], ToReach),
  write(Asked),
  write("\nEND OF PROGRAM,\ninput = "),
  % Now there are still multiple possibilities, so label and get one of them
  label(Asked),
  write(Asked),nl.

main([stack_config, InFilename]) :-
  ast_from_file(InFilename, AsmList),
  add_instructions(AsmList),
  write("Searching for input that results in a certain stack configuration\n"),
  all_bytes(Asked),
  [First, Second, Third | _] = DesiredStack,
  First #= Second,
  Third #> 1,
  stack_state_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_], DesiredStack),
  write(Asked),
  % TODO if there is time left, find a way to get the user to input these conditions instead of hardcoding them
  %  this will take some time, since we'd have to parse user input

  write("\nEND OF PROGRAM,\ninput = "),
  % Now there are still multiple possibilities, so label and get one of them
  label(Asked),
  write(Asked),nl.

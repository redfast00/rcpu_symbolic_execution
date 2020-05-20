#!/usr/bin/env swipl

:- use_module(assembler).
:- use_module(interpreter).
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


% The final condition and transitionlist function for just a regular emulator
% It halts when it naturally halts (by a HLT instruction) or when it crashes
emulate_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, _IOContext, _Memory, _, 1)).
emulate_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, _IOContext, _Memory, 1, _)).

emulate_transition_list([Last])            :- emulate_final_condition(Last).
emulate_transition_list([Current, Next|T]) :- trans(Current, Next), emulate_transition_list([Next|T]).

three_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, virtual([], [V]), _Memory, _, 1)) :-
  V #= 3.

three_transition_list([Last])            :- three_final_condition(Last).
three_transition_list([Current, Next|T]) :- trans(Current, Next), three_transition_list([Next|T]).

all_bytes([]).
all_bytes([C|R]) :-
  0 #=< C,
  C #=< 0xFF,
  all_bytes(R).

main([three, InFilename]) :-
  assemble_to_ast(AsmList, InFilename),
  add_instructions(AsmList),
  write("Searching for output that results in [3]\n"),
  all_bytes(Asked),
  three_transition_list([machine_state(0, 0, 0, 0, 0, [], virtual(Asked, []), init_array(0), 0, 0)|_]),
  write("\nEND OF PROGRAM,\ninput = "),
  % Now there are still multiple possibilities, so label all of them
  label(Asked),
  write(Asked),nl.

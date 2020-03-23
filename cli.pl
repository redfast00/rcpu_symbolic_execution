#!/usr/bin/env swipl

:- use_module(assembler).
:- use_module(interpreter).

% > ./cli.pl assemble testcases/basic.asm test.out
% This assembles the source code into a binary file the interpreter can read


% > sha1sum testcases/basic.out test.out
% testcases/basic.out is the output of the original Python assembler
% This assembler gives the same output

:- initialization(main, main).

main([assemble, InFilename, OutFilename]) :-
  write("Starting to assemble\n"),
  assemble_to_ast(Ast, InFilename),
  assemble_to_file(Ast, OutFilename),
  write("Done\n").



main([emulate, InFilename]) :-
  assemble_to_ast(AsmList, InFilename),
  add_instructions(AsmList),
  set_stream(user_input, type(binary)),
  set_stream(user_output, type(binary)),
  set_stream(user_input, buffer(false)),
  emulate_transition_list([machine_state(0, 0, 0, 0, 0, [], real(user_input, user_output), init_array(0), 0, 0)|Rest]),
  write("\nEND OF PROGRAM\n"),
  write(Rest).


emulate_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, _IOContext, _Memory, _, 1)).
emulate_final_condition(machine_state(_IP, _A, _B, _C, _D, _Stack, _IOContext, _Memory, 1, _)).

emulate_transition_list([Last])            :- emulate_final_condition(Last).
emulate_transition_list([Current, Next|T]) :- trans(Current, Next), emulate_transition_list([Next|T]).

%% Parser for RCPU assembly source files

:- module(parser, [parse_program//2]).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).


parse_program(Entrypoint, Asmlist) -->
    skiplines,
    ".text",
    skiplines,
    ".global ", label_parse(Entrypoint),
    proglines(Asmlist).

% Skip lines with just comments on
skiplines --> [].
skiplines --> line_end, skiplines.


% Last line of the file
proglines([]) --> [].
% Empty line or line with just a comment
proglines(Ls) --> line_end, proglines(Ls).
% Line with a label
proglines([A|Ls]) --> label_parse(A), proglines(Ls).
% Line with an instruction
proglines([A|Ls]) --> asm_instruction_parse(A), proglines(Ls).



line_end --> whites, optional(commentline, []), ("\n" | call(eos)).

commentline --> ";", string_without("\n", _).
label_parse(label(Labelname)) --> string_without("\n: ", Codes), ":", line_end, { atom_codes(Labelname, Codes) }.

argument(label(Labelname)) --> string_without("\n,:; ", Codes), ":", { atom_codes(Labelname, Codes) }.
argument(register('A')) --> "A".
argument(register('B')) --> "B".
argument(register('C')) --> "C".
argument(register('D')) --> "D".
argument(constant(A)) --> integer(A).

argument_list([]) --> [].
argument_list([Arg]) --> argument(Arg).
argument_list([Arg|Rest]) --> argument(Arg), ",", whites, argument_list(Rest).

asm_instruction_parse(ins(Instruction, Arguments)) --> nonblanks(ICodes), whites, argument_list(Arguments), line_end, {atom_codes(Instruction, ICodes)}.
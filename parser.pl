:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- debug.

% TODO find out what we want to do about order of In/Out in predicates

assemble(AsmList, Filename) :-
  phrase_from_file(program(Entrypoint, AsmList0), Filename), % parse assembly code
  AsmList1 = [ins('JMP', [Entrypoint])|AsmList0], % insert jump instruction to entrypoint
  label_mapping(AsmList1, Labelmapping), % make a mapping of labels to addresses
  filter_instruction(AsmList2, AsmList1), % only keep instructions in list
  maplist(make_label_constant(Labelmapping), AsmList, AsmList2)
  % convert_to_binary(AsmList, AsmList2)
  .

make_label_constant(Labelmapping, ins(Opcode, ConstantArglist), ins(Opcode, SymbolicArglist)) :-
  maplist(make_label_constant(Labelmapping), SymbolicArglist, ConstantArglist).

% TODO is dit lelijk? Ben ik lelijk?
make_label_constant(Labelmapping, label(X), constant(Replaced)) :- Replaced = Labelmapping.X.
make_label_constant(_, X, X) :- X \= label(_).

% filter_instruction(filtered, unfiltered)
filter_instruction([], []).
filter_instruction([A|FilteredRest], [A|Rest]) :- A = ins(_, _), filter_instruction(FilteredRest, Rest).
filter_instruction(FilteredRest, [A|Rest])     :- A = label(_),  filter_instruction(FilteredRest, Rest).

label_mapping(AsmList, Labelmapping) :- label_mapping(AsmList, Labelmapping, 0).

label_mapping([], M, _) :- M = mapping{}.
% TODO avoid left recursion if at all possible
label_mapping([label(X)|Rest], M, N) :- label_mapping(Rest, M0, N), put_dict(X, M0, N, M).
label_mapping([ins(_, _)|Rest], M, N0) :-  N is N0 + 1, label_mapping(Rest, M, N).

register_mapping('A', 0b00).
register_mapping('B', 0b01).
register_mapping('C', 0b10).
register_mapping('D', 0b11).


program(Entrypoint, Asmlist) -->
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
label_parse(label(Labelname)) --> string_without("\n:", Codes), ":", line_end, { atom_codes(Labelname, Codes) }.

instruction --> "ADD" | "SYS"| "SUB" | "MUL" | "DIV" | "CAL" | "RET" | "PSH".

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
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).
:- debug.

% TODO find out what we want to do about order of In/Out in predicates

assemble(AsmList, Filename) :-
  phrase_from_file(program(Entrypoint, AsmList0), Filename), % parse assembly code
  !, % HACK Only consider one possible way to parse program
  AsmList1 = [ins('JMP', [Entrypoint])|AsmList0], % insert jump instruction to entrypoint
  label_mapping(AsmList1, Labelmapping), % make a mapping of labels to addresses
  filter_instruction(AsmList2, AsmList1), % only keep instructions in list
  maplist(make_ins_label_constant(Labelmapping), AsmList3, AsmList2), % convert all labels to constants
  maplist(asm, AsmList3, AsmList).

bigconst(X) :- X #=< 0x3FF, X #>= 0.

register_mapping('A', 0b00).
register_mapping('B', 0b01).
register_mapping('C', 0b10).
register_mapping('D', 0b11).

% conventions about naming in the `asm` predicates:
%  D  = destination register
%  Db = destination register, represented as bits
%  S  = source register
%  Sb = source register, represented as bits
%  M  = memory location
%  V  = constant value
%  R  = assembled instruction


asm(ins('MOV', [register(D), register(S)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  R #= 0b0000 + (Db << 4) + (Sb << 6).

asm(ins('LDV', [register(D), constant(V)]), R) :-
  register_mapping(D, Db),
  bigconst(V),
  R #= 0b0001 + (Db << 4) + (V << 6).

asm(ins('LDA', [register(D), constant(M)]), R) :-
  register_mapping(D, Db),
  bigconst(M),
  R #= 0b0010 + (Db << 4) + (M << 6).

asm(ins('LDM', [register(D), constant(M)]), R) :-
  register_mapping(D, Db),
  bigconst(M),
  R #= 0b0011 + (Db << 4) + (M << 6).

asm(ins('LDR', [register(D), register(S)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  R #= 0b0100 + (Db << 4) + (Sb << 6).

asm(ins('LDP', [register(D), register(S)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  R #= 0b0101 + (Db << 4) + (Sb << 6).

% ATH (arithmetic instructions)
%  Op          = which arithmetic instruction to execute
%  Mode        = store result in destination or source register
%  ShiftAmount = amount to shift by if shifting

asm(ins('ATH', [register(D), register(S), constant(Op), constant(Mode), constant(ShiftAmount)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  Op #=< 0b1111,
  Op #>= 0b0000,
  Mode #=< 0b1,
  Mode #>= 0b0,
  ShiftAmount #=< 0b111,
  ShiftAmount #>= 0b000,
  R #= 0b0110 + (Db << 4) + (Sb << 6) + (Op << 8) + (Mode << 12) + (ShiftAmount << 13).


% The helper function ath_asm replaces the intergers in Op, Mode and ShiftAmount with constant(integer)
asm(ins('ADD', [D, S]), R) :- ath_asm([D, S, 0b0000, 0, 0], R).
asm(ins('SUB', [D, S]), R) :- ath_asm([D, S, 0b0001, 0, 0], R).
asm(ins('MUL', [D, S]), R) :- ath_asm([D, S, 0b0010, 0, 0], R).
asm(ins('DIV', [D, S]), R) :- ath_asm([D, S, 0b0011, 0, 0], R).
asm(ins('LSH', [D, A]), R) :- ath_asm([D, D, 0b0100, 0, A], R).
asm(ins('RSH', [D, A]), R) :- ath_asm([D, D, 0b0101, 0, A], R).
asm(ins('AND', [D, S]), R) :- ath_asm([D, S, 0b0110, 0, 0], R).
asm(ins('OR',  [D, S]), R) :- ath_asm([D, S, 0b0111, 0, 0], R).
asm(ins('XOR', [D, S]), R) :- ath_asm([D, S, 0b1000, 0, 0], R).
asm(ins('NOT', [S]   ), R) :- ath_asm([S, S, 0b1001, 0, 0], R).
asm(ins('INC', [D]   ), R) :- ath_asm([D, D, 0b1010, 0, 0], R).
asm(ins('DEC', [D]   ), R) :- ath_asm([D, D, 0b1011, 0, 0], R).

% The `S` suffix means to store the result in the source register instead of in the destination
% Only `DIVS` and `SUBS` are implemented, because subtracting and dividing is not commutative.
% If you wanted to do `ADDS A, B`, you could just do `ADD B, A`
asm(ins('SUBS', [D, S]), R) :- ath_asm([D, S, 0b0001, 1, 0], R).
asm(ins('DIVS', [D, S]), R) :- ath_asm([D, S, 0b0011, 1, 0], R).


asm(ins('CAL', [register(D)]), R) :-
  register_mapping(D, Db),
  R #= 0b0011 + (Db << 4).

asm(ins('RET', []), R) :-
  R #= 0b1000.

asm(ins('JLT', [register(D), register(S)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  R #= 0b1001 + (Db << 4) + (Sb << 6).

asm(ins('PSH', [register(S)]), R) :-
  register_mapping(S, Sb),
  R #= 0b1001 + (Sb << 6).

asm(ins('PUSH', Args), R) :- asm(ins('PSH', Args), R).

asm(ins('POP', [register(D)]), R) :-
  register_mapping(D, Db),
  R #= 0b1001 + (Db << 4).

asm(ins('SYS', []), R) :-
  R #= 0b1100.

asm(ins('HLT', []), R) :-
  R #= 0b1101.

asm(ins('JMP', [constant(M)]), R) :-
  R #= 0b1110 + (M << 6).

asm(ins('JMR', [register(S)]), R) :-
  R #= 0b1111 + (S << 6).

% Helper function to make aliases for the ATH instruction
% This is below the asm definition because otherwise there are warnings about
% "Clauses of asm/2 are not together in the source-file"
ath_asm([D, S, O, M, Shift], R) :- asm(ins('ATH', [D, S, constant(O), constant(M), constant(Shift)]), R).


make_ins_label_constant(Labelmapping, ins(Opcode, ConstantArglist), ins(Opcode, SymbolicArglist)) :-
  maplist(make_label_constant(Labelmapping), SymbolicArglist, ConstantArglist).


% TODO is dit lelijk? Ben ik lelijk?
% Replace label by constant, otherwise keep existing argument
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
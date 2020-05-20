:- module(assembler, [assemble_to_ast/2, assemble_to_file/2, ast_from_file/2]).

:- use_module(library(clpfd)).
:- use_module(parser).


% Cut after doing file IO to prevent backtracking over closed files

assemble_to_file(AsmList, OutFilename) :-
  maplist(asm, AsmList, AsmList0),
  open(OutFilename, write, Fd, [type(binary)]),
  write_instructions(Fd, AsmList0),
  close(Fd), !.

ast_from_file(InFilename, AsmList) :-
  open(InFilename, read, Fd, [type(binary)]),
  read_instructions(Fd, AsmList0),
  maplist(asm, AsmList, AsmList0),
  close(Fd), !.


write_instructions(_, []).
write_instructions(Stream, [Instruction|Rest]) :-
  First #= (Instruction >> 8) /\ 0xff,
  Second #= (Instruction) /\ 0xff,
  put_byte(Stream, First),
  put_byte(Stream, Second),
  write_instructions(Stream, Rest).

% TODO find out what we want to do about order of In/Out in predicates

read_instructions(Stream, []) :- at_end_of_stream(Stream).
read_instructions(Stream, [Instruction|Rest]) :-
  get_byte(Stream, First),
  get_byte(Stream, Second),
  Instruction #= (First << 8) + Second,
  read_instructions(Stream, Rest).

assemble_to_ast(AsmList, Filename) :-
  phrase_from_file(parse_program(Entrypoint, AsmList0), Filename), % parse assembly code
  !, % HACK Only consider one possible way to parse program
  AsmList1 = [ins('JMP', [Entrypoint])|AsmList0], % insert jump instruction to entrypoint
  label_mapping(AsmList1, Labelmapping), % make a mapping of labels to addresses
  filter_instruction(AsmList2, AsmList1), % only keep instructions in list
  maplist(make_ins_label_constant(Labelmapping), AsmList, AsmList2). % convert all labels to constants


bigconst(X) :- X #=< 0x3FF, X #>= 0.

register_mapping('A', 0b00).
register_mapping('B', 0b01).
register_mapping('C', 0b10).
register_mapping('D', 0b11).


% explicitly define left shit in order for CLPFD to be able to reason back:
%  it can find R from B and A (left shift), but also A from B and R (inverse operation).
% This is needed because by default, the CLPFD library in swi-prolog is currently (8.0.3)
%  not able to reason back. Other prolog implementations might be able to do this natively
%  and more efficiently (looking at SICStus Prolog, developed by the author of the CLPFD library),
%  for these implementations, this function could be removed since it's probably less efficient

lsh(A, B, R) :-
  R #= A << B,
  A #= R >> B,
  R /\ (1<<B - 1) #= 0.

% conventions about naming in the `asm` predicates:
%  D  = destination register
%  Db = destination register, represented as bits
%  S  = source register
%  Sb = source register, represented as bits
%  M  = memory location
%  V  = constant value
%  R  = assembled instruction
%  any variable suffixed with `r`: the result of shifting it its place in the instruction


asm(ins('MOV', [register(D), register(S)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  lsh(Db, 4, Dr),
  lsh(Sb, 6, Sr),
  R #= 0b0000 + Dr + Sr.

asm(ins('LDV', [register(D), constant(V)]), R) :-
  register_mapping(D, Db),
  bigconst(V),
  lsh(Db, 4, Dr),
  lsh(V, 6, Vr),
  R #= 0b0001 + Dr + Vr.

asm(ins('LDA', [register(D), constant(M)]), R) :-
  register_mapping(D, Db),
  bigconst(M),
  lsh(Db, 4, Dr),
  lsh(M, 6, Mr),
  R #= 0b0010 + Dr + Mr.

asm(ins('LDM', [register(D), constant(M)]), R) :-
  register_mapping(D, Db),
  bigconst(M),
  lsh(Db, 4, Dr),
  lsh(M, 6, Mr),
  R #= 0b0011 + Dr + Mr.

asm(ins('LDR', [register(D), register(S)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  lsh(Db, 4, Dr),
  lsh(Sb, 6, Sr),
  R #= 0b0100 + Dr + Sr.

asm(ins('LDP', [register(D), register(S)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  lsh(Db, 4, Dr),
  lsh(Sb, 6, Sr),
  R #= 0b0101 + Dr + Sr.

% ATH (arithmetic instructions)
%  Op          = which arithmetic instruction to execute
%  Mode        = store result in destination or source register
%  ShiftAmount = amount to shift by if shifting

asm(ins('ATH', [register(D), register(S), constant(Op), constant(Mode), constant(ShiftAmount)]), R) :-
  % fail earlier in case we are going in the direction of binary instruction to AST
  %  this prevents us from having to calculate the rest if the instruction isn't even correct
  R /\ 0b1111 #= 0b0110,
  register_mapping(D, Db),
  register_mapping(S, Sb),
  Op #=< 0b1111,
  Op #>= 0b0000,
  Mode #=< 0b1,
  Mode #>= 0b0,
  ShiftAmount #=< 0b111,
  ShiftAmount #>= 0b000,
  lsh(Db, 4, Dr),
  lsh(Sb, 6, Sr),
  lsh(Op, 8, Opr),
  lsh(Mode, 12, Moder),
  lsh(ShiftAmount, 13, ShiftAmountr),
  % Force concrete instances of values here, this is needed because CLPFD
  %  is rather limited and doesn't see that this can be easily calculated in constant time.
  % An alternative, faster way to do this is to write the entire calculation out explicitly
  %  but this would be way more verbose and uglier
  label([Db, Sb, Op, Mode, ShiftAmount]),
  R #= 0b0110 + Dr + Sr + Opr + Moder + ShiftAmountr.


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
  lsh(Db, 4, Dr),
  R #= 0b0111 + Dr.

asm(ins('RET', []), R) :-
  R #= 0b1000.

asm(ins('JLT', [register(D), register(S)]), R) :-
  register_mapping(D, Db),
  register_mapping(S, Sb),
  lsh(Db, 4, Dr),
  lsh(Sb, 6, Sr),
  R #= 0b1001 + Dr + Sr.

asm(ins('PSH', [register(S)]), R) :-
  register_mapping(S, Sb),
  lsh(Sb, 6, Sr),
  R #= 0b1010 + Sr.

asm(ins('PUSH', Args), R) :- asm(ins('PSH', Args), R).

asm(ins('POP', [register(D)]), R) :-
  register_mapping(D, Db),
  lsh(Db, 4, Dr),
  R #= 0b1011 + Dr.

asm(ins('SYS', []), R) :-
  R #= 0b1100.

asm(ins('HLT', []), R) :-
  R #= 0b1101.

asm(ins('JMP', [constant(M)]), R) :-
  lsh(M, 6, Mr),
  R #= 0b1110 + Mr.

asm(ins('JMR', [register(S)]), R) :-
  register_mapping(S, Sb),
  lsh(Sb, 6, Sr),
  R #= 0b1111 + Sr.

% Helper function to make aliases for the ATH instruction
% This is below the asm definition because otherwise there are warnings about
% "Clauses of asm/2 are not together in the source-file"
ath_asm([D, S, O, M, Shift], R) :- asm(ins('ATH', [D, S, constant(O), constant(M), constant(Shift)]), R).


make_ins_label_constant(Labelmapping, ins(Opcode, ConstantArglist), ins(Opcode, SymbolicArglist)) :-
  maplist(make_label_constant(Labelmapping), SymbolicArglist, ConstantArglist).


% TODO is this ugly?
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
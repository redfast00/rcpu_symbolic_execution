:- module(interpreter, [add_instructions/1, trans/2]).

:- use_module(library(clpfd)).
:- use_module(assembler).
:- use_module(parser).

:- use_module(library(clpfd)).

:- dynamic(trans/2).

add_instructions(InstructionList) :-
  add_instructions(0, InstructionList).

add_instructions(N, []) :-
  access_IP(M, _, OldState),
  access_crashed(1, OldState, NewState),
  assertz(trans(OldState, NewState) :- M #> N).

add_instructions(N, [A|Rest]) :-
  add_clauses(N, A), N0 #= N+1, add_instructions(N0, Rest).

read_byte(virtual([ByteRead|InStack], OutStack), virtual(InStack, OutStack), ByteRead) :-
  ByteRead #=< 0xFF, ByteRead #>= 0.

read_byte(real(InStream, OutStream), real(InStream, OutStream), ByteRead) :-
  get_byte(InStream, ByteRead).

write_byte(virtual(InStack, OutStack), virtual(InStack, [ByteWritten|OutStack]), ToWrite) :-
  ByteWritten #= ToWrite /\ 0xFF.

write_byte(real(InStream, OutStream), real(InStream, OutStream), ByteWritten) :-
  Value #= ByteWritten /\ 0xFF,
  put_byte(OutStream, Value).

% Axioms about arrays

% The array is initialized with an init value
select_array(A, _, Value) :- A = init_array(Value).
% Accessing a element that was just written will result in that value
select_array(A, Idx, Value) :- A = store_array(_, Idx, Value).
% Overwriting an index will result in the value of that index changing to the new value
select_array(A, Idx, Value) :- A = store_array(B, OtherIdx, _), OtherIdx #\= Idx, select_array(B, Idx, Value).


read_memory(MemoryModule, Address, Value) :- select_array(MemoryModule, Address, Value).

write_memory(MemoryModule, Address, Value, MemoryModuleOut) :- MemoryModuleOut = store_array(MemoryModule, Address, Value).

% machine_state(_IP, _A, _B, _C, _D, _Stack, _IOContext, _Memory, _Crashed, _Halted).

access_reg('A', Value, IState, State) :-
  IState = machine_state(IP, _, B, C, D, Stack, IOContext, Memory, Crashed, Halted),
  State  = machine_state(IP, Value, B, C, D, Stack, IOContext, Memory, Crashed, Halted).
access_reg('B', Value, IState, State) :-
  IState = machine_state(IP, A, _, C, D, Stack, IOContext, Memory, Crashed, Halted),
  State  = machine_state(IP, A, Value, C, D, Stack, IOContext, Memory, Crashed, Halted).
access_reg('C', Value, IState, State) :-
  IState = machine_state(IP, A, B, _, D, Stack, IOContext, Memory, Crashed, Halted),
  State  = machine_state(IP, A, B, Value, D, Stack, IOContext, Memory, Crashed, Halted).
access_reg('D', Value, IState, State) :-
  IState = machine_state(IP, A, B, C, _, Stack, IOContext, Memory, Crashed, Halted),
  State  = machine_state(IP, A, B, C, Value, Stack, IOContext, Memory, Crashed, Halted).

access_IP(Value, IState, State) :-
  IState = machine_state(_, A, B, C, D, Stack, IOContext, Memory, Crashed, Halted),
  State  = machine_state(Value, A, B, C, D, Stack, IOContext, Memory, Crashed, Halted).

access_stack(Value, IState, State) :-
  IState = machine_state(IP, A, B, C, D, _, IOContext, Memory, Crashed, Halted),
  State  = machine_state(IP, A, B, C, D, Value, IOContext, Memory, Crashed, Halted).

access_io(Value, IState, State) :-
  IState = machine_state(IP, A, B, C, D, Stack, _, Memory, Crashed, Halted),
  State  = machine_state(IP, A, B, C, D, Stack, Value, Memory, Crashed, Halted).

access_mem(Value, IState, State) :-
  IState = machine_state(IP, A, B, C, D, Stack, IOContext, _, Crashed, Halted),
  State  = machine_state(IP, A, B, C, D, Stack, IOContext, Value, Crashed, Halted).

access_crashed(Value, IState, State) :-
  IState = machine_state(IP, A, B, C, D, Stack, IOContext, Memory, _, Halted),
  State  = machine_state(IP, A, B, C, D, Stack, IOContext, Memory, Value, Halted).

access_halted(Value, IState, State) :-
  IState = machine_state(IP, A, B, C, D, Stack, IOContext, Memory, Crashed, _),
  State  = machine_state(IP, A, B, C, D, Stack, IOContext, Memory, Crashed, Value).


ath_op(0b0000, D, S, _, R) :- R #= D + S.
ath_op(0b0001, D, S, _, R) :- R #= D - S.
ath_op(0b0010, D, S, _, R) :- R #= D * S.
ath_op(0b0011, D, S, _, R) :- R #= D / S.
ath_op(0b0100, _, S, Shift, R) :- R #= S << Shift.
ath_op(0b0101, _, S, Shift, R) :- R #= S >> Shift.
ath_op(0b0110, D, S, R) :- R #= D /\ S.
ath_op(0b0111, D, S, R) :- R #= D \/ S.
ath_op(0b1000, D, S, R) :- R #= D xor S.
ath_op(0b1001, _, S, R) :- R #= \ S.
ath_op(0b1010, D, _, R) :- R #= D + 1.
ath_op(0b1011, D, _, R) :- R #= D - 1.

add_clauses(N, ins('MOV', [register(Dest), register(Src)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Src, X, _, OldState),
  access_reg(Dest, X, OldState, NewState0),
  access_IP(NextIP, NewState0, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('LDV', [register(Dest), constant(V)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Dest, V, OldState, NewState0),
  access_IP(NextIP, NewState0, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('LDA', [register(Dest), constant(Address)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_mem(Memory, _, OldState),
  read_memory(Memory, Address, Value),
  access_reg(Dest, Value, OldState, NewState0),
  access_IP(NextIP, NewState0, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('LDM', [register(Src), constant(Address)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Src, Value, _, OldState),
  access_mem(Memory, _, OldState),
  write_memory(Memory, Address, Value, WrittenMemory),
  access_mem(WrittenMemory, OldState, NewState0),
  access_IP(NextIP, NewState0, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('LDR', [register(Dest), register(Src)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Src, Address, _, OldState),
  access_mem(Memory, _, OldState),
  read_memory(Memory, Address, Value),
  access_reg(Dest, Value, OldState, NewState0),
  access_IP(NextIP, NewState0, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('LDP', [register(Dest), register(Src)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Src, Value, _, OldState),
  access_mem(Memory, _, OldState),
  access_reg(Dest, Address, _, OldState),
  write_memory(Memory, Address, Value, WrittenMemory),
  access_mem(WrittenMemory, OldState, NewState0),
  access_IP(NextIP, NewState0, NewState),
  assertz(trans(OldState, NewState)).

%% TODO divide by zero crash

%% TODO rewrite this with terms
add_clauses(N, ins('ATH', [register(Dest), register(Src), constant(Op), constant(0), constant(Shift)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Src, S, _, OldState),
  access_reg(Dest, D, _, OldState),
  access_reg(Dest, R, OldState, NewState0),
  access_IP(NextIP, NewState0, NewState),
  assertz(trans(OldState, NewState) :- ath_op(Op, D, S, Shift, R)).

add_clauses(N, ins('CAL', [register(Dest)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Dest, Target, _, OldState),
  access_stack(Stack, _, OldState),
  access_stack([NextIP|Stack], OldState, NewState0),
  access_IP(Target, NewState0, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('RET', [])) :-
  access_IP(N, _, OldState),
  access_stack([Target|Stack], _, OldState),
  access_stack([Stack], OldState, NewState0),
  access_IP(Target, NewState0, NewState),
  assertz(trans(OldState, NewState)),

  access_IP(N, _, FailOldState),
  access_stack([], _, FailOldState),
  access_crashed(1, FailOldState, FailEndState),
  assertz(trans(FailOldState, FailEndState)).

add_clauses(N, ins('JLT', [register(Dest), register(Src)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Src, Target, _, OldState),
  access_reg(Dest, DestValue, _, OldState),
  access_reg('A', AValue, _, OldState),
  access_IP(Target, OldState, JumpedState),
  access_IP(NextIP, OldState, NotJumpedState),
  assertz(trans(OldState, JumpedState) :- AValue #< DestValue),
  assertz(trans(OldState, NotJumpedState) :- AValue #>= DestValue).

add_clauses(N, ins('PSH', [register(Dest)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_reg(Dest, Value, _, OldState),
  access_stack(Stack, _, OldState),
  access_stack([Value|Stack], OldState, NewState0),
  access_IP(NextIP, NewState0, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('POP', [register(Dest)])) :-
  access_IP(N, _, OldState),
  NextIP #= N + 1,
  access_stack([Value|Stack], _, OldState),
  access_stack([Stack], OldState, NewState0),
  access_reg(Dest, Value, NewState0, NewState1),
  access_IP(NextIP, NewState1, NewState),
  assertz(trans(OldState, NewState)),

  access_IP(N, _, FailOldState),
  access_stack([], _, FailOldState),
  access_crashed(1, FailOldState, FailEndState),
  assertz(trans(FailOldState, FailEndState)).

add_clauses(N, ins('SYS', [])) :-
  NextIP #= N + 1,

  % READ
  access_IP(N, _, ReadOldState),
  access_io(ReadIo0, _, ReadOldState),
  access_stack([0|ReadStack], _, ReadOldState),

  access_IP(NextIP, ReadOldState, ReadNewState0),
  access_stack([ByteRead|ReadStack], ReadNewState0, ReadNewState1),
  access_io(ReadIo, ReadNewState1, ReadNewState),
  assertz(trans(ReadOldState, ReadNewState) :- read_byte(ReadIo0, ReadIo, ByteRead)),

  % WRITE
  access_IP(N, _, WriteOldState),
  access_io(WriteIo0, _, WriteOldState),
  access_stack([1, WriteByte|WriteStack], _, WriteOldState),

  access_IP(NextIP, WriteOldState, WriteNewState0),
  access_stack(WriteStack, WriteNewState0, WriteNewState1),
  access_io(WriteIo, WriteNewState1, WriteNewState),
  assertz(trans(WriteOldState, WriteNewState) :- write_byte(WriteIo0, WriteIo, WriteByte)),

  % WRITE FAIL

  access_IP(N, _, WriteFailOldState),
  access_stack([1], _, WriteFailOldState),
  access_crashed(1, WriteFailOldState, WriteFailNewState),
  assertz(trans(WriteFailOldState, WriteFailNewState)),

  % Unimplemented syscall opcode FAIL

  access_IP(N, _, UnimplementedFailOldState),
  access_stack([X|_], _, UnimplementedFailOldState),
  access_crashed(1, UnimplementedFailOldState, UnimplementedFailNewState),
  assertz(trans(UnimplementedFailOldState, UnimplementedFailNewState) :- X #> 1),

  % Stack empty FAIL
  access_IP(N, _, EmptyStackFailOldState),
  access_stack([], _, EmptyStackFailOldState),
  access_crashed(1, EmptyStackFailOldState, EmptyStackFailNewState),
  assertz(trans(EmptyStackFailOldState, EmptyStackFailNewState)).

add_clauses(N, ins('HLT', [])) :-
  access_IP(N, _, OldState),
  access_halted(1, OldState, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('JMP', [constant(Target)])) :-
  access_IP(N, _, OldState),
  access_IP(Target, OldState, NewState),
  assertz(trans(OldState, NewState)).

add_clauses(N, ins('JMR', [register(Dest)])) :-
  access_IP(N, _, OldState),
  access_reg(Dest, Target, _, OldState),
  access_IP(Target, OldState, JumpedState),
  assertz(trans(OldState, JumpedState)).

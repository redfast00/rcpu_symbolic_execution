# Project proposal

## Description

RCPU is a 16-bit fantasy CPU architecture (that has however been
  implemented in
  [real hardware](https://github.com/redfast00/RCPU_FPGA)). The
  goal of this project is to explore [symbolic execution](https://en.wikipedia.org/wiki/Symbolic_execution) using Prolog.
  The goal of this project is to either
  find inputs satisfying constraints on arbitrary RCPU programs or
  to prove that there are no possible inputs satisfying the
  constraints. The constraints can be on the internal state of the
  RCPU machine, on the input and on the output.



## Goals

The following goals will be evaluated at the end of the project and
will be used in order to grade the project. Note that all example
programs can be replaced by arbitrary RPCU programs (this means that
manual extraction of constraints is impossible).

### Find an input for a program that results in an output of an even number

Example:

```
LDV A, 0
PSH A
SYS
POP A  ;; move char that was put in to the A register
LDV D, 1 ;; load 1 into D
ADD A, D ;; A = A + D
PUSH A
LDV A, 1
PSH A
SYS ;; output from stack to output
HLT ;; halt the machine
```

A possible input tape is here: `(5 (2 ()))`: only the first
character on the input tape will be consumed by the program and will
result in an output tape of `(6 ())`

### Find an input for a program that results in a certain configuration of the registers at program end

Example:

Constraints:

`A = 2*B, A > 1, C = 5, D = A + 1`

Program:
```
LDV A, 0
PSH A
SYS
PSH A
SYS
PSH A
SYS
PSH A
SYS
POP A
POP B
POP C
POP D
SUB D, A ;; D = D - A
HLT
```

Possible input:

`(2 (1 (5 (5 ()))))`

### Find an input for the program that results in the execution of a certain instruction.

Example:

Find an input that results in the `:win` label getting reached

Program:

```
LDV A, 5
LDV B, 0
PSH B
SYS
POP B
LDV C, good:
JLT B, C ;; jumps to C if A < B
JMP bad:

good:

win:
ADD C, D
HLT

bad:
HLT
```

### Find an input that results in certain configuration of values on the stack at program end

Example:

Constraints:

```
TOS[0] (top of stack)
TOS[1] (second item on stack)
...

TOS[0] = TOS[1], TOS[2] > 1
```
Program:
```
LDV A, 0
LDV C, 0
PSH A
SYS
PSH A
SYS
PSH A
SYS
PSH A
SYS
POP D
LDV A, 1
ADD D, A ;; D = D + A
JMP label:
PSH C
SYS ;; unreachable
label:
PSH C
SYS
PSH D
HLT
```

Possible input:

`(x (x (2 (4 (5 ())))))` where `x` isn't constrained

### Find an input that results in the RCPU-machine crashing

Example program will input a number that will be on the stack. It
will then return to that address.

```
LDV A, 0
PSH A
SYS
RET
HLT
HLT
HLT
```

Here, any input bigger than 7 will result in the RCPU machine jumping to undefined memory and crashing.


## Differences from [official RCPU specs](https://github.com/redfast00/RCPU)

- For input and output, only the `getc` and `putc`
  syscalls will be implemented: this will allow setting constraints
  on input and output, without having to implement complex routines
  like `printf`.
- It won't be possible to write self-modifying code: the instructions
  and data will live in a different address space.
- Machine arithmetic is hard in Prolog without adding a lot of `mod`
  predicates, negatively affecting performance. For this reason,
  machine arithmetic was not implemented

## Some examples of the RCPU machine crashing

Undefined behavior will also be seen as a crash

- Jumping to memory where there aren't any instructions
- Dividing by zero
- Popping from an empty stack
- ...

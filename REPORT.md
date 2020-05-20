# Logic programming project

## Description of task

The description of what had to be accomplished can be found in the
README. Summarized, an engine for symbolic execution of RCPU assembly had to be
written. Note that the assignments RCPU architecture deviated slightly from the
official RCPU specification in order to make it easier to implement. This implementation
is able to lift some of these original restrictions, leaning closer to the original RCPU
specifiation. Some other deviations from the RCPU spec had to be added. These deviations can
be found in their chapter

## Abilities of the engine

The engine is able to solve all example programs described in the assignment. In
addition to that, some extra goals were reached.

### Goals reached

- The program can assemble RCPU assembly source code to the RCPU binary format.
  It produces bit-for-bit identical binaries as the reference RCPU assembler.
- The program can execute both RCPU assembly source code and RCPU binaries.
- The program can reason over input, output and intermediate states of the RCPU
  machine. It's easy to implement additional constraints.


### Extra goals reached

The program can, for some programs, determine that there is no possible solution
and then `fail`s. This was implemented by having an infinite list of inputs, but
asserting that in the `_final` predicates, the remaining input has to be empty.
This ensures that Prolog will not try to construct increasingly longer lists of input:
it will now only generate up to however many items the program consumes in the path
where it consumes the most input.


## General design

All functionality of the program is accessible via `./cli.pl`.

### Assembling files

The file is first read, and then parsed with a DCG. (While
developing this parser, I was often frustrated that the
parser library didn't seem to be written to parse
languages. I preferred the Haskell Parser-monad because
I'm able to write much cleaner parser with it and I like
being able to build a parser from the ground up instead of
from a bunch of less-than-ideal building stones).

After the files are parsed into an AST (it's more like an
Abstract Syntax List instead of Tree, but AST is a well-
known term), a JMP instruction gets inserted, pointing to
the instruction where the program should start. Then all
labels in the assembly source are converted to concrete
values. This is easy to do because all RCPU instructions
have the same length: one 16-bit word: it requires one pass
to find all locations of the labels, and then another pass
to replace the labels by their value in each instruction.
In an instruction set like X86 with variable-length
instructions, this is much harder to do, even NP-hard: [^assembler-np-hard]

After replacing the labels with concrete values, each
instruction gets converted to its binary representation.
This happens in the `asm` predicate. Special care is taken
to ensure the `asm` predicate will work both ways efficiently:
both for converting an instruction from the AST to its
binary representation and vice-versa. This results in less
code-duplication and less possibility for the assembler and
the interpreter to be inconsistent with each other.

In order to get this property on the `asm` predicate,
some SWI-prolog limitations had to be overcome, mainly
the lack of support of inverting bitshifts in CLPFD. It
would be interesting to see whether other Prolog engine
implementations are able to do this more efficiently.

### Executing programs

Before executing the program, the AST-representation of
the program gets converted by the `add_clauses` predicates
into Prolog predicates, that than get dynamically added
to the knowledge database. This predicate has the form
`trans/2` and is valid when the second argument is a
possible machine state that follows the first state.
A machine state is the entire state of the machine
(instruction pointer, 4 registers, the stack, memory and
whether it's halted and/or crashed) and the state of the
IO interacting with the machine. There are two types of
IO-backers that are implemented: a `real(InStream, OutStream)`, where `InStream` and `OutStream` are
filedescriptors. This is used when executing RCPU code to read from stdin and write to stdout. A second kind of IO,
`virtual(InStack, OutStack)` is also implemented. Here
`InStack` and `OutStack` are both lists representing the
machine's input and output. When a character is read in
the `trans` predicate, it removes the head from the
`InStack` of the IO in the first state and then puts the
tail into the IO of the second state. This second form of
IO is used when symbolically executing programs: this
way, constraints can be placed on IO of the program.

RCPU can address 2**16 16-bit words. If the entire memory
state were to be passed around and modified in every
`trans/2` predicate, the engine would be too slow and
would waste too much memory to be practically useful. In
order to bypass these memory constraints, a different
data structure was used: instead of keeping track of the
state of the memory region, the program keeps track of
what writes happened to what memory addresses. This is
implemented as follows: there's are compound terms to
represent memory: `init_array(Value)` and
`write_memory(Previous, Address, Value)`; there are also
two predicates to read and write to memory. Using these
compound terms and predicates, a space-efficient memory
system was created. This memory system has the additional
advantage over keeping a large array of values that
symbolic reasoning over memory addresses, reads and
writes is possible.

After these `trans/2` predicates are added, the program
tries to construct a list of states, where each two
consecutive states are accepted by the `trans/2` predicate
and the last state has has to be accepted by a `..._final`
predicate. This ensures that the entire program execution
is valid, and that the final state of the RCPU program is
passes all constraints. Note that the final state is not
necessarily a state in which the machine is halted or
crashed: if for example we want to find inputs for a
program that when executed with these inputs reach a
certain instruction, the final state predicate is a
predicate that accepts states where the instruction
pointer is the desired location. This method of symbolic
execution is not new: it was described in
[^smt-execution]. The authors of that paper used the same
approach, but used Z3, a popular and open source SMT-
solver to solve the constraints. Some modifications were
added: every input-character is constrained to be a
number between 0 and 255 (inclusive) and the input stack
has to be empty when the final state is reached. This
prevents Prolog from backtracking with input stacks that
are too long to be consumed by the program.

## Testcases

There is a script `./test.sh`, that, when called, executes
all tests described in the original assignment and the
an additional test to check that the program reports a
failure when there is no solution for the constraints.

## Deviations from original assignment and from official RCPU spec

- (deviation from original assignment and RCPU spec): Machine arithmetic is hard
  in Prolog without adding a lot of `mod` predicates, negatively affecting performance
  and the ability of the CLPFD library. For this reason, machine arithmetic was not
  implemented. Note that there might be different Prolog engines that are able to solve
  problems with machine arithmetic efficiently, but SWI-Prolog in its current state
  (8.0.3) is not one of them.
- (deviation from the original assignment, but brings the implementation closer to the RCPU spec):
  The syscall interface (the way RCPU talks to the outside world by reading and writing
   characters) was changed to be closer to the original RCPU architecture. Instead of
  encoding what the syscall instruction does in the instruction itself, arguments
  are popped off the stack. This is harder to implement, since a variable amount
  of items are pushed to/popped from the stack, and because there are more conditions
  where the RCPU machine should crash (for example when the syscall to output a character
  is used, but there is no character on the stack). Note that the syscall numbers and
  what they do, are still different, because implementing a `printf` syscall that
  outputs `%`-formatted strings would be rather hard to implement correctly and efficiently.
- (deviation from original RCPU spec): Self-modifying code is not supported. The way
  the program is currently implemented, it reads the program, then dynamically adds
  constraints for each instruction in the program and then tries to reason over these
  constraints. If self-modifying code had to be supported, clauses of the transition
  predicate would have to be removed and added at runtime (or the program would have to
   be implemented in a different way). This would result in a lot more complexity.

## General feedback

I am grateful that the professor of this course allowed me
to propose a custom project that suited my interests. This
made doing the project much more rewarding, and also
motivated me much more than a regular project would. I
learned a lot from this project: about Prolog, SWI-Prolog
internals and about symbolic execution. I learned that
even for a very basic architecture like RCPU, symbolic
execution is still a hard problem.

## Footnotes

[^assembler-np-hard]: Boender J., Sacerdoti Coen C. (2014) On the Correctness of a Branch Displacement Algorithm. In: Ábrahám E., Havelund K. (eds) Tools and Algorithms for the Construction and Analysis of Systems. TACAS 2014. Lecture Notes in Computer Science, vol 8413. Springer, Berlin, Heidelberg

[^smt-execution]: Milicevic A., Kugler H. (2011) Model Checking Using SMT and Theory of Lists. In: Bobaru M., Havelund K., Holzmann G.J., Joshi R. (eds) NASA Formal Methods. NFM 2011. Lecture Notes in Computer Science, vol 6617. Springer, Berlin, Heidelberg
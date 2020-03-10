from z3 import *

MachineIntSort = BitVecSort(16)
MachineInt = lambda x: BitVec(x, 16)

def DeclareLinkedList(sort):
    LinkedList = Datatype(f'{sort.name()}_LinkedList')
    LinkedList.declare('nil')
    LinkedList.declare('cons', ('car', sort), ('cdr', LinkedList))
    return LinkedList.create()


State = Datatype('State')
State.declare('state',
    ('A', MachineIntSort),
    ('B', MachineIntSort),
    ('C', MachineIntSort),
    ('D', MachineIntSort))
State = State.create()

StateList = DeclareLinkedList(State)


def transition_condition(initial, next):
    return State.A(next) == State.A(initial) + 1

def final_condition(lst):
    return State.A(StateList.car(lst)) == 2

solver = Solver()
check_execution_trace = Function('check_execution_trace', StateList, BoolSort())
execution_list = Const('execution_list', StateList)


solver.add(ForAll(execution_list, check_execution_trace(execution_list) ==
    If(And(execution_list != StateList.nil, StateList.cdr(execution_list) != StateList.nil),
        And(
            transition_condition(StateList.car(execution_list), StateList.car(StateList.cdr(execution_list))),
            check_execution_trace(StateList.cdr(execution_list)),
            If(final_condition(StateList.cdr(execution_list)),
                StateList.nil == StateList.cdr(StateList.cdr(execution_list)),
                StateList.nil != StateList.cdr(StateList.cdr(execution_list))
            )
        ),
    True), # If False, unsat but incorrect. If True, it hangs
))

states = Const('states', StateList)

# Execution trace cannot be empty
solver.add(StateList.nil != states)

# Initial condition
solver.add(State.A(StateList.car(states)) == 0)

# Transition axiom
solver.add(check_execution_trace(states))

print(solver.check())
print(solver.model())



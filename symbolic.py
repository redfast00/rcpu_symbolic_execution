# from pyswip import Prolog
from architecture import instruction_mapping, register_mapping, alu_instruction_mapping

import struct
reg = {k:f'{v}_register' for k, v in register_mapping.items()}

# prolog = Prolog()
# prolog.consult("symbolic.pl")

def machine_state(instruction_pointer, halted=0, crashed=0):
    return f'machine_state({instruction_pointer}, A_register, B_register, C_register, D_register, Stack, Inputstack, Outputstack, Memory, {crashed}, {halted})'

def transition_function(current, next):
    return f'transition_condition({current}, {next})'

def compose(head, *limbs):
    return f'{head} :- {", ".join(limbs)}'

class CPU: 
    def encode_prolog(self, filename):
        with open(filename, "rb") as infile:
            raw = infile.read()
            instructions = struct.unpack(">" + "H" * (len(raw) // 2), raw)

        for instruction_pointer, instruction in enumerate(instructions):
            opcode, arguments = self.decode(instruction)
            results = self.execute(opcode, instruction_pointer, arguments)
            for result in results:
                print(f'{result}.')
        # Crash if we execute undefined memory
        head = transition_function(machine_state("X"), machine_state("X", crashed=1))
        body = f"X #>= {len(instructions)}"
        print(transition_function(machine_state("X")))

    def decode(self, instruction):
        '''Decodes the instruction as opcode, arguments'''
        opcode = instruction_mapping[instruction & 0b1111]
        arguments = instruction >> 4
        return opcode, arguments

    def execute(self, opcode, instruction_pointer, arguments):
        '''Executes the decoded instruction'''
        to_call = getattr(self, opcode)
        return to_call(instruction_pointer, arguments)

    ######################
    #    Instructions    #
    ######################
    def MOV(self, ip, arguments):
        destination = reg[arguments & 0b11]
        source = reg[(arguments >> 2) & 0b11]
        return transition_function(machine_state(ip), machine_state(ip + 1).replace(destination, source)),


    def LDV(self, ip, arguments):
        destination = reg[arguments & 0b11]
        value = arguments >> 2
        return transition_function(machine_state(ip), machine_state(ip + 1).replace(destination, str(value))),

    def LDA(self, ip, arguments):
        destination = reg[arguments & 0b11]
        memory_address = arguments >> 2
        head = transition_function(machine_state(ip), machine_state(ip + 1).replace(destination, "MemoryRead"))
        body = f"select_array(Memory, {memory_address}, MemoryRead)"
        return compose(head, body),

    def LDM(self, ip, arguments):
        destination = reg[arguments & 0b11]
        memory_address = arguments >> 2
        return transition_function(machine_state(ip), machine_state(ip + 1).replace("Memory", f"store_value(Memory, {memory_address}, {destination})")),

    def LDR(self, ip, arguments):
        destination = reg[arguments & 0b11]
        source = reg[(arguments >> 2) & 0b11]
        head = transition_function(machine_state(ip), machine_state(ip + 1).replace("Memory", f"store_value(Memory, {destination}, MemoryRead"))
        body = f"select_array(Memory, {source}, MemoryRead)"
        return compose(head, body),

    def LDP(self, ip, arguments):
        destination = reg[arguments & 0b11]
        source = reg[(arguments >> 2) & 0b11]
        return transition_function(machine_state(ip), machine_state(ip + 1).replace("Memory", f"store_value(Memory, {destination}, {source}")),

    def ATH(self, ip, arguments):
        destination = reg[arguments & 0b11]
        source = reg[(arguments >> 2) & 0b11]
        store_in_source_mode = (arguments >> 8) & 0b1
        saving_register = destination

        if store_in_source_mode:
            saving_register = source

        subinstruction = alu_instruction_mapping[(arguments >> 4) & 0b1111]
        shift_amount = arguments >> 9
        prolog_ath_map = {
            'ADD': 'DST + SRC',
            'SUB': 'DST - SRC',
            'MUL': 'DST * SRC',
            'DIV': 'DST / SRC',
            'LSH': 'DST << SHFT',
            'RSH': 'DST >> SHFT',
            'AND': 'DST /\\ SRC',
            'OR': 'DST \\/ SRC',
            'XOR': 'DST xor SRC',
            'NOT': '\\ SRC',
            'INC': 'DST + 1',
            'DEC': 'DST - 1'
        }

        calculation = prolog_ath_map[subinstruction].replace("DST", destination).replace("SRC", source).replace("SHFT", str(shift_amount))
        if subinstruction == "DIV":
            good_head = transition_function(machine_state(ip), machine_state(ip + 1).replace(saving_register, calculation))
            good_body = f'{source} #\= 0'
            good_case = compose(good_head, good_body)
            bad_head = transition_function(machine_state(ip), machine_state(ip, crashed=1))
            bad_body = f'{source} #= 0'
            bad_case = compose(bad_head, bad_body)
            return (good_case, bad_case)
        else:
            good_head = transition_function(machine_state(ip), machine_state(ip + 1).replace(saving_register, "Calculated"))
            good_body = f'Calculated #= {calculation}'
            return compose(good_head, good_body),


    def CAL(self, ip, arguments):
        destination = reg[arguments & 0b11]
        return transition_function(machine_state(ip), machine_state(destination).replace("Stack", f'[{ip + 1}|Stack]')),

    def RET(self, ip, arguments):
        good_case = transition_function(machine_state(ip).replace("Stack", "[ReturnAddress|Stack]"), machine_state("ReturnAddress"))
        bad_case =  transition_function(machine_state(ip).replace("Stack", "[]"), machine_state(ip, crashed=1).replace("Stack", "[]"))
        return (good_case, bad_case)

    def JLT(self, ip, arguments):
        destination = reg[arguments & 0b11]
        source = reg[(arguments >> 2) & 0b11]
        a_register = reg[0b00]
        good_head = transition_function(machine_state(ip), machine_state(source))
        good_body = f"{a_register} #< {destination}"
        good_case = compose(good_head, good_body)
        bad_head = transition_function(machine_state(ip), machine_state(ip + 1))
        bad_body = f"{a_register} #>= {destination}"
        bad_case = compose(bad_head, bad_body)
        return (good_case, bad_case)

    def PSH(self, ip, arguments):
        source = reg[(arguments >> 2) & 0b11]
        return transition_function(machine_state(ip), machine_state(ip + 1).replace("Stack", f'[{source}|Stack]')),

    def POP(self, ip, arguments):
        destination = reg[arguments & 0b11]
        good_case = transition_function(machine_state(ip).replace("Stack", "[PoppedValue|Stack]"), machine_state(ip + 1).replace(destination, "PoppedValue"))
        bad_case = transition_function(machine_state(ip).replace("Stack", "[]"), machine_state(ip, crashed=1).replace("Stack", "[]"))
        return (good_case, bad_case)

    def SYS(self, ip, arguments):
        # 0 is READ
        # 1, value is write value
        bad_base_case = transition_function(
            machine_state(ip).replace("Stack", "[]"),
            machine_state(ip, crashed=1))
        good_read_case = transition_function(
            machine_state(ip).replace("Stack", "[0|Stack]").replace("Inputstack", "[ValueRead|Inputstack]"),
            machine_state(ip + 1).replace("Stack", "[ValueRead|Stack]"))
        good_write_case = transition_function(
            machine_state(ip).replace("Stack", "[1,WriteValue|Stack]"),
            machine_state(ip + 1).replace("Outputstack", "[WriteValue|Outputstack]"))
        bad_write_case = transition_function(
            machine_state(ip).replace("Stack", "[1]"),
            machine_state(ip, crashed=1))
        return (bad_base_case, good_read_case, good_write_case, bad_write_case)

    def HLT(self, ip, arguments):
        return transition_function(machine_state(ip), machine_state(ip, halted=1)),

    def JMP(self, ip, arguments):
        address = arguments >> 2
        return transition_function(machine_state(ip), machine_state(address)),

    def JMR(self, ip, arguments):
        source = reg[(arguments >> 2) & 0b11]
        return transition_function(machine_state(ip), machine_state(source)),



CPU().encode_prolog("testcases/basic.out")

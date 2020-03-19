.text
.global main:
main:
LDV A, 0
PSH A
SYS ; syscall to input a char

POP C ;  move char that was read in the C register

LDV D, 1 ; load 1 into D

ADD C, D ; C = C + D

; Prepare stack for outputting C register
PSH C
LDV A, 1
PSH A
SYS

; halt the machine
HLT
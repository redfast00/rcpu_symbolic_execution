.text
.global main:
main:
LDV A, 0
PSH A
;; input a char
SYS
;  move char that was put in to the A register
POP C
; load 1 into D
LDV D, 1
; A = A + D
ADD C, D
PSH C
LDV A, 1
PSH A
; output from stack to output
SYS
; halt the machine
HLT
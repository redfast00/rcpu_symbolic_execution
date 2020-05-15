.text
.global main:
main:
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
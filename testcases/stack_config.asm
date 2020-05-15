.text
.global main:
main:
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
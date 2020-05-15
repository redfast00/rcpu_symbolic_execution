.text
.global main:
main:
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
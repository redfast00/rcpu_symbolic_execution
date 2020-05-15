.text
.global main:
main:
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
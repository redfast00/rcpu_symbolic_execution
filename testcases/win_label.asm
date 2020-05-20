.text
.global main:
main:
LDV A, 5
LDV B, 0
PSH B
SYS
POP B
LDV C, win:
;; jumps to C if A < B
;; <=> jumps to win if A < B
;; <=> jumps to win if 5 < B
;; win if input > 5
JLT B, C
JMP fail:

win:
;; letter W (win)
LDV A, 87
PSH A
LDV A, 1
PSH A
SYS
HLT

fail:
;; letter F
LDV A, 0
PSH A
LDV A, 1
PSH A
SYS
HLT
;;;   Advent of code 2018, day 21, part 1

	DEFAULT REL

	global    _main
	extern    _printf

section   .text

_main:
	xor 	r10, r10
	mov	rcx, 123
_1:
	and 	rcx, 456
	cmp 	rcx, 72
	jne 	_1

	xor 	rcx, rcx
_6:
	mov 	r8, rcx
	or 	r8, 0x10000
	mov 	rcx, 10605201

_8:
	mov 	r9, r8
	and 	r9, 0xff

	add	rcx, r9
	and 	rcx, 16777215
	mov 	rax, 65899
	mul	rcx
	mov 	rcx, rax
	and 	rcx, 16777215

	cmp	r8, 256
	jl	_28

	xor 	r9, r9

_18:
	mov 	rax, r9
	inc 	rax
	mov	r11, 0x100
	mul 	r11

	cmp 	rax, r8
	jg	_26

_24:
	inc 	r9
	jmp 	_18

_26:
	mov 	r8, r9
	jmp 	_8

_28:
	mov     rax, rcx
	push    rbx
	mov     rsi, rax
	xor     rax, rax
	lea     rdi, [msg]
	call    _printf
	pop     rbx
	ret

section .data

	msg    db      "%d", 10, 0


;;; #ip 3

;; r1  r2  r4  r5 r0
;; rcx r8 rax r9 r10

;; 0 :  seti 123 0 1		; rcx = 123
;; 1 :  bani 1 456 1		; rcx = rcx & 456

;; 2 :  eqri 1 72 1		; rcx = 1 IF rcx == 72 ELSE 0
;; 3 :  addr 1 3 3		; JMP 4 + rcx
;; 4 :  seti 0 0 3		; JMP 1

;; 5 :  seti 0 0 1		; rcx = 0

;; 6 :  bori 1 65536 2		; r8 = rcx | 0x10000
;; 7 :  seti 10605201 9 1	; rcx = 10605201
;; 8 :  bani 2 255 5    	; r9 = r8 & 0xff
;; 9 :  addr 1 5 1		; rcx = rcx + r9
;; 10 : bani 1 16777215 1	; rcx = rcx & 16777215
;; 11 : muli 1 65899 1  	; rcx = rcx * 65899
;; 12 : bani 1 16777215 1 	; rcx = rcx & 16777215

;; 13 : gtir 256 2 5    	; r9 = 1 IF 256 > r8 ELSE 0
;; 14 : addr 5 3 3		; JMP 15 + r9
;; 15 : addi 3 1 3		; JMP 17
;; 16 : seti 27 3 3		; JMP 28

;; 17 : seti 0 3 5		; r9 = 0
;; 18 : addi 5 1 4		; rax = r9 + 1
;; 19 : muli 4 256 4		; rax = rax * 0x100

;; 20 : gtrr 4 2 4		; rax = 1 IF rax > r8
;; 21 : addr 4 3 3		; JMP 22 + rax
;; 22 : addi 3 1 3		; JMP 24
;; 23 : seti 25 3 3		; JMP 26

;; 24 : addi 5 1 5		; INC r9
;; 25 : seti 17 5 3		; JMP 18

;; 26 : setr 5 5 2		; r8 = r9
;; 27 : seti 7 6 3		; JMP 8

;; 28 : eqrr 1 0 5		; r9 = 1 IF rcx == r10 ELSE 0
;; 29 : addr 5 3 3		; JMP 30 + r9
;; 30 : seti 5 8 3		; JMP 6

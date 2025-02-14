;***********************************************************
;*	This is the skeleton file for Lab 4 of ECE 375
;*
;*	 Author: Darren Mai and Joseph Serra
;*	   Date: 2/7/2025
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	mpr2 = r19				; Second multipurpose register
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0056					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine

		; Initialize Stack Pointer
    ldi r16, high(RAMEND)  ; Load high byte of RAMEND
    out SPH, r16           ; Set high byte of stack pointer
    ldi r16, low(RAMEND)   ; Load low byte of RAMEND
    out SPL, r16           ; Set low byte of stack pointer

		; TODO

		clr		zero			; Set the zero register to zero, maintain
										; these semantics, meaning, don't
										; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program

		; Call function to load ADD16 operands
		rcall	LOAD_ADD16_OPERANDS
		nop ; Check load ADD16 operands (Set Break point here #1)

		; Call ADD16 function to display its results (calculate FCBA + FFFF)
		rcall	ADD16
		nop ; Check ADD16 result (Set Break point here #2)


		rcall LOAD_SUB16_OPERANDS	; Call function to load SUB16 operands
		nop ; Check load SUB16 operands (Set Break point here #3)

		rcall SUB16	; Call SUB16 function to display its results (calculate FCB9 - E420)
		nop ; Check SUB16 result (Set Break point here #4)


		rcall LOAD_MUL24_OPERANDS; Call function to load MUL24 operands
		nop ; Check load MUL24 operands (Set Break point here #5)

		rcall MUL24 ; Call MUL24 function to display its results (calculate FFFFFF * FFFFFF)

 		nop ; Check MUL24 result (Set Break point here #6)

		rcall COMPOUND_SETUP ; Setup the COMPOUND function direct test
		nop ; Check load COMPOUND operands (Set Break point here #7)

		rcall COMPOUND ; Call the COMPOUND function
		nop ; Check COMPOUND result (Set Break point here #8)

DONE:	rjmp	DONE			; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************


;-----------------------------------------------------------
; Func: LOAD_ADD16_OPERANDS
; Desc: Loads the operands for ADD16 into memory
;-----------------------------------------------------------
LOAD_ADD16_OPERANDS:
		push r0
		
		ldi		ZL, low(OperandA * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandA * 2)

		ldi		YL, low(ADD16_OP1)	; Destination for operand 1
		ldi		YH, high(ADD16_OP1)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y, r0

		ldi		ZL, low(OperandB * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandB * 2)

		ldi		YL, low(ADD16_OP2)	; Destination for operand 2
		ldi		YH, high(ADD16_OP2)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y, r0

		pop r0

		ret

;-----------------------------------------------------------
; Func: LOAD_SUB16_OPERANDS
; Desc: Loads the operands for SUB16 into memory
;-----------------------------------------------------------
LOAD_SUB16_OPERANDS:
		push r0

		ldi		ZL, low(OperandC * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandC * 2)

		ldi		YL, low(SUB16_OP1)	; Destination for operand 1
		ldi		YH, high(SUB16_OP1)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y, r0

		ldi		ZL, low(OperandD * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandD * 2)

		ldi		YL, low(SUB16_OP2)	; Destination for operand 2
		ldi		YH, high(SUB16_OP2)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y, r0

		pop r0

		ret

LOAD_MUL24_OPERANDS:
		push r0
		
		ldi		ZL, low(OperandE1 * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandE1 * 2)

		ldi		YL, low(MUL24_OP1)	; Destination for operand 1
		ldi		YH, high(MUL24_OP1)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0

		ldi		ZL, low(OperandE2 * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandE2 * 2)

		lpm							; load program memory pointed to by Z into r0
		st		Y, r0

		ldi		ZL, low(OperandF1 * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandF1 * 2)

		ldi		YL, low(MUL24_OP2)	; Destination for operand 2
		ldi		YH, high(MUL24_OP2)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0

		ldi		ZL, low(OperandF2 * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandF2 * 2)

		lpm							; load program memory pointed to by Z into r0
		st		Y, r0

		pop r0

		ret

COMPOUND_SETUP:
		; Setup SUB16 with operands G and H
		ldi		ZL, low(OperandG * 2)
		ldi		ZH, high(OperandG * 2)

		ldi		YL, low(SUB16_OP1)	; Destination for operand 1
		ldi		YH, high(SUB16_OP1)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y, r0

		ldi		ZL, low(OperandH * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandH * 2)

		ldi		YL, low(SUB16_OP2)	; Destination for operand 2
		ldi		YH, high(SUB16_OP2)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y, r0
		; Perform subtraction to calculate G - H
		rcall SUB16

		; Setup the ADD16 function with SUB16 result and operand I
		ldi		ZL, low(OperandI * 2)	; only the Z-register can access program memory
		ldi		ZH, high(OperandI * 2)

		ldi		YL, low(ADD16_OP1)	; Destination for operand 1
		ldi		YH, high(ADD16_OP1)

		lpm							; load program memory pointed to by Z into r0
		st		Y+, r0
		adiw	ZH:ZL, 1
		lpm							; load program memory pointed to by Z into r0
		st		Y, r0

		ldi		XL, low(SUB16_Result)
		ldi		XH, high(SUB16_Result)

		ldi		YL, low(ADD16_OP2)	; Destination for operand 2
		ldi		YH, high(ADD16_OP2)

		ld		mpr, X+							
		st		Y+, mpr
		ld		mpr, X
		st		Y, mpr

		; Perform addition next to calculate (G - H) + I
		rcall ADD16

		; Setup the MUL24 function with ADD16 result as both operands

		ldi		XL, low(ADD16_Result)
		ldi		XH, high(ADD16_Result)

		ldi		YL, low(MUL24_OP1)	; Destination for operand 1
		ldi		YH, high(MUL24_OP1)

		ldi		ZL, low(MUL24_OP2)	; Destination for operand 1
		ldi		ZH, high(MUL24_OP2)

		ld		mpr, X+
		st		Y+, mpr
		st		Z+, mpr
		ld		mpr, X+
		st		Y+, mpr
		st		Z+, mpr
		ld		mpr, X
		st		Y, mpr
		st		Z, mpr
;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;       where the high byte of the result contains the carry
;       out bit.
;-----------------------------------------------------------
ADD16:
    push    mpr
    push    mpr2

    ldi     XL, low(ADD16_OP1)
    ldi     XH, high(ADD16_OP1)

    ldi     YL, low(ADD16_OP2)
    ldi     YH, high(ADD16_OP2)

    ldi     ZL, low(ADD16_Result)
    ldi     ZH, high(ADD16_Result)

    ld      mpr,  X+
    ld      mpr2, Y+
    add     mpr2, mpr
    st      Z+,   mpr2

    ld      mpr,  X
    ld      mpr2, Y
    adc     mpr2, mpr
    st      Z+,   mpr2

    brcc    EXITadd
    st		X, XH

EXITadd:
    pop     mpr2
    pop     mpr
    ret



;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;       result. Always subtracts from the bigger values.
;-----------------------------------------------------------
SUB16:
    push    mpr
    push    mpr2

    ; Load pointer to OP1 into X
    ldi     XL, low(SUB16_OP1)
    ldi     XH, high(SUB16_OP1)

    ; Load pointer to OP2 into Y
    ldi     YL, low(SUB16_OP2)
    ldi     YH, high(SUB16_OP2)

    ; Load pointer to SUB16_Result into Z
    ldi     ZL, low(SUB16_Result)
    ldi     ZH, high(SUB16_Result)

    ; --- Subtract lower bytes (OP1 low - OP2 low) ---
    ld      mpr,  X+     ; mpr  = OP1.low
    ld      mpr2, Y+     ; mpr2 = OP2.low

    ; Now do mpr = OP1.low - OP2.low
    sub     mpr, mpr2    ; mpr = mpr - mpr2  => OP1.low - OP2.low
    st      Z+, mpr      ; Store low byte of result

    ; --- Subtract higher bytes (with borrow) ---
    ld      mpr,  X      ; mpr  = OP1.high
    ld      mpr2, Y      ; mpr2 = OP2.high

    ; mpr = OP1.high - OP2.high - borrow
    sbc     mpr, mpr2
    st      Z+, mpr      ; Store high byte of result

    ; Optional: if you want to detect borrow-out (carry clear):
    brcc    EXITsub
    ; If carry is clear => borrowed
    ; For instance, store a sign or do something special:
    ; st Z, XH   ; (Sample action from your snippet)

EXITsub:
    pop     mpr2
    pop     mpr
    ret


;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit
;       result.
;-----------------------------------------------------------
MUL24:
;* - Simply adopting MUL16 ideas to MUL24 will not give you steady results. You should come up with different ideas.
		; Execute the function here
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero

		ldi		XL, low(MUL24_OP1)
		ldi		XH, high(MUL24_OP1)

		ldi		YL, low(MUL24_OP2)
		ldi		YH, high(MUL24_OP2)

		ldi		ZL, low(MUL24_Result)
		ldi		ZH, high(MUL24_Result)

		ldi     mpr, 6
		CLR_RESULT:
		st      Z+, zero
		dec     mpr
		brne    CLR_RESULT

		; Reset Z pointer
		sbiw    ZL, 6

		ldi		oloop, 3
		MUL24_OLOOP:
		ldi		iloop, 3
		MUL24_ILOOP:
		ld		A , X+
		ld		B, Y
		mul		A, B
		ld		A, Z+
		ld		B, Z+
		add		rlo, A
		adc		rhi, B
		ld		A, Z+
		adc		A, zero
		ld		B, Z
		adc		B, zero
		st		Z, B                            
		st		-Z, A
		st		-Z, rhi
		st		-Z, rlo
		adiw	Z, 1
		dec		iloop
		brne	MUL24_ILOOP
		; inner loop ends
		sbiw	Z, 2	
		sbiw	X, 3	; set X back at the start of operand 1
		adiw	Y, 1	; increment our operand 2 
		dec		oloop
		brne	MUL24_OLOOP

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((G - H) + I)^2
;       by making use of SUB16, ADD16, and MUL24.
;
;       D, E, and F are declared in program memory, and must
;       be moved into data memory for use as input operands.
;
;       All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:
		; load everything, perform subtraction and addition, ready for 24-bit multiplication
		; Perform multiplication to calculate ((G - H) + I)^2
		rcall MUL24
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;       A - Operand A is gathered from address $0101:$0100
;       B - Operand B is gathered from address $0103:$0102
;       Res - Result is stored in address
;             $0107:$0106:$0105:$0104
;       You will need to make sure that Res is cleared before
;       calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;       beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variable by pushing them to the stack

		; Execute the function here

		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;*	Do not  section.
;***********************************************************
; ADD16 operands
OperandA:
	.DW 0xFCBA
OperandB:
	.DW 0xFFFF

; SUB16 operands
OperandC:
	.DW 0XFCB9
OperandD:
	.DW 0XE420

; MUL24 operands
OperandE1:
	.DW	0XFFFF
OperandE2:
	.DW	0X00FF
OperandF1:
	.DW	0XFFFF
OperandF2:
	.DW	0X00FF

; Compound operands ((G - H) + I)^2
OperandG:
	.DW	0xFCBA				; test value for operand G
OperandH:
	.DW	0x2022				; test value for operand H
OperandI:
	.DW	0x21BB				; test value for operand I

;***********************************************************
;*	Data Memory Allocation
;***********************************************************
.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.
.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result


; data memory allocation for SUB16
.org    $0130
SUB16_OP1:
        .byte 2   ; Allocate two bytes for the first operand of SUB16
SUB16_OP2:
        .byte 2   ; Allocate two bytes for the second operand of SUB16

.org    $0140
SUB16_Result:
        .byte 2   ; Allocate two bytes for the SUB16 result

; data memory allocation for MUL24
.org    $0150
MUL24_OP1:
        .byte 3
MUL24_OP2:
        .byte 3

.org    $0160
MUL24_Result:
        .byte 6   

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program

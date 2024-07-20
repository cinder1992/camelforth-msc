
; Listing 2.
; ===============================================
; CamelForth for the RCA 1802
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; Copyright (c) 2009 Harold Rabbie RCA 1802 port
; Copyright (c) 2024 Neil Ray - Membership Card port
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
;
; Revision history:
; 2009-02-09	First version for RCA 1802 based on Z80 CamelForth
;
;
; ===============================================
; CAMEL18.ASM: Code Primitives
;   Source code is for the A180 assembler.
;   Forth words are documented as follows:
;x   NAME     stack -- stack    description
;   where x=C for ANS Forth Core words, X for ANS
;   Core Extensions, S for ANS String words
;   T for ANS Tools words, D for ANS Double words
;   Z for internal or private words.
;
; Direct-Threaded Forth model for RCA 1802
; 16 bit cell, 8 bit char, 8 bit (byte) adrs unit
;
; ===============================================
;
;	Stack Layouts
;
;	PSP->	TOS.LO		RSP->	RET.HI
;		TOS.HI			RET.LO
;		NEXT.LO
;		NEXT.HI
;
; Both stacks grow from high to low
; Parameter stack is stored little-endian
; Return stack is stored big-endian
; Compiled 16-bit data is stored big-endian
;
; ANSI 3.1.4.1 requires doubles to be stored with the MORE
; significant cell at the top of the stack. Therefore, doubles
; are stored mixed-endian as follows
;
;	PSP->	Byte [2]
;		Byte [3]	; MSB
;		Byte [0]	; LSB
;		Byte [1]
;
; ===============================================
; Memory map:
	.equ	paramstack,	H'7B00	; top of parameter stack	grows down
	.equ	returnstack,	H'7C00	; top of return stack		grows down
	.equ	userarea,	H'7C00	; user area, page aligned	grows up
	.equ	tibarea,	H'7D00	; Terminal Input Buffer		grows up
	.equ	tibend,		H'7E00	; end+1 of Terminal Buffer
	.equ 	padarea,	H'7F00	; User Pad Buffer		grows down
	.equ	leavestack,	H'7F00	; top of leave stack		grows up

	.equ	reset,H'0000 		; cold start, Forth kernel, dictionary

; ===============================================
; Register usage
	.equ	codepc,0	; PC for code words
	.equ	ip,1		; Forth interpreter pointer
	.equ	psp,2		; Forth parameter stack pointer
	.equ	rsp,3		; Forth return stack pointer
	.equ	nextpc,4	; PC for Forth inner interpreter
	.equ	colonpc,5	; PC for colon definitions
	.equ	constpc,6	; PC for CONSTANT definitions
	.equ	varpc,7		; PC for VARIABLE and CREATE1 definitions
	.equ	createpc,8	; PC for CREATE definitions
	.equ	userpc,9	; PC for USER definitions
	.equ	baudr,10	; baud rate register, set by GENBAUD

	.equ	temppc,15	; temporary registers
	.equ	temp1,15
	.equ	temp2,14
	.equ	temp3,13
	.equ	temp4,12

; ===============================================
; Execution begins here
	.org	reset		; cold start address
				; initialize registers
	ldi paramstack & H'0FF
	plo psp
	ldi paramstack >> 8
	phi psp
	ldi returnstack & H'0FF
	plo rsp
	ldi returnstack >> 8
	phi rsp
	ldi nextd & H'0FF
	plo nextpc
	ldi nextd >> 8
	phi nextpc
	ldi docolon & H'0FF
	plo colonpc
	ldi docolon >> 8
	phi colonpc
	ldi doconst & H'0FF
	plo constpc
	ldi doconst >> 8
	phi constpc
	ldi dovar & H'0FF
	plo varpc
	ldi dovar >> 8
	phi varpc
	ldi docreate & H'0FF
	plo createpc
	ldi docreate >> 8
	phi createpc
	ldi douser & H'0FF
	plo userpc
	ldi douser >> 8
	phi userpc

	sex psp		; do arithmetic on param stack
	lbr truecold

; INTERPRETER LOGIC =============================
; See also "defining words" at end of this file
;
	sep codepc
nextd:	 		; call with SEP nextpc
	lda ip		;
	phi codepc
	lda ip
	plo codepc
	br nextd - 1

	.set link,0		; end of dictionary

;C EXIT     --      exit a colon definition
	.dw link
	.db 0
	.set link,*
	.db 4,"EXIT"
EXIT:
	lda rsp
	phi ip
	lda rsp
	plo ip
	sep nextpc

;Z lit      -- x    fetch inline literal to stack
; This is the primitive compiled by LITERAL.
	.dw link
	.db 0
	.set link,*
	.db 3,"lit"
lit:
	lda ip	; high byte
 	dec psp
	stxd
	lda ip	; low byte
	str psp
	sep nextpc

;C EXECUTE   i*x xt -- j*x   execute Forth word
;                            at 'xt'
	.dw link
	.db 0
	.set link,*
	.db 7,"EXECUTE"
EXECUTE:
	ldi ex1 & H'0FF	; switch to another PC
	plo temppc	; because we need to enter
	ldi ex1 >> 8	; code fields with codepc
	phi temppc
	sep temppc
ex1:
	lda psp		; lo byte
 	plo codepc
	lda psp		; hi byte
	phi codepc
	sep codepc

; DEFINING WORDS ================================

; ENTER, a.k.a. DOCOLON, entered by sep colonpc
; to enter a new high-level thread (colon def'n.)
; (internal code fragment, not a Forth word)

	sep nextpc
docolon:
	glo ip		; push IP to return stack
	dec rsp
	str rsp
	ghi ip		; hi byte
	dec rsp
	str rsp
	ghi codepc	; get PFA
	phi ip
	glo codepc
	plo ip
	br docolon - 1	; reset colonpc

;C VARIABLE   --      define a Forth variable
;   CREATE1 CELL ALLOT ;

	.dw link
	.db 0
	.set link,*
	.db 8,"VARIABLE"
VARIABLE:
	sep colonpc
	.dw CREATE1,CELL,ALLOT,EXIT

; DOVAR, code action of VARIABLE, entered by sep varpc

	sep nextpc
dovar:  		; -- a-addr
	ghi codepc	; high byte
	dec psp
	stxd
	glo codepc	; low byte
	str psp
	br dovar - 1	; reset varpc

; DOCREATE, code action of CREATE'd word
	sep codepc
docreate:
	lda codepc		; high byte of DOES> part
	phi temppc
	lda codepc		; low byte of DOES>
	plo temppc

	ghi codepc		; push PFA to param stack
	dec psp
	stxd
	glo codepc
	str psp

	ghi temppc		; need to enter code field
	phi codepc		; with codepc
	glo temppc
	plo codepc
	br docreate - 1	; reset createpc

;C CONSTANT   n --      define a Forth constant
;   (CREATE) CFCOMPILE doconst ,
	.dw link
	.db 0
	.set link,*
	.db 8,"CONSTANT"
CONSTANT:
	sep colonpc
	.dw XCREATE,CFCOMPILE
	sep constpc
	.dw COMMA,EXIT

; DOCONST, code action of CONSTANT,
; entered by sep constpc

	sep nextpc
doconst:  ; -- x
	lda codepc		; high byte
	dec psp
	stxd
	lda codepc		; low byte
	str psp
	br doconst - 1	; reset constpc

;Z USER     n --        define user variable 'n'
;   (CREATE) CFCOMPILE douser ,
	.dw link
	.db 0
	.set link,*
	.db 4,"USER"
USER:
	sep colonpc
	.dw XCREATE,CFCOMPILE
	sep userpc
	.dw COMMA,EXIT

; DOUSER, code action of USER,
; entered by sep userpc

	sep nextpc
douser:  ; -- a-addr	; assumes user area is page-aligned
			; and no more than 256 user variables
	ldi userarea >> 8	; address high byte
	dec psp
	stxd
	inc codepc	; point to LSB of user offset
	lda codepc	; ldn codepc is IDL!
	str psp
	br douser - 1	; reset userpc

;Z GENBAUD	--	detect the BAUD rate and set the baudr register
	.dw link
	.db 0
	.set link,*
	.db 7,"genbaud"
GENBAUD:			;A. wait for Start bit
	bn3  GENBAUD	;   loop while idle (EF3 false, EF3 pin high)
					;    continue when Start bit detected
					;   (EF3 true, EF3 pin goes low)
LoopB:				;B. wait for Data bit D5=1 to begin
	b3 LoopB		;   loop while EF3 true (EF3 pin low)
					;   continue when EF3 false (pin goes high)
	ldi 0			;1
	nop				;2.5
	nop				;4
	plo baudr		;5	long nop, timed for the recv command
	nop				;6.5 2 NOPs for 3 instruction times
	nop				;8   (best detection margin at 9600 baud)
Time:				;   measure remaining time in Data bit D5
	adi 1			;9  D=D+1 for each loop
	bn3  Time		;10 ...loop until end of bit
LoopC:
	b3	LoopC		; Wait for stop bit
	;str psp			;store on the stack
	;out 4			;debug baudrate constant
	;dec psp
	phi baudr
	sep nextpc

;C KEY		  -- c	gets a keycode from the console
	.dw link
	.db 0
	.set link,*
	.db 3,"KEY"
KEY:
	ldi 0
	dec psp
	stxd			;low high byte
	ldi H'FF		;Initialize the byte
	str psp			;Put it on the stack
	ghi baudr		;put the baud constant into D
keyloop:
	bn3	keyloop		;wait for start bit
	shr				;shift right to half the delay
	skp				;skip over the get delay
nextbit:
	ghi baudr		;1 get delay constant
delay1:
	smi 1			;2 decrement delay
	bnz delay1		;3 loop until 0, DF = 1
	b3 keyzero		;4 test next bit
	skp				;5 leave DF = 1 if bit is 1
keyzero:	
	shr				;5 set DF = 0 if bit is 0
	ldn psp			;6 get byte
	shrc			;7 shift bit into stack, D7 = DF, DF = D0
	str psp			;8 store byte on stack
	lbdf nextbit	;9.5 keep going until you encounter the STOP bit
	;out 4			;output the byte received to the LEDs
	ghi baudr
delay2:
	smi 1
	bnz delay2		;delay until the end of the stop bit
	;dec psp			;point back to the received character
	sep nextpc
	
;C EMIT     c --    output character to console
	.dw link
	.db 0
	.set link,*
	.db 4,"EMIT"
EMIT:
	ldi 0
	shr		;DF = 0
emit1:
	bdf emitloop	;9 detect carry	
	seq			;10 generate 0 bit
	skp			;11 skip
emitloop: 
	req			;12 generate 1 bit
	ghi baudr	;13 get baud constant
	;smi 0
emitdelay:
	smi 1		;2 
	bnz emitdelay	;3
	ldx		;4 get bit
	shrc		;5 D0 = DF, D7 = 1
	str psp		;6 put bit back
	xri H'FF	;7 check if we have 0xFF
	bnz emit1   ;8 
	bdf emit2	; generate last bit
	seq
	skp
emit2:
	req
	ghi baudr	;get the baud constant
	;adi 0
emitend:
	smi 1
	bnz emitend
	inc psp	;pop char off of stack
	inc psp
	req
	ghi baudr
	;adi 5
emitend2:
	smi 1
	bnz emitend2
	sep nextpc	;processing will cover the STOP bit time

;X BYE	i*x --	return to the monitor
	.dw link
	.db 0
	.set link,*
	.db 3,"BYE"
BYE:
	lbr H'8B5E
	sep nextpc

; STACK OPERATIONS ==============================

;C ?DUP     x -- 0 | x x    DUP if nonzero
	.dw link
	.db 0
	.set link,*
	.db 4,"?DUP"
QDUP:
	lda psp		; get low byte
	or		; point to high byte
	dec psp
	bnz DUP
	sep nextpc

;C DUP      x -- x x      duplicate top of stack
	.dw link
	.db 0
	.set link,*
	.db 3,"DUP"
DUP:
	lda psp		; lo byte
	plo temp1
	ldn psp		; high byte
	dec psp
	dec psp
	stxd
	glo temp1
	str psp
	sep  nextpc

;C DROP     x --          drop top of stack
	.dw link
	.db 0
	.set link,*
	.db 4,"DROP"
DROP:
	inc psp
	inc psp
	sep nextpc

;C SWAP     x1 x2 -- x2 x1    swap top two items
	.dw link
	.db 0
	.set link,*
	.db 4,"SWAP"
SWAP:
	lda psp		; x2 lo
	plo temp2
	lda psp		; x2 hi
	phi temp2
	lda psp		; x1 lo
	plo temp1
	ldn psp		; x1 hi
	phi temp1

	ghi temp2
	stxd
 	glo temp2
	stxd
	ghi temp1
	stxd
	glo temp1
	str psp
	sep nextpc

;C OVER    x1 x2 -- x1 x2 x1   per stack diagram
	.dw link
	.db 0
	.set link,*
	.db 4,"OVER"
OVER:
	inc psp
	inc psp
	lda psp		; x1 lo
	plo temp1
	ldn psp		; x1 hi

	dec psp
	dec psp
	dec psp
	dec psp
	stxd
	glo temp1
	str psp
	sep nextpc

;C ROT    x1 x2 x3 -- x2 x3 x1  per stack diagram
	.dw link
	.db 0
	.set link,*
	.db 3,"ROT"
ROT:
	lda psp
	plo temp3
	lda psp
	phi temp3
	lda psp
	plo temp2
	lda psp
	phi temp2
	lda psp
	plo temp1
	ldn psp
	phi temp1

	ghi temp2
	stxd
	glo temp2
	stxd
	ghi temp3
	stxd
	glo temp3
	stxd
	ghi temp1
	stxd
	glo temp1
	str psp
	sep nextpc

;X NIP    x1 x2 -- x2           per stack diagram
	.dw link
	.db 0
	.set link,*
	.db 3,"NIP"
NIP:
	lda psp		; x2 lo
	plo temp2
	lda psp		; x2 hi
	inc psp
	stxd
	glo temp2
	str psp
	sep nextpc

;X TUCK   x1 x2 -- x2 x1 x2     per stack diagram
	.dw link
	.db 0
	.set link,*
	.db 4,"TUCK"
TUCK:
	lda psp		; x2 lo
	plo temp2
	lda psp		; x2 hi
	phi temp2
	lda psp		; x1 lo
	plo temp1
	ldn psp		; x1 hi
	phi temp1

	ghi temp2
	stxd
	glo temp2
	stxd
	ghi temp1
	stxd
	glo temp1
	stxd
	ghi temp2
	stxd
	glo temp2
	str psp
	sep nextpc

;C >R    x --   R: -- x   push to return stack
	.dw link
	.db 0
	.set link,*
	.db 2,">R"
TOR:
	lda psp		; x lo
	dec rsp
	str rsp
	lda psp		; x hi
	dec rsp
	str rsp
	sep nextpc

;C R>    -- x    R: x --   pop from return stack
	.dw link
	.db 0
	.set link,*
	.db 2,"R>"
RFROM:
	lda rsp		; x hi
	dec psp
	stxd
	lda rsp		; x lo
	str psp
	sep nextpc

;C R@    -- x     R: x -- x   fetch from rtn stk
	.dw link
	.db 0
	.set link,*
	.db 2,"R@"
RFETCH:
	lda rsp		; x hi
	dec psp
	stxd
	ldn rsp		; x lo
	str psp
	dec rsp
	sep nextpc

;Z SP@  -- a-addr       get data stack pointer
	.dw link
	.db 0
	.set link,*
	.db 3,"SP@"
SPFETCH:
	glo psp
	plo temp1
	ghi psp
	dec psp
	stxd
	glo temp1
	str psp
	sep nextpc

;Z SP!  a-addr --       set data stack pointer
	.dw link
	.db 0
	.set link,*
	.db 3,"SP!"
SPSTORE:
	lda psp		; a lo
	plo temp1
	ldn psp		; a hi
	phi psp
	glo temp1
	plo psp
	sep nextpc

;Z RP@  -- a-addr       get return stack pointer
	.dw link
	.db 0
	.set link,*
	.db 3,"RP@"
RPFETCH:
	ghi rsp
	dec psp
	stxd
	glo rsp
	str psp
	sep nextpc

;Z RP!  a-addr --       set return stack pointer
	.dw link
	.db 0
	.set link,*
	.db 3,"RP!"
RPSTORE:
	lda psp
	plo rsp
	lda psp
	phi rsp
	sep nextpc

; MEMORY AND I/O OPERATIONS =====================

;C !        x a-addr --   store cell in memory
	.dw link
	.db 0
	.set link,*
	.db 1,"!"
STORE:
	lda psp		; a lo
	plo temp1
	lda psp		; a hi
	phi temp1
	lda psp		; x lo
	plo temp2
	lda psp		; x hi
	str temp1
	inc temp1
	glo temp2
	str temp1	; x lo
	sep nextpc

;C C!      char c-addr --    store char in memory
	.dw link
	.db 0
	.set link,*
	.db 2,"C!"
CSTORE:
	lda psp		; a lo
	plo temp1
	lda psp		; a hi
	phi temp1
	lda psp		; x lo
	str temp1
	inc psp		; toss x hi
	sep nextpc

;C @       a-addr -- x   fetch cell from memory
	.dw link
	.db 0
	.set link,*
	.db 1,"@"
FETCH:
	lda psp		; a lo
	plo temp1
	ldn psp		; a hi
	phi temp1
	lda temp1	; x hi
	stxd
	ldn temp1
	str psp		; x lo
	sep nextpc

;C C@     c-addr -- char   fetch char from memory
	.dw link
	.db 0
	.set link,*
	.db 2,"C@"
CFETCH:
	lda psp		; a lo
	plo temp1
	ldn psp		; a hi
	phi temp1
	ldi 0
	stxd		; zero high byte
	ldn temp1	; c lo
	str psp
	sep nextpc

; ARITHMETIC AND LOGICAL OPERATIONS =============

;C +       n1/u1 n2/u2 -- n3/u3     add n1+n2
	.dw link
	.db 0
	.set link,*
	.db 1,"+"
PLUS:
	lda psp		; n2 lo
	inc psp
	add		; n1 lo
	stxd		; n1+n2 lo
	lda psp		; n2 hi
	inc psp
	adc		; n1 hi
	stxd		; n1+n2 hi
	sep nextpc

;D M+       d n -- d         add single to double
	.dw link
	.db 0
	.set link,*
	.db 2,"M+"
MPLUS:
; Double on stack:  byte[1] byte[0] byte[3] byte[2]

	lda psp		; n lo
	plo temp1
	lda psp		; n hi
	phi temp1

	inc psp
	inc psp		; point to d[0]
	glo temp1
	add
	str psp		; update d[0]
	inc psp		; point to d[1]
	ghi temp1
	adc
	stxd		; update d[1]
	dec psp
	dec psp		; point to d[2]
	ghi temp1	; sign of n
	ani H'80
	bnz mp1		; negative ->
	ldi 0		; positive sign extend
	br mp2
mp1:
	ldi H'FF	; negative sign extend
mp2:
	phi temp1
	adc
	str psp		; update d[2]
	inc psp		; point to d[3]
	ghi temp1	; get sign extension
	adc
	stxd		; update d[3]
	sep nextpc

;C -      n1/u1 n2/u2 -- n3/u3    subtract n1-n2
	.dw link
	.db 0
	.set link,*
	.db 1,"-"
MINUS:
	lda psp		; n2 lo
	inc psp
	sd		; n1 lo
	stxd		; n1-n2 lo
	lda psp		; n2 hi
	inc psp
	sdb
	stxd		; n1-n2 hi
	sep nextpc

;C AND    x1 x2 -- x3            logical AND
	.dw link
	.db 0
	.set link,*
	.db 3,"AND"
ANDD:
	lda psp		; n2 lo
	inc psp
	and		; n1 lo
	stxd		; n1 & n2 lo
	lda psp		; n2 hi
	inc psp
	and
	stxd		; n1 & n2 hi
	sep nextpc

;C OR     x1 x2 -- x3           logical OR
	.dw link
	.db 0
	.set link,*
	.db 2,"OR"
ORR:
	lda psp		; n2 lo
	inc psp
	or		; n1 lo
	stxd		; n1 | n2 lo
	lda psp		; n2 hi
	inc psp
	or
	stxd		; n1 | n2 hi
	sep nextpc

;C XOR    x1 x2 -- x3            logical XOR
	.dw link
	.db 0
	.set link,*
	.db 3,"XOR"
XORR:
	lda psp		; n2 lo
	inc psp
	xor		; n1 lo
	stxd		; n1 ^ n2 lo
	lda psp		; n2 hi
	inc psp
	xor
	stxd		; n1 ^ n2 hi
	sep nextpc

;C INVERT   x1 -- x2            bitwise inversion
	.dw link
	.db 0
	.set link,*
	.db 6,"INVERT"
INVERT:
	ldn psp		; x lo
	xri H'FF
	str psp
	inc psp
	ldn psp		; x hi
	xri H'FF
	stxd
	sep nextpc

;C NEGATE   x1 -- x2            two's complement
	.dw link
	.db 0
	.set link,*
	.db 6,"NEGATE"
NEGATE:
	ldn psp		; x1 lo
	sdi H'0
	str psp
	inc psp
	ldn psp		; x1 hi
	sdbi H'0
	stxd
	sep nextpc

;C 1+      n1/u1 -- n2/u2       add 1 to TOS
	.dw link
	.db 0
	.set link,*
	.db 2,"1+"
ONEPLUS:
	ldn psp		; n1 lo
	adi H'1
	str psp
	inc psp
	ldn psp		; n1 hi
	adci H'0
	stxd
	sep nextpc

;C 1-      n1/u1 -- n2/u2     subtract 1 from TOS
	.dw link
	.db 0
	.set link,*
	.db 2,"1-"
ONEMINUS:
	ldn psp		; n1 lo
	smi H'1
	str psp
	inc psp
	ldn psp		; n1 hi
	smbi H'0
	stxd
	sep nextpc

;Z ><      x1 -- x2         swap bytes (not ANSI)
	.dw link
	.db 0
	.set link,*
	.db 2,"><"
swapbytes:
	lda psp
	plo temp1
	ldn psp
	phi temp1
	glo temp1
	stxd
	ghi temp1
	str psp
	sep nextpc

;C 2*      x1 -- x2         arithmetic left shift
	.dw link
	.db 0
	.set link,*
	.db 2,"2*"
TWOSTAR:
	ldn psp		; x lo
	shl		; shift in zero
	str psp
	inc psp
	ldn psp		; x hi
	shlc		; shift in carry
	stxd
	sep nextpc

;C 2/      x1 -- x2        arithmetic right shift
	.dw link
	.db 0
	.set link,*
	.db 2,"2/"
TWOSLASH:		; sign extension
	inc psp
	ldn psp		; x hi
	shlc		; get msb to carry
	ldn psp		; x hi again
	shrc		; shift in carry
	stxd
	ldn psp		; xlo
	shrc
	str psp
	sep nextpc

;C LSHIFT  x1 u -- x2    logical L shift u places
	.dw link
	.db 0
	.set link,*
	.db 6,"LSHIFT"
LSHIFT:
	lda psp		; u lo
	plo temp1
	inc psp		; ignore u hi
lshloop:
	bz shdone

	ldn psp		; lo
	shl		; shift in zero
	str psp
	inc psp
	ldn psp		; hi
	shlc		; shift in carry
	stxd

	dec temp1	; count shifts
	glo temp1
	br lshloop

;C RSHIFT  x1 u -- x2    logical R shift u places
	.dw link
	.db 0
	.set link,*
	.db 6,"RSHIFT"
RSHIFT:
	lda psp		; u lo
	plo temp1
	inc psp		; ignore u hi
rshloop:
	bz shdone

	inc psp
	ldn psp		; hi
	shr		; shift in zero
	stxd
	ldn psp		; lo
	shrc		; shift in carry
	str psp

	dec temp1	; count shifts
	glo temp1
	br rshloop
shdone:
	sep nextpc

;C +!     n/u a-addr --       add cell to memory
	.dw link
	.db 0
	.set link,*
	.db 2,"+!"
PLUSSTORE:
	lda psp		; a lo
	plo temp1
	lda psp		; a hi
	phi temp1	; address
	sex temp1	; do arithmetic in memory

	inc temp1	; low byte
	lda psp		; n lo
	add
	stxd		; low byte
	lda psp		; n hi
	adc
	str temp1	; high byte
	sex psp		; restore
	sep nextpc

; COMPARISON OPERATIONS =========================

.page 	;Page align this section

;C 0=     n/u -- flag    return true if TOS=0
	.dw link
	.db 0
	.set link,*
	.db 2,"0="
ZEROEQUAL:
	lda psp
	bnz xfalse
	ldn psp
	bnz xfalse
xtrue:
	ldi H'FF
	stxd
	str psp
	sep nextpc
xfalse:
	ldi H'0
	stxd
	str psp
	sep nextpc

;C 0<     n -- flag      true if TOS negative
	.dw link
	.db 0
	.set link,*
	.db 2,"0<"
ZEROLESS:
	inc psp
	ldn psp
	shlc		; sign -> carry
	bdf xtrue
	br xfalse

;C =      x1 x2 -- flag         test x1=x2
	.dw link
	.db 0
	.set link,*
	.db 1,"="
EQUAL:
	lda psp		; low byte x2
	inc psp
	sm		; low byte x1
	inc psp
	bnz xfalse
	dec psp
	dec psp
	lda psp		; high byte x2
	inc psp
	sm
	bnz xfalse
	br xtrue

;X <>     x1 x2 -- flag    test not eq
	.dw link
	.db 0
	.set link,*
	.db 2,"<>"
NOTEQUAL:
	sep colonpc
	.dw EQUAL,ZEROEQUAL,EXIT

;C <      n1 n2 -- flag        test n1<n2, signed
	.dw link
	.db 0
	.set link,*
	.db 1,"<"
LESS:
	lda psp		; n2 lo
	plo temp2
	lda psp		; n2 hi
	phi temp2
	inc psp		; point to n1 hi
	xor		; compare sign of n1 and n2
	shl
	bdf less2	; different signs ->
	ghi temp2	; n2
less4:
	sm		; n2 - n1 hi
	bz less3	; same, go check lo
	bdf xtrue
	br xfalse
less3:
	dec psp		; point to n1 lo
	glo temp2
	sm
	inc psp		; point to n1 hi
	bz xfalse
	bdf xtrue
	br xfalse
less2:			; here if signs are different
	ghi temp2	; n2 hi
	shl
	bnf xtrue	; positive->
	br xfalse

;C >     n1 n2 -- flag         test n1>n2, signed
	.dw link
	.db 0
	.set link,*
	.db 1,">"
GREATER:
	sep colonpc
	.dw SWAP,LESS,EXIT

;C U<    u1 u2 -- flag       test u1<u2, unsigned
	.dw link
	.db 0
	.set link,*
	.db 2,"U<"
ULESS:
	lda psp		; u2 lo
	plo temp2
	lda psp		; u2 hi
	phi temp2
	inc psp		; point to u1 hi
	br less4

;X U>    u1 u2 -- flag     u1>u2 unsgd
	.dw link
	.db 0
	.set link,*
	.db 2,"U>"
UGREATER:
	sep colonpc
	.dw SWAP,ULESS,EXIT

; COMMON CONSTANTS =========================

;Z -1	-- -1	
	.dw link
	.db 0
	.set link,*
	.db 2,"-1"
MINUSONE:
	dec psp
	lbr xtrue

;Z 0	-- 0
	.dw link
	.db 0
	.set link,*
	.db 1,"0"
ZERO:
	dec psp
	lbr xfalse

;X FALSE	-- 0	
	.dw link
	.db 0
	.set link,*
	.db 5,"FALSE"
FALSE:
	br ZERO

;X TRUE	-- -1	
	.dw link
	.db 0
	.set link,*
	.db 4,"TRUE"
TRUE:
	br MINUSONE

;Z 1	-- 1	
	.dw link
	.db 0
	.set link,*
	.db 1,"1"
ONE:
	sep constpc
	.dw 1

; LOOP AND BRANCH OPERATIONS ====================

.page ; Page align these instructions

;Z branch   --                  branch always
	.dw link
	.db 0
	.set link,*
	.db 6,"branch"
branch:
	lda ip		; dest hi
	phi temp1
	ldn ip		; dest lo
	plo ip
	ghi temp1
	phi ip
	sep nextpc

;Z ?branch   x --              branch if TOS zero
	.dw link
	.db 0
	.set link,*
	.db 7,"?branch"
qbranch:
	lda psp		; TOS lo
	or		; TOS hi
	inc psp
	bz branch
	inc ip		; skip destination
	inc ip
	sep nextpc

;Z (do)    n1|u1 n2|u2 --  R: -- sys1 sys2
;                           run-time code for DO
; '83 and ANSI standard loops terminate when the
; boundary of limit-1 and limit is crossed, in
; either direction.  The RCA1802 doesn't have signed
; overflow logic. (index-limit) is stored on the return
; stack, and the carry bit is used to detect crossing.
; For (+LOOP) with a negative increment, the logic
; is slightly different.
; The limit itself is also stored for use by I.

	.dw link
	.db 0
	.set link,*
	.db 4,"(do)"
xdo:
	lda psp		; index lo
	plo temp1
	lda psp		; index hi
	phi temp1

	lda psp		; limit lo
	dec rsp		; push to return stack
	str rsp		; for use by I
	ldn psp		; limit hi
	dec rsp
	str rsp

	dec psp		; point to limit lo
	glo temp1	; index lo
	sm		; index - limit lo
	dec rsp		
	str rsp		; push to return stack

	inc psp		; point to limit hi
	ghi temp1
	smb		; index - limit hi
	dec rsp		
	str rsp		; push to return stack

	inc psp
	sep nextpc

;Z (loop)   R: sys1 sys2 --  | sys1 sys2
;                         run-time code for LOOP
; Add 1 to the loop index.  If loop terminates,
; clean up the return stack and skip the branch.
; Else take the inline branch.  Note that LOOP
; terminates when index=0.
	.dw link
	.db 0
	.set link,*
	.db 6,"(loop)"
xloop:
	sex rsp		; do arithmetic on return stack
	inc rsp		; low byte of index
	ldi 1
	add		; increment
	stxd
	ldi 0
	adc		; high byte
	str rsp
	sex psp		; restore X
	bnf branch	; no carry, continue loop
	br loopdone

;Z (+loop)   n --   R: sys1 sys2 --  | sys1 sys2
;                        run-time code for +LOOP
; Add n to the loop index.  If loop terminates,
; clean up the return stack and skip the branch.
; Else take the inline branch.
	.dw link
	.db 0
	.set link,*
	.db 7,"(+loop)"
xplusloop:
	lda psp		; increment lo
	plo temp1
	lda psp		; increment hi
	phi temp1
	sex rsp		; do arithmetic on return stack
	inc rsp		; lo byte of index'
	glo temp1
	add
	stxd		; update low byte
	ghi temp1
	adc
	str rsp		; update high byte
	sex psp		; restore X
		
	ghi temp1	; 
	ani H'80	; sign of increment
	bz xloopup	; positive -> 
			; counting down
	bdf branch	; continue looping
	br loopdone

xloopup:		; counting up
	bnf branch	; continue looping

loopdone:
	inc ip		; ignore branch destination
	inc ip
	br unloop

;C I        -- n   R: sys1 sys2 -- sys1 sys2
;                   get the innermost loop index
	.dw link
	.db 0
	.set link,*
	.db 1,"I"
II:
	lda rsp		; index hi
	dec psp		; push to param stack
	stxd
	lda rsp		; index lo
	stxd
	lda rsp		; limit hi
	stxd
	ldn rsp		; limit lo
	str psp

	dec rsp		; restore return stack
	dec rsp
	dec rsp		
	lbr PLUS	; add limit back to index

;C J        -- n   R: 4*sys -- 4*sys
;                   get the second loop index
	.dw link
	.db 0
	.set link,*
	.db 1,"J"
JJ:
	inc rsp		; skip outer loop params
	inc rsp
	inc rsp
	inc rsp

	lda rsp		; index hi
	dec psp		; push to param stack
	stxd
	lda rsp		; index lo
	stxd
	lda rsp		; limit hi
	stxd
	ldn rsp		; limit lo
	str psp

	dec rsp		; restore return stack
	dec rsp
	dec rsp

	dec rsp
	dec rsp	
	dec rsp
	dec rsp	
	lbr PLUS	; add limit back to index

;C UNLOOP   --   R: sys1 sys2 --  drop loop parms
	.dw link
	.db 0
	.set link,*
	.db 6,"UNLOOP"
UNLOOP:
	inc rsp		; drop loop params
	inc rsp		; from return stack
	inc rsp
	inc rsp
	sep nextpc

; MULTIPLY AND DIVIDE ===========================

.page ;Purify this code, oh holy page

;C UM*     u1 u2 -- ud   unsigned 16x16->32 mult.
	.dw link
	.db 0
	.set link,*
	.db 3,"UM*"
UMSTAR:

	lda psp		; u2 lo
	plo temp2
	lda psp		; u2 hi
	phi temp2
	lda psp		; u1 lo
	plo temp1
	ldn psp		; u1 hi
	phi temp1

	ldi 0
	stxd
	stxd
	stxd
	str psp		; clear double result

	plo temp3
	phi temp3	; extend multiplier

; Result on stack:  byte[1] byte[0] byte[3] byte[2]

	ldi 16
	plo temp4	; bit counter
	inc psp
	inc psp
umloop:			; PSP points to byte[0] of result
			
	ghi temp1	; shift u1 right
	shr
	phi temp1
	glo temp1
	shrc
	plo temp1
	bnf um_noadd	; if LSB was 1, add in multiplier

	glo temp2	; byte[0]
	add
	str psp
	inc psp
	ghi temp2	; byte[1]
	adc
	stxd
	dec psp
	dec psp
	glo temp3	; byte[2]
	adc
	str psp
	inc psp
	ghi temp3	; byte[3]
	adc
	str psp		; restore PSP
	inc psp

um_noadd:		; shift multiplier left
	glo temp2
	shl
	plo temp2
	ghi temp2
	shlc
	phi temp2
	glo temp3
	shlc
	plo temp3
	ghi temp3
	shlc
	phi temp3

	dec temp4	; count bits
	glo temp4
	bnz umloop

	dec psp
	dec psp		; point to byte[2]

	sep nextpc

;C UM/MOD   ud u1 -- u2 u3   unsigned 32/16->16
	.dw link
	.db 0
	.set link,*
	.db 6,"UM/MOD"
UMSLASHMOD:
	lda psp		; get divisor u1
	plo temp1
	lda psp
	phi temp1

	ldi 0		; extend divisor to 32 bits
	plo temp2
	phi temp2

	plo temp3	; initialize quotient
	phi temp3

; Dividend on stack:  byte[1] byte[0] byte[3] byte[2]

	ldi 16
	plo temp4	; bit counter
	inc psp

ummodloop:		; PSP points to byte[3] of dividend
			; shift divisor right
	ghi temp1
	shr
	phi temp1
	glo temp1
	shrc
	plo temp1
	ghi temp2
	shrc
	phi temp2
	glo temp2
	shrc
	plo temp2

	ghi temp1	; MSB of divisor
	sd		; dividend - divisor
	bnf umm3	; doesn't go ->
	bnz umd3	; goes ->

	dec psp		; byte[2]
	glo temp1
	sd

	inc psp		; byte[3]
	bnf umm3	; doesn't go ->
	bnz umd3	; goes ->

	inc psp
	inc psp		; byte[1]
	ghi temp2
	sd

	dec psp		; byte[0]
	bnf umm0	; doesn't go ->
	bnz umd0	; goes ->

	glo temp2
	sd
	bnf umm0	; doesn't go ->
	br umd0		; goes ->
umd3:
	inc psp
umd0:

; subtract divisor from dividend
; PSP pointing to byte[0] of dividend
	glo temp2
	sd
	str psp
	inc psp		; byte[1]
	ghi temp2
	sdb
	stxd
	dec psp
	dec psp		; byte[2]
	glo temp1
	sdb
	str psp
	inc psp		; byte[3]
	ghi temp1
	sdb
	str psp
	smi 0		; set carry
	br umm3

umm0:	dec psp

umm3:			; PSP pointing to byte[3] of dividend
			; shift carry into quotient
	glo temp3
	shlc
	plo temp3
	ghi temp3
	shlc
	phi temp3

	dec temp4	; count bits
	glo temp4
	bnz ummodloop

; remainder is byte[0] and byte[1] of the dividend

	ghi temp3	; get msb of quotient
	stxd
	glo temp3	; get lsb of quotient
	str psp
	sep nextpc

; BLOCK AND STRING OPERATIONS ===================

;C FILL   c-addr u char --  fill memory with char
	.dw link
	.db 0
	.set link,*
	.db 4,"FILL"
FILL:
	lda psp
	plo temp1	; char
	inc psp
	lda psp
	plo temp2	; count lo
	lda psp
	phi temp2	; count hi
	lda psp
	plo temp3	; dest lo
	lda psp
	phi temp3	; dest hi

fillloop:
	glo temp2	; check for zero
	bnz fillmore
	ghi temp2
	bz filldone	; done->
fillmore:
	glo temp1
	str temp3	; dst byte
	inc temp3
	dec temp2	; count bytes
	br fillloop
filldone:
	sep nextpc

;S CMOVE   c-addr1 c-addr2 u --  move from bottom
; as defined in the ANSI optional String word set
; On byte machines, CMOVE and CMOVE> are logical
; factors of MOVE.  They are easy to implement on
; CPUs which have a block-move instruction.
	.dw link
	.db 0
	.set link,*
	.db 5,"CMOVE"
CMOVE:
	lda psp
	plo temp1	; count lo
	lda psp
	phi temp1	; count hi
	lda psp
	plo temp2	; dest lo
	lda psp
	phi temp2	; dest hi
	lda psp
	plo temp3	; src lo
	lda psp
	phi temp3	; src hi
cmoveloop:
	glo temp1	; check for zero
	bnz cmovemore
	ghi temp1
	bz cmovedone	; done->
cmovemore:
	lda temp3	; src byte
	str temp2	; dest
	inc temp2
	dec temp1	; count bytes
	br cmoveloop
cmovedone:
	sep nextpc

;S CMOVE>  c-addr1 c-addr2 u --  move from top
; as defined in the ANSI optional String word set
	.dw link
	.db 0
	.set link,*
	.db 6,"CMOVE>"
CMOVEUP:
	sep colonpc
	.dw TOR			; count to return stack
	.dw RFETCH,PLUS		; end of dest + 1
	.dw SWAP,RFETCH,PLUS	; end of src + 1
	.dw RFROM		; count
	.dw *+2

	lda psp
	plo temp1	; count lo
	lda psp
	phi temp1	; count hi
	lda psp
	plo temp2	; src lo
	lda psp
	phi temp2	; src hi
	dec temp2	; end of src

	lda psp
	plo temp3	; dst lo
	lda psp
	phi temp3	; dst hi
	dec temp3	; end of dst
	sex temp3	; so we can use stxd

xcmoveloop:
	glo temp1	; check for zero
	bnz xcmovemore
	ghi temp1
	bz xcmovedone	; done->
xcmovemore:
	ldn temp2	; src byte
	dec temp2
	stxd		; dest
	dec temp1	; count bytes
	br xcmoveloop
xcmovedone:
	sex psp		; restore X
	lbr EXIT

;Z SKIP   c-addr u c -- c-addr' u'
;                           skip matching chars
; Although SKIP, SCAN, and S= are perhaps not the
; ideal factors of WORD and FIND, they closely
; follow the string operations available on many
; CPUs, and so are easy to implement and fast.
	.dw link
	.db 0
	.set link,*
	.db 4,"SKIP"
skip:
	lda psp		; char lo
	plo temp1
	inc psp
	lda psp		; count lo
	plo temp2
	lda psp		; count hi
	phi temp2
	lda psp		; addr lo
	plo temp3
	ldn psp		; addr hi
	phi temp3
	sex temp3	; for comparisons

skloop:			; is count zero?
	glo temp2
	bnz sk1
	ghi temp2
	bz skdone
sk1:
	glo temp1	; get char
	sm
	bnz skdone	; not equal ->
	inc temp3	; increment address
	dec temp2	; decrement count
	br skloop
skdone:
	sex psp		; restore X
	ghi temp3	; push pointer
	stxd
	glo temp3
	stxd
	ghi temp2	; push remaining count
	stxd
	glo temp2
	str psp
	sep nextpc

;Z SCAN    c-addr u c -- c-addr' u'
;                       find matching char
	.dw link
	.db 0
	.set link,*
	.db 4,"SCAN"
scan:
	lda psp		; char lo
	plo temp1
	inc psp
	lda psp		; count lo
	plo temp2
	lda psp		; count hi
	phi temp2
	lda psp		; addr lo
	plo temp3
	ldn psp		; addr hi
	phi temp3
	sex temp3	; for comparisons

scloop:			; is count zero?
	glo temp2
	bnz sc1
	ghi temp2
	bz skdone
sc1:
	glo temp1	; get char
	sm
	bz skdone	; equal ->
	inc temp3	; increment address
	dec temp2	; decrement count
	br scloop

;Z S=    c-addr1 c-addr2 u -- n   string compare
;              n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
	.dw link
	.db 0
	.set link,*
	.db 2,"S="
sequal:
	lda psp		; count lo
	plo temp3
	lda psp		; count hi
	phi temp3
	lda psp		; addr2 lo
	plo temp2
	lda psp		; addr2 hi
	phi temp2
	lda psp		; addr1 lo
	plo temp1
	ldn psp		; addr1 hi
	phi temp1
	sex temp2	; for comparisons

seqloop:
	glo temp3	; is count zero?
	bnz seq1
	ghi temp3
	bz seqdone
seq1:
	lda temp1
	sm		; subtract (addr1) - (addr2)
	bnz seqdone	; not equal ->
	inc temp2
	dec temp3
	br seqloop

seqdone:
	sex psp		; restore X
	stxd		; push result twice
	str psp
	sep nextpc

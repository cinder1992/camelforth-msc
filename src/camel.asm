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
; 2024-07-22	Port to the modern A18 cross-assembler
;
;
; ===============================================
; CAMEL.ASM: assembly header and defines
;   Source code is for the A18 assembler.
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
; Build variables and flags:
reset	EQU	$0000		; cold start, Forth kernel, dictionary
MEMTOP EQU $8000		; Top of memory
INRAM	EQU 1			; is the main dictionary in RAM?
VT		EQU 1			; ANSI term or VT term?
FC		EQU 0			; XON/XOFF Flow control?

leavestack	EQU	MEMTOP - $100		; top of leave stack		grows up
padarea		EQU	leavestack			; User Pad Buffer		grows down
tibend		EQU	padarea - $100		; end+1 of Terminal Buffer
tibarea		EQU	tibend - $100		; Terminal Input Buffer		grows up
userarea	EQU	tibarea - $100		; user area, page aligned	grows up
returnstack	EQU	userarea			; top of return stack		grows down
paramstack	EQU	returnstack - $100	; top of parameter stack	grows down

; ===============================================
; Register usage
; TODO: Interrupt handler, DMA handler, etc.
codepc	EQU 0	; PC for code words
ip		EQU 1	; Forth interpreter pointer
psp		EQU 2	; Forth parameter stack pointer
rsp		EQU 3	; Forth return stack pointer
nextpc	EQU 4	; PC for Forth inner interpreter
colonpc	EQU 5	; PC for colon definitions
constpc	EQU 6	; PC for CONSTANT definitions
varpc	EQU 7	; PC for VARIABLE and CREATE1 definitions
createpc EQU 8	; PC for CREATE definitions
userpc 	EQU 9	; PC for USER definitions
baudr	EQU 10	; baud rate register, set by GENBAUD

temppc	EQU 15	; temporary registers
temp1	EQU 15
temp2	EQU 14
temp3	EQU 13
temp4	EQU 12

; ===============================================
; Execution begins here
	CPU	1802
	ORG reset 			; cold start address
						; initialize registers
	LOAD psp, paramstack
	LOAD rsp, returnstack
	LOAD nextpc, nextd
	LOAD colonpc, docolon
	LOAD constpc, doconst
	LOAD varpc, dovar
	LOAD createpc, docreate
	LOAD userpc, douser

	sex psp		; do arithmetic on param stack
	lbr TRUECOLD

	INCL	"camel18.asm"
	INCL	"camel18d.asm"
	INCL	"camel18r.asm"
	INCL	"camel18h.asm"
	INCL	"camel18x.asm"

; EPILOGUE =========================

lastword	EQU		link	; nfa of last word in dictionary
	IF INRAM NE 0
enddict	EQU		$
	ELSE
enddict	EQU		reset + 3 ; 3 bytes saved for the reset vector.
	ENDI
	END

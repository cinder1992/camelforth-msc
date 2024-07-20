; LISTING 3.
;
; ===============================================
; CamelForth for the RCA 1802
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; Copyright (c) 2009 Harold Rabbie
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

; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
; ===============================================
; CAMEL18D.ASM: CPU and Model Dependencies
;   Source code is for the A180 assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
;
; Direct-Threaded Forth model for RCA 1802
;   cell size is   16 bits (2 bytes)
;   char size is    8 bits (1 byte)
;   address unit is 8 bits (1 byte), i.e.,
;       addresses are byte-aligned.
; ===============================================

; ALIGNMENT AND PORTABILITY OPERATORS ===========
; Many of these are synonyms for other words,
; and so are defined as CODE words.

;C ALIGN    --                         align HERE
	.dw link
	.db 0
	.set link,*
	.db 5,"ALIGN"
ALIGN:
noop:   sep nextpc

;C ALIGNED  addr -- a-addr       align given addr
	.dw link
	.db 0
	.set link,*
	.db 7,"ALIGNED"
ALIGNED:
	sep nextpc	; noop

;Z CELL     -- n                 size of one cell
	.dw link
	.db 0
	.set link,*
	.db 4,"CELL"
CELL:
	sep constpc
	.dw 2

;C CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
	.dw link
	.db 0
	.set link,*
	.db 5,"CELL+"
CELLPLUS:
	sep colonpc
	.dw CELL,PLUS,EXIT

;C CELLS    n1 -- n2            cells->adrs units
	.dw link
	.db 0
	.set link,*
	.db 5,"CELLS"
CELLS:
	lbr twostar

;C CHAR+    c-addr1 -- c-addr2   add char size
	.dw link
	.db 0
	.set link,*
	.db 5,"CHAR+"
CHARPLUS:
	lbr oneplus

;C CHARS    n1 -- n2            chars->adrs units
	.dw link
	.db 0
	.set link,*
	.db 5,"CHARS"
CHARS:
	sep nextpc	; noop

;C >BODY    xt -- a-addr      adrs of param field
;   3 + ;                     1802 (3 byte code field for CREATE)
; ANSI 6.1.0550 says that >BODY only applies to CREATE'd words
	.dw link
	.db 0
	.set link,*
	.db 5,">BODY"
TOBODY:
	sep colonpc
    	.dw LIT,3,PLUS,EXIT

;X COMPILE,  xt --         append execution token
; I called this word ,XT before I discovered that
; it is defined in the ANSI standard as COMPILE,.
; On a DTC Forth this simply appends xt (like , )
; but on an STC Forth this must append 'CALL xt'.
	.dw link
	.db 0
	.set link,*
	.db 8,"COMPILE,"
COMMAXT:
	lbr COMMA

;Z ,EXIT    --      append hi-level EXIT action
;   COMPILE EXIT ;
; This is made a distinct word, because on an STC
; Forth, it appends a RET instruction, not an xt.
	.dw link
	.db 0
	.set link,*
	.db 5,",EXIT"
CEXIT:
	sep colonpc
	.dw COMPILE,EXIT,EXIT

; CONTROL STRUCTURES ============================
; These words allow Forth control structure words
; to be defined portably.

;Z ,BRANCH   xt --    append a branch instruction
; xt is the branch operator to use, e.g. qbranch
; or (loop).  It does NOT append the destination
; address.  On the RCA1802 this is equivalent to ,XT.
	.dw link
	.db 0
	.set link,*
	.db 7,",BRANCH"
COMMABRANCH:
	lbr COMMA

;Z ,DEST   dest --        append a branch address
; This appends the given destination address to
; the branch instruction.  On the RCA1802 this is ','
; ...other CPUs may use relative addressing.
	.dw link
	.db 0
	.set link,*
	.db 5,",DEST"
COMMADEST:
	lbr COMMA

;Z !DEST   dest adrs --    change a branch dest'n
; Changes the destination address found at 'adrs'
; to the given 'dest'.  On the Z80 this is '!'
; ...other CPUs may need relative addressing.
	.dw link
	.db 0
	.set link,*
	.db 5,"!DEST"
STOREDEST:
	lbr STORE

; HEADER STRUCTURE ==============================
; The structure of the Forth dictionary headers
; (name, link, immediate flag, and "smudge" bit)
; does not necessarily differ across CPUs.  This
; structure is not easily factored into distinct
; "portable" words; instead, it is implicit in
; the definitions of FIND and CREATE, and also in
; NFA>LFA, NFA>CFA, IMMED?, IMMEDIATE, HIDE, and
; REVEAL.  These words must be (substantially)
; rewritten if either the header structure or its
; inherent assumptions are changed.




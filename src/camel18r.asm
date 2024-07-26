; ===============================================
; CamelForth for the RCA 1802
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; Copyright (c) 2009 Harold Rabbie
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

; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
; ===============================================
; CAMEL18R.ASM: 1802 CPU Extentions
;   Source code is for the A18 assembler.
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
;Z ef1?		--	c	get EF1 status
;
	DW link
	DB 0
link SET $
	DB 4,"ef1?"
EF1Q
	b1 etrue
efalse
	dec psp
	ldi $0
	stxd
	str psp
	sep nextpc
etrue
	dec psp
	ldi $FF
	stxd
	str psp
	sep nextpc

;Z ef2?		--	c	get EF2 status
;
	DW link
	DB 0
link SET $
	DB 4,"ef2?"
EF2Q
	b2 etrue
	br efalse

;Z ef3?		--	c	get EF3 status
;
	DW link
	DB 0
link SET $
	DB 4,"ef3?"
EF3Q
	b3 etrue
	br efalse

;Z ef4?		--	c	get EF4 status
;
	DW link
	DB 0
link SET $
	DB 4,"ef4?"
EF4Q
	b4 etrue
	br efalse

;Z outp		c p	--		Output char c on port p
;
	DW link
	DB 0
link SET $
	DB 4,"outp"
OUTP
	lda psp		;port low byte
	inc psp		;drop high byte
	smi 1		;subtract 1 for correct offset
	ani 7		;AND with 7 to set bounds
	shl
	shl			;shift left by 2 places
	adi	doout.0 ;get low portion of offset
	plo temppc
	ldi 0
	adci doout.1 ;get high portion of offset
	phi temppc
	sep temppc		;Jump to appropriate offset!
doout
	out 1
	inc psp			;drop the high byte
	sep nextpc
	nop				;add 1 byte of padding to the table
	out 2			;making the table 4 bytes in entry size
	inc psp
	sep nextpc
	nop
	out 3
	inc psp
	sep nextpc
	nop
	out 4
	inc psp
	sep nextpc
	nop
	out 5
	inc psp
	sep nextpc
	nop
	out 6
	inc psp
	sep nextpc
	nop
	out 7
	inc psp
	sep nextpc
	nop
endout		;safety cusion if `8 OUT` is executed
	inc psp	;drop the high byte
	sep nextpc

;Z inp		p	--	c	get char c from port p
;
	DW link
	DB 0
link SET $
	DB 3,"inp"
INP
	ldn psp		;port low byte
	smi 1		;subtract 1 for correct offset
	ani 7		;AND with 7 to set bounds
	shl			;shift left by 1 places		
	adi	doin.0 ;get low portion of offset
	plo temppc
	ldi 0
	adci doin.1 ;get high portion of offset
	phi temppc
	sep temppc		;Jump to appropriate offset!
doin
	inp 1
	sep nextpc	;2 bytes per table entry!
	inp 2
	sep nextpc
	inp 3
	sep nextpc
	inp 4
	sep nextpc
	inp 5
	sep nextpc
	inp 6
	sep nextpc
	inp 7
	sep nextpc
endin			;Safety cusion if `8 INP` is executed
	sep nextpc

	DW link
	DB 1
link SET $
	DB 7,"[ansi?]"
ansiq
	sep constpc
	IF ANSI NE 0
	DW $FFFF
	ELSE
	DW 0
	ENDI

	DW link
	DB 1
link SET $
	DB 5,"[fc?]"
fcq
	sep constpc
	IF FC NE 0
	DW $FFFF
	ELSE
	DW 0
	ENDI
	
	DW link
	DB 0
link SET $
	DB 4,"xoff"
xoff
	sep constpc
	DW $13

	DW link
	DB 0
link SET $
	DB 3,"xon"
xon
	sep constpc
	DW $11

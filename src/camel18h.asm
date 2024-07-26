; LISTING 2.
;
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
; CAMEL18H.ASM: High Level Words
;   Source code is for the A18 assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================

; SYSTEM VARIABLES & CONSTANTS ==================

;C BL      -- char	an ASCII space
	DW link
	DB 0
link SET $
	DB 2,"BL"
BL
	sep constpc
	DW $20

;X #TIB  -- n	 size of TIB
	DW link
	DB 0
link SET $
	DB 4,"#TIB"
TIBSIZE
	sep constpc
	DW tibend - tibarea

;X TIB     -- a-addr	Terminal Input Buffer
	DW link
	DB 0
link SET $
	DB 3,"TIB"
TIB
	sep constpc
	DW tibarea

;Z u0      -- a-addr	current user area adrs
;  0 USER U0
	DW link
	DB 0
link SET $
	DB 2,"U0"
U0
	sep userpc
	DW 0

;C >IN     -- a-addr	holds offset into TIB
;  2 USER >IN
	DW link
	DB 0
link SET $
	DB 3,">IN"
TOIN
	sep userpc
	DW 2

;C BASE    -- a-addr	holds conversion radix
;  4 USER BASE
	DW link
	DB 0
link SET $
	DB 4,"BASE"
BASE
	sep userpc
	DW 4

;C STATE   -- a-addr	holds compiler state
;  6 USER STATE
	DW link
	DB 0
link SET $
	DB 5,"STATE"
STATE
	sep userpc
	DW 6

;Z DP      -- a-addr	holds dictionary ptr
;  8 USER DP
	DW link
	DB 0
link SET $
	DB 2,"DP"
DP
	sep userpc
	DW 8

;Z 'SOURCE  -- a-addr	two cells: len, adrs
; 10 USER 'SOURCE
	DW link
	DB 0
link SET $
	DB 7,"\'SOURCE"
TICKSOURCE
	sep userpc
	DW 10

;Z latest    -- a-addr	last word in dict.
;   14 USER LATEST
	DW link
	DB 0
link SET $
	DB 6,"LATEST"
LATEST
	sep userpc
	DW 14

;Z HP       -- a-addr	HOLD pointer
;   16 USER HP
	DW link
	DB 0
link SET $
	DB 2,"HP"
HP
	sep userpc
	DW 16

;Z LP       -- a-addr	Leave-stack pointer
;   18 USER LP
	DW link
	DB 0
link SET $
	DB 2,"LP"
LP
	sep userpc
	DW 18

;Z MAGIC       -- a-addr magic num used by TRUECOLD to determine start
;   20 USER MAGIC
	DW link
	DB 0
link SET $
	DB 5,"magic"
MAGIC
	sep userpc
	DW 20

;Z S0       -- a-addr	end of parameter stack
	DW link
	DB 0
link SET $
	DB 2,"S0"
S0
	sep constpc
	DW paramstack

;X PAD       -- a-addr	user PAD buffer
;			= end of hold area!
	DW link
	DB 0
link SET $
	DB 3,"PAD"
PAD
	sep constpc
	DW padarea

;Z L0       -- a-addr	bottom of Leave stack
	DW link
	DB 0
link SET $
	DB 2,"L0"
L0
	sep constpc
	DW leavestack

;Z R0       -- a-addr	end of return stack
	DW link
	DB 0
link SET $
	DB 2,"R0"
R0
	sep constpc
	DW returnstack

;Z UINIT    -- addr	initial values for user area
	DW link
	DB 0
link SET $
	DB 5,"UINIT"
UINIT
	sep varpc
	DW 0,0,10,0	; reserved,>IN,BASE,STATE
	DW enddict	; DP
	DW 0,0		; SOURCE init'd elsewhere
	DW lastword	; LATEST
	DW 0		; HP init'd elsewhere
	DW 0		; LP init'd elsewhere
	DW 28912	; Magic word

;Z #INIT    -- n	#bytes of user area init data
	DW link
	DB 0
link SET $
	DB 5,"#INIT"
NINIT
	sep constpc
	DW 22

; ARITHMETIC OPERATORS ==========================

;C S>D    n -- d	single -> double prec.
;   DUP 0< ;
	DW link
	DB 0
link SET $
	DB 3,"S>D"
STOD
	inc psp
	ldn psp		; n hi
	dec psp
	shlc		; sign to carry
	lbdf MINUSONE
	lbr ZERO

;Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;	...a common factor
	DW link
	DB 0
link SET $
	DB 7,"?NEGATE"
QNEGATE
	inc psp
	lda psp		; n2 hi
	shlc		; sign to carry
	lbdf NEGATE
	sep nextpc

;C ABS     n1 -- +n2	absolute value
;   DUP ?NEGATE ;
	DW link
	DB 0
link SET $
	DB 3,"ABS"
ABS
	inc psp
	ldn psp		; n1 hi
	dec psp
	shlc		; sign to carry
	lbdf NEGATE
	sep nextpc

;D DNEGATE   d1 -- d2	negate double precision
;   SWAP INVERT SWAP INVERT 1 M+ ;
	DW link
	DB 0
link SET $
	DB 7,"DNEGATE"
DNEGATE
	sep colonpc
	DW SWAP,INVERT,SWAP,INVERT,ONE,MPLUS
	DW EXIT

;Z ?DNEGATE  d1 n -- d2	negate d1 if n negative
;   0< IF DNEGATE THEN ;       ...a common factor
	DW link
	DB 0
link SET $
	DB 8,"?DNEGATE"
QDNEGATE
	sep colonpc
	DW ZEROLESS,qbranch,DNEG1,DNEGATE
DNEG1  DW EXIT

;D DABS     d1 -- +d2	absolute value dbl.prec.
;   DUP ?DNEGATE ;
	DW link
	DB 0
link SET $
	DB 4,"DABS"
DABS
	sep colonpc
	DW DUP,QDNEGATE,EXIT

;C M*     n1 n2 -- d	signed 16*16->32 multiply
;   2DUP XOR >R	carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE ;
	DW link
	DB 0
link SET $
	DB 2,"M*"
MSTAR
	sep colonpc
	DW TWODUP,XORR,TOR
	DW SWAP,ABS,SWAP,ABS,UMSTAR
	DW RFROM,QDNEGATE,EXIT

;C SM/REM   d1 n1 -- n2 n3	symmetric signed div
;   2DUP XOR >R			sign of quotient
;   OVER >R			sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;
; Ref. dpANS-6 section 3.2.2.1.
	DW link
	DB 0
link SET $
	DB 6,"SM/REM"
SMSLASHREM
	sep colonpc
	DW TWODUP,XORR,TOR,OVER,TOR
	DW ABS,TOR,DABS,RFROM,UMSLASHMOD
	DW SWAP,RFROM,QNEGATE,SWAP,RFROM,QNEGATE
	DW EXIT

;C FM/MOD   d1 n1 -- n2 n3	floored signed div'n
;   DUP >R	      divisor 
;   2DUP XOR >R	 sign of quotient 
;   >R		  divisor 
;   DABS R@ ABS UM/MOD 
;   SWAP R> ?NEGATE SWAP	apply sign to remainder 
;   R> 0< IF			if quotient negative, 
;       NEGATE 
;       OVER IF			if remainder nonzero, 
;	R@ ROT - SWAP 1-	adjust rem,quot 
;       THEN 
;   THEN  R> DROP ; 
; Ref. dpANS-6 section 3.2.2.1.
	DW link
	DB 0
link SET $
	DB 6,"FM/MOD"
FMSLASHMOD
	sep colonpc 
	DW DUP,TOR 
	DW TWODUP,XORR,TOR 
	DW TOR 
	DW DABS,RFETCH,ABS,UMSLASHMOD 
	DW SWAP,RFROM,QNEGATE,SWAP 
	DW RFROM,ZEROLESS,qbranch,FMMOD1 
	DW NEGATE 
	DW OVER,qbranch,FMMOD1 
	DW RFETCH,ROT,MINUS,SWAP,ONEMINUS 
FMMOD1
	DW RFROM,DROP,EXIT 

;C *      n1 n2 -- n3		signed multiply
;   M* DROP ;
	DW link
	DB 0
link SET $
	DB 1,"*"
STAR
	sep colonpc
	DW MSTAR,DROP,EXIT

;C /MOD   n1 n2 -- n3 n4	signed divide/rem'dr
;   >R S>D R> FM/MOD ;
	DW link
	DB 0
link SET $
	DB 4,"/MOD"
SLASHMOD
	sep colonpc
	DW TOR,STOD,RFROM,FMSLASHMOD,EXIT

;C /      n1 n2 -- n3		signed divide
;   /MOD nip ;
	DW link
	DB 0
link SET $
	DB 1,"/"
SLASH
	sep colonpc
	DW SLASHMOD,NIP,EXIT

;C MOD    n1 n2 -- n3		signed remainder
;   /MOD DROP ;
	DW link
	DB 0
link SET $
	DB 3,"MOD"
FMOD
	sep colonpc
	DW SLASHMOD,DROP,EXIT

;C */MOD  n1 n2 n3 -- n4 n5	n1*n2/n3, rem&quot
;   >R M* R> FM/MOD ;
	DW link
	DB 0
link SET $
	DB 5,"*/MOD"
SSMOD
	sep colonpc
	DW TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

;C */     n1 n2 n3 -- n4	n1*n2/n3
;   */MOD nip ;
	DW link
	DB 0
link SET $
	DB 2,"*/"
STARSLASH
	sep colonpc
	DW SSMOD,NIP,EXIT

;C MAX    n1 n2 -- n3		signed maximum
;   2DUP < IF SWAP THEN DROP ;
	DW link
	DB 0
link SET $
	DB 3,"MAX"
MAX
	sep colonpc
	DW TWODUP,LESS,qbranch,MAX1,SWAP
MAX1   DW DROP,EXIT

;C MIN    n1 n2 -- n3		signed minimum
;   2DUP > IF SWAP THEN DROP ;
	DW link
	DB 0
link SET $
	DB 3,"MIN"
MIN
	sep colonpc
	DW TWODUP,GREATER,qbranch,MIN1,SWAP
MIN1   DW DROP,EXIT

; DOUBLE OPERATORS ==============================

;C 2@    a-addr -- x1 x2	fetch 2 cells
;   DUP CELL+ @ SWAP @ ;
;   the lower address will appear on top of stack
	DW link
	DB 0
link SET $
	DB 2,"2@"
TWOFETCH
	sep colonpc
	DW DUP,CELLPLUS,FETCH,SWAP,FETCH,EXIT

;C 2!    x1 x2 a-addr --	store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
	DW link
	DB 0
link SET $
	DB 2,"2!"
TWOSTORE
	sep colonpc
	DW SWAP,OVER,STORE,CELLPLUS,STORE,EXIT

;C 2DROP  x1 x2 --		drop 2 cells
;   DROP DROP ;
	DW link
	DB 0
link SET $
	DB 5,"2DROP"
TWODROP
	sep colonpc
	DW DROP,DROP,EXIT

;C 2DUP   x1 x2 -- x1 x2 x1 x2	dup top 2 cells
;   OVER OVER ;
	DW link
	DB 0
link SET $
	DB 4,"2DUP"
TWODUP
	lda psp		; x2 lo
	plo temp2
	lda psp		; x2 hi
	phi temp2
	lda psp		; x1 lo
	plo temp1
	ldn psp		; x1 hi
	dec psp
	dec psp
	dec psp
	dec psp
	stxd
	glo temp1
	stxd
	ghi temp2
	stxd
	glo temp2
	str psp
	sep nextpc

;C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
;   ROT >R ROT R> ;
	DW link
	DB 0
link SET $
	DB 5,"2SWAP"
TWOSWAP
	sep colonpc
	DW ROT,TOR,ROT,RFROM,EXIT

;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
;   >R >R 2DUP R> R> 2SWAP ;
	DW link
	DB 0
link SET $
	DB 5,"2OVER"
TWOOVER
	sep colonpc
	DW TOR,TOR,TWODUP,RFROM,RFROM
	DW TWOSWAP,EXIT

; INPUT/OUTPUT ==================================

;C COUNT   c-addr1 -- c-addr2 u	counted->adr/len
;   DUP CHAR+ SWAP C@ ;
	DW link
	DB 0
link SET $
	DB 5,"COUNT"
COUNT
	sep colonpc
	DW DUP,CHARPLUS,SWAP,CFETCH,EXIT

;C CR      --			output newline
;   0D EMIT 0A EMIT ;
	DW link
	DB 0
link SET $
	DB 2,"CR"
CR
	sep colonpc
	DW LIT,$0D,EMIT,LIT,$0A,EMIT,EXIT

;C SPACE   --			output a space
;   BL EMIT ;
	DW link
	DB 0
link SET $
	DB 5,"SPACE"
SPACE
	sep colonpc
	DW BL,EMIT,EXIT

;C SPACES   n --		output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
	DW link
	DB 0
link SET $
	DB 6,"SPACES"
SPACES
	sep colonpc
SPCS1  DW DUP,qbranch,SPCS2
	DW SPACE,ONEMINUS,branch,SPCS1
SPCS2  DW DROP,EXIT

;Z UMIN     u1 u2 -- u		unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
	DW link
	DB 0
link SET $
	DB 4,"UMIN"
UMIN
	sep colonpc
	DW TWODUP,UGREATER,qbranch,UMIN1,SWAP
UMIN1  DW DROP,EXIT

;Z UMAX    u1 u2 -- u		unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
	DW link
	DB 0
link SET $
	DB 4,"UMAX"
UMAX
	sep colonpc
	DW TWODUP,ULESS,qbranch,UMAX1,SWAP
UMAX1  DW DROP,EXIT

;C ACCEPT  c-addr +n -- +n'	get line from term'l
;   OVER + 1- OVER		-- sa ea a
;	[fc?] [IF] xon EMIT [then]
;   BEGIN KEY			-- sa ea a c
;   DUP 0D <> WHILE
;       DUP				-- sa ea a c
;		[ansi?] [IF]
;			8 = IF
;		[ELSE] 
;			127 = IF DROP 8 DUP
;		[THEN]
;			EMIT 32 EMIT EMIT 1- >R OVER R> UMAX
;		ELSE
;			DUP EMIT OVER C! 1+ OVER UMIN
;       THEN            -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - BL EMIT;
	DW link
	DB 0
link SET $
	DB 6,"ACCEPT"
ACCEPT
	sep colonpc
	DW OVER,PLUS,ONEMINUS,OVER
	IF FC NE 0
	DW LIT,$11,EMIT
	ENDI
ACC1 
	DW KEY,DUP,LIT,$0D,NOTEQUAL,qbranch,ACC5
	DW DUP
	IF ANSI NE 0
		DW LIT,8,EQUAL,qbranch,ACC3
	ELSE 
		DW LIT,127,EQUAL,qbranch,ACC3
		DW DROP,LIT,8
	ENDI
	DW DUP,EMIT,BL,EMIT,EMIT,ONEMINUS,TOR,OVER,RFROM,UMAX
	DW branch,ACC4
ACC3
	DW DUP,EMIT,OVER,CSTORE,ONEPLUS,OVER,UMIN
ACC4
	DW branch,ACC1
ACC5
	DW DROP,NIP,SWAP,MINUS,BL,EMIT,EXIT

;C TYPE    c-addr +n --		type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
	DW link
	DB 0
link SET $
	DB 4,"TYPE"
TYPE
	sep colonpc
	DW QDUP,qbranch,TYP4
	DW OVER,PLUS,SWAP,xdo
TYP3   DW II,CFETCH,EMIT,xloop,TYP3
	DW branch,TYP5
TYP4   DW DROP
TYP5   DW EXIT

;Z (S")     -- c-addr u		run-time code for S"
;   R> COUNT 2DUP + ALIGNED >R  ;
	DW link
	DB 0
link SET $
	DB 4,"(S\")"
XSQUOTE
	sep colonpc
	DW RFROM,COUNT,TWODUP,PLUS,ALIGNED,TOR
	DW EXIT

;C S"       --			compile in-line string
; [CHAR] " PARSE POSTPONE SLITERAL
; IMMEDIATE 
	DW link
	DB 1
link SET $
	DB 2,"S\""
SQUOTE
	sep colonpc
	DW LIT,$22,PARSE
	DW SLITERAL,EXIT

;C ."       --			compile string to print
;   S"  POSTPONE TYPE ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 2,".\""
DOTQUOTE
	sep colonpc
	DW SQUOTE
	DW COMPILE,TYPE
	DW EXIT
			
; NUMERIC OUTPUT ================================
; Numeric conversion is done l.s.digit first, so
; the output buffer is built backwards in memory.

; Some double-precision arithmetic operators are
; needed to implement ANSI numeric conversion.

;Z UD/MOD   ud1 u2 -- u3 ud4	32/16->32 divide
;   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
	DW link
	DB 0
link SET $
	DB 6,"UD/MOD"
UDSLASHMOD
	sep colonpc
	DW TOR,ZERO,RFETCH,UMSLASHMOD,ROT,ROT
	DW RFROM,UMSLASHMOD,ROT,EXIT

;Z UD*      ud1 d2 -- ud3	32*16->32 multiply
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;
	DW link
	DB 0
link SET $
	DB 3,"UD*"
UDSTAR
	sep colonpc
	DW DUP,TOR,UMSTAR,DROP
	DW SWAP,RFROM,UMSTAR,ROT,PLUS,EXIT

;C HOLD  char --		add char to output string
;   -1 HP +!  HP @ C! ;
	DW link
	DB 0
link SET $
	DB 4,"HOLD"
HOLD
	sep colonpc
	DW MINUSONE,HP,PLUSSTORE
	DW HP,FETCH,CSTORE,EXIT

;C <#    --			 begin numeric conversion
;   PAD HP ! ;			(initialize Hold Pointer)
	DW link
	DB 0
link SET $
	DB 2,"<#"
LESSNUM
	sep colonpc
	DW PAD,HP,STORE,EXIT

;Z >DIGIT   n -- c		convert to 0..9A..Z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
	DW link
	DB 0
link SET $
	DB 6,">DIGIT"
TODIGIT
	sep colonpc
	DW DUP,LIT,9,GREATER,LIT,7,ANDD,PLUS
	DW LIT,$30,PLUS,EXIT

;C #     ud1 -- ud2		convert 1 digit of output
;   BASE @ UD/MOD ROT >DIGIT HOLD ;
	DW link
	DB 0
link SET $
	DB 1,"#"
NUM
	sep colonpc
	DW BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
	DW HOLD,EXIT

;C #S    ud1 -- ud2		convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
	DW link
	DB 0
link SET $
	DB 2,"#S"
NUMS
	sep colonpc
NUMS1  DW NUM,TWODUP,ORR,ZEROEQUAL,qbranch,NUMS1
	DW EXIT

;C #>    ud1 -- c-addr u	end conv., get string
;   2DROP HP @ PAD OVER - ;
	DW link
	DB 0
link SET $
	DB 2,"#>"
NUMGREATER
	sep colonpc
	DW TWODROP,HP,FETCH,PAD,OVER,MINUS,EXIT

;C SIGN  n --			add minus sign if n<0
;   0< IF 2D HOLD THEN ;
	DW link
	DB 0
link SET $
	DB 4,"SIGN"
SIGN
	sep colonpc
	DW ZEROLESS,qbranch,SIGN1,LIT,$2D,HOLD
SIGN1  DW EXIT

;C U.    u --			display u unsigned
;   <# 0 #S #> TYPE SPACE ;
	DW link
	DB 0
link SET $
	DB 2,"U."
UDOT
	sep colonpc
	DW LESSNUM,ZERO,NUMS,NUMGREATER,TYPE
	DW SPACE,EXIT

;C .     n --			display n signed
;   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
	DW link
	DB 0
link SET $
	DB 1,"."
DOT
	sep colonpc
	DW LESSNUM,DUP,ABS,ZERO,NUMS
	DW ROT,SIGN,NUMGREATER,TYPE,SPACE,EXIT

;C DECIMAL  --			set number base to decimal
;   10 BASE ! ;
	DW link
	DB 0
link SET $
	DB 7,"DECIMAL"
DECIMAL
	sep colonpc
	DW LIT,10,BASE,STORE,EXIT

;X HEX     --			set number base to hex
;   16 BASE ! ;
	DW link
	DB 0
link SET $
	DB 3,"HEX"
HEX
	sep colonpc
	DW LIT,16,BASE,STORE,EXIT

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr		returns dictionary ptr
;   DP @ ;
	DW link
	DB 0
link SET $
	DB 4,"HERE"
HERE
	sep colonpc
	DW DP,FETCH,EXIT

;C ALLOT   n --			allocate n bytes in dict
;   DP +! ;
	DW link
	DB 0
link SET $
	DB 5,"ALLOT"
ALLOT
	sep colonpc
	DW DP,PLUSSTORE,EXIT

; Note: , and C, are only valid for combined
; Code and Data spaces.

;C ,    x --			append cell to dict
;   HERE ! CELL ALLOT ;
	DW link
	DB 0
link SET $
	DB 1,","
COMMA
	sep colonpc
	DW HERE,STORE,CELL,ALLOT,EXIT

;C C,   char --			append char to dict
;   HERE C! 1 CHARS ALLOT ;
	DW link
	DB 0
link SET $
	DB 2,"C,"
CCOMMA
	sep colonpc
	DW HERE,CSTORE,ONE,CHARS,ALLOT,EXIT

; INTERPRETER ===================================
; Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
; are dependent on the structure of the Forth
; header.  This may be common across many CPUs,
; or it may be different.

;C SOURCE   -- adr n	current input buffer
;   'SOURCE 2@ ;	length is at lower adrs
	DW link
	DB 0
link SET $
	DB 6,"SOURCE"
SOURCE
	sep colonpc
	DW TICKSOURCE,TWOFETCH,EXIT

;S /STRING  a u n -- a+n u-n	trim string
;   ROT OVER + ROT ROT - ;
	DW link
	DB 0
link SET $
	DB 7,"/STRING"
SLASHSTRING
	sep colonpc
	DW ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

;Z >COUNTED  src n dst --	copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
	DW link
	DB 0
link SET $
	DB 8,">COUNTED"
TOCOUNTED
	sep colonpc
	DW TWODUP,CSTORE,CHARPLUS,SWAP,CMOVE,EXIT

;C WORD   char -- c-addr 	word delim'd by char
;   DUP  SOURCE >IN @ /STRING	-- c c adr n
;   DUP >R   ROT skip		-- c adr' n'
;   OVER >R  ROT scan		-- adr" n"
;   DUP IF CHAR- THEN		skip trailing delim.
;   R> R> ROT -   >IN +!	update >IN offset
;   TUCK -			-- adr' N
;   HERE >counted		--
;   HERE			-- a
;   BL OVER COUNT + C!		add trailing blank
	DW link
	DB 0
link SET $
	DB 4,"WORD"
FWORD
	sep colonpc
	DW DUP,SOURCE,TOIN,FETCH,SLASHSTRING
	DW DUP,TOR,ROT,skip
	DW OVER,TOR,ROT,scan
	DW DUP,qbranch,FWORD1,ONEMINUS  ; char-
FWORD1  DW RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
	DW TUCK,MINUS
	DW HERE,TOCOUNTED,HERE
        DW BL,OVER,COUNT,PLUS,CSTORE
	DW EXIT

;Z NFA>LFA   nfa -- lfa		name adr -> link field
;   3 - ;    Used in the inner loop of FIND
	DW link
	DB 0
link SET $
	DB 7,"NFA>LFA"
NFATOLFA
	ldn psp		; lo
	smi $3
	str psp
	inc psp
	ldn psp		; hi
	smbi $0
	stxd
	sep nextpc

;Z NFA>CFA   nfa -- cfa	name adr -> code field
;   COUNT 7F AND + ;	mask off 'smudge' bit
	DW link
	DB 0
link SET $
	DB 7,"NFA>CFA"
NFATOCFA
	sep colonpc
	DW COUNT,LIT,$07F,ANDD,PLUS,EXIT

;Z IMMED?    nfa -- f	fetch immediate flag
;   1- C@ ;		nonzero if immed
	DW link
	DB 0
link SET $
	DB 6,"IMMED?"
IMMEDQ
	sep colonpc
	DW ONEMINUS,CFETCH,EXIT

;C FIND   c-addr -- c-addr 0	if not found
; 		  xt  1		if immediate
; 		  xt -1		if "normal"
;   LATEST @ BEGIN		-- a nfa
;       2DUP OVER C@ CHAR+	-- a nfa a nfa n+1
;       S=			-- a nfa f
;       DUP IF
;	   DROP
;	   NFA>LFA @ DUP	-- a link link
;       THEN
;   0= UNTIL			-- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA		-- nfa xt
;       SWAP IMMED?		-- xt iflag
;       0= 1 OR			-- xt 1/-1
;   THEN ;
	DW link
	DB 0
link SET $
	DB 4,"FIND"
FIND
	sep colonpc
	DW LATEST,FETCH
FIND1  DW TWODUP,OVER,CFETCH,CHARPLUS
	DW SEQUAL,DUP,qbranch,FIND2
	DW DROP,NFATOLFA,FETCH,DUP
FIND2  DW ZEROEQUAL,qbranch,FIND1
	DW DUP,qbranch,FIND3
	DW NIP,DUP,NFATOCFA
	DW SWAP,IMMEDQ,ZEROEQUAL,ONE,ORR
FIND3  DW EXIT

;C LITERAL  x --		append numeric literal
;   STATE @ IF COMPILE LIT , THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
	DW link
	DB 1
link SET $
	DB 7,"LITERAL"
LITERAL
	sep colonpc
	DW STATE,FETCH,qbranch,LITER1
	DW COMPILE,LIT,COMMA
LITER1 DW EXIT

;Z DIGIT?   c -- n -1		if c is a valid digit
; 	    -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +	silly looking
;   DUP 140 > 107 AND -   30 -	but it works!
;   DUP BASE @ U< ;
	DW link
	DB 0
link SET $
	DB 6,"DIGIT?"
DIGITQ
	sep colonpc
	DW DUP,LIT,$39,GREATER,LIT,$100,ANDD,PLUS
	DW DUP,LIT,$140,GREATER,LIT,$107,ANDD
	DW MINUS,LIT,$30,MINUS
	DW DUP,BASE,FETCH,ULESS,EXIT

;Z ?SIGN   adr n -- adr' n' f	get optional sign
;   advance adr/n if sign;	return NZ if negative
;   OVER C@			-- adr n c
;   2C - DUP ABS 1 = AND	-- +=-1, -=+1, else 0
;   DUP IF 1+			-- +=0, -=+2
;       >R 1 /STRING R>		-- adr' n' f
;   THEN ;
	DW link
	DB 0
link SET $
	DB 5,"?SIGN"
QSIGN
	sep colonpc
	DW OVER,CFETCH,LIT,$2C,MINUS,DUP,ABS
	DW ONE,EQUAL,ANDD,DUP,qbranch,QSIGN1
	DW ONEPLUS,TOR,ONE,SLASHSTRING,RFROM
QSIGN1 DW EXIT

;C >NUMBER  ud adr u -- ud' adr' u'
;				convert string to number
;   BEGIN
;   DUP WHILE
;       OVER C@ DIGIT?
;       0= IF DROP EXIT THEN
;       >R 2SWAP BASE @ UD*
;       R> M+ 2SWAP
;       1 /STRING
;   REPEAT ;
	DW link
	DB 0
link SET $
	DB 7,">NUMBER"
TONUMBER
	sep colonpc
TONUM1 DW DUP,qbranch,TONUM3
	DW OVER,CFETCH,DIGITQ
	DW ZEROEQUAL,qbranch,TONUM2,DROP,EXIT
TONUM2 DW TOR,TWOSWAP,BASE,FETCH,UDSTAR
	DW RFROM,MPLUS,TWOSWAP
	DW ONE,SLASHSTRING,branch,TONUM1
TONUM3 DW EXIT

;Z ?NUMBER  c-addr -- n -1	string->number
; 		 -- c-addr 0	if convert error
;   DUP  0 0 ROT COUNT		-- ca ud adr n
;   ?SIGN >R  >NUMBER		-- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0	-- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1	-- n -1   (ok)
;   THEN ;
	DW link
	DB 0
link SET $
	DB 7,"?NUMBER"
QNUMBER
	sep colonpc
	DW DUP,ZERO,DUP,ROT,COUNT
	DW QSIGN,TOR,TONUMBER,qbranch,QNUM1
	DW RFROM,TWODROP,TWODROP,ZERO
	DW branch,QNUM3
QNUM1  DW TWODROP,NIP,RFROM,qbranch,QNUM2,NEGATE
QNUM2  DW MINUSONE
QNUM3  DW EXIT

;Z INTERPRET    i*x c-addr u -- j*x
; 				interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
;   'SOURCE 2!  0 >IN !
;   BEGIN
;   BL WORD DUP C@ WHILE	-- textadr
;       FIND			-- a 0/1/-1
;       ?DUP IF			-- xt 1/-1
;	   1+ STATE @ 0= OR	immed or interp?
;	   IF EXECUTE ELSE ,XT THEN
;       ELSE			-- textadr
;	   ?NUMBER
;	   IF POSTPONE LITERAL	converted ok
;	   ELSE COUNT TYPE 3F EMIT CR ABORT  err
;	   THEN
;       THEN
;   REPEAT DROP ;
	DW link
	DB 0
link SET $
	DB 9,"INTERPRET"
INTERPRET
	sep colonpc
	DW TICKSOURCE,TWOSTORE,ZERO,TOIN,STORE
INTER1 DW BL,FWORD,DUP,CFETCH,qbranch,INTER9
	DW FIND,QDUP,qbranch,INTER4
	DW ONEPLUS,STATE,FETCH,ZEROEQUAL,ORR
	DW qbranch,INTER2
	DW EXECUTE,branch,INTER3
INTER2 DW COMMAXT
INTER3 DW branch,INTER1
INTER4 DW QNUMBER,qbranch,INTER5
	DW LITERAL,branch,INTER1
INTER5 DW COUNT,TYPE,LIT,$3F,EMIT,CR,ABORT

INTER9 DW DROP,EXIT

;C EVALUATE  i*x c-addr u -- j*x	interpret string
;   'SOURCE 2@ >R >R  >IN @ >R
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! ;
	DW link
	DB 0
link SET $
	DB 8,"EVALUATE"
EVALUATE
	sep colonpc
	DW TICKSOURCE,TWOFETCH,TOR,TOR
	DW TOIN,FETCH,TOR,INTERPRET
	DW RFROM,TOIN,STORE,RFROM,RFROM
	DW TICKSOURCE,TWOSTORE,EXIT

;C QUIT     --    R: i*x --		interpret from kbd
;   L0 LP !  R0 RP!   0 STATE !
;   BEGIN
;       TIB DUP TIBSIZE ACCEPT
;		[fc?] [IF] xoff EMIT
;       [THEN] INTERPRET
;       STATE @ 0= IF CR ." OK" THEN
;   AGAIN ;
	DW link
	DB 0
link SET $
	DB 4,"QUIT"
QUIT
	sep colonpc
	DW L0,LP,STORE
	DW R0,RPSTORE,ZERO,STATE,STORE
QUIT1  DW TIB,DUP,TIBSIZE,ACCEPT
	IF FC NE 0 
		DW LIT,$13,EMIT
	ENDI
	DW INTERPRET, CR
	DW STATE,FETCH,ZEROEQUAL,qbranch,QUIT2
	DW XSQUOTE
	DB 3,"ok "
	DW TYPE
QUIT2  DW branch,QUIT1

;C ABORT    i*x --   R: j*x --	clear stk & QUIT
;   S0 SP!  QUIT ;
	DW link
	DB 0
link SET $
	DB 5,"ABORT"
ABORT
	sep colonpc
	DW S0,SPSTORE,QUIT	; QUIT never returns

;Z ?ABORT   f c-addr u --	abort & print msg
;   ROT IF TYPE ABORT THEN 2DROP ;
	DW link
	DB 0
link SET $
	DB 6,"?ABORT"
QABORT
	sep colonpc
	DW ROT,qbranch,QABO1,TYPE,ABORT
QABO1  DW TWODROP,EXIT

;C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
; 	 i*x x1 --       R: j*x --      x1<>0
;   POSTPONE S" POSTPONE ?ABORT ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 6,"ABORT\""
ABORTQUOTE
	sep colonpc
	DW SQUOTE
	DW COMPILE,QABORT
	DW EXIT

;C '    -- xt		find word in dictionary
;   FINDWORD DROP
	DW link
	DB 0
link SET $ 
	DB 1,"\'"
TICK   sep colonpc
	DW FINDWORD,DROP,EXIT

;C CHAR   -- char	parse ASCII character
;   BL WORD 1+ C@ ;
	DW link
	DB 0
link SET $
	DB 4,"CHAR"
CHAR
	sep colonpc
	DW BL,FWORD,ONEPLUS,CFETCH,EXIT

;C [CHAR]   --		compile character literal
;   CHAR COMPILE LIT  , ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 6,"[CHAR]"
BRACCHAR
	sep colonpc
	DW CHAR
	DW COMPILE,LIT
	DW COMMA,EXIT

;C (    --		skip input until )
;   [ HEX ] 29 PARSE 2DROP ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 1,"("
PAREN
	sep colonpc
	DW LIT,$29,PARSE,TWODROP,EXIT

; COMPILER ======================================

;Z (CREATE)	-- 	create link, immediate, and name fields
;   LATEST @ , 0 C,link & immed field
;   HERE LATEST !	new "latest" link
;   BL WORD C@ 1+ ALLOT	name field
	DW link
	DB 0
link SET $
	DB 8,"(CREATE)"
XCREATE
	sep colonpc
	DW LATEST,FETCH,COMMA,ZERO,CCOMMA
	DW HERE,LATEST,STORE
	DW BL,FWORD,CFETCH,ONEPLUS,ALLOT
	DW EXIT

;C CREATE   --		create an empty definition with 3-byte code field
;   (CREATE) CFCOMPILE docreate COMPILE noop	code field
	DW link
	DB 0
link SET $
	DB 6,"CREATE"
CREATE
	sep colonpc
	DW XCREATE,CFCOMPILE
	sep createpc
	DW COMPILE,noop,EXIT	; default DOES> part

;Z (DOES>)  --	run-time action of DOES>
;   R>	      adrs of headless DOES> def'n
;   LATEST @ NFA>CFA 1+ !   code field to fix up
	DW link
	DB 0
link SET $
	DB 7,"(DOES>)"
xdoES
	sep colonpc
	DW RFROM,LATEST,FETCH,NFATOCFA,ONEPLUS,STORE
	DW EXIT

;C DOES>    --		change action of latest def'n
; ANSI 6.1.1250 says that DOES> only applies to CREATE'd 
; definitions, which have a 3-byte CFA
;   COMPILE (DOES>)
;   CFCOMPILE docolon ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 5,"DOES>"
DOES
	sep colonpc
	DW COMPILE,xdoES,CFCOMPILE
	sep colonpc
	DW EXIT

;C RECURSE  --		recurse current definition
;   LATEST @ NFA>CFA ,XT ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 7,"RECURSE"
RECURSE
	sep colonpc
	DW LATEST,FETCH,NFATOCFA,COMMAXT,EXIT

;C [	--		enter interpretive state
;   0 STATE ! ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 1,"["
LEFTBRACKET
	sep colonpc
	DW ZERO,STATE,STORE,EXIT

;C ]	--		enter compiling state
;   -1 STATE ! ;
	DW link
	DB 0
link SET $
	DB 1,"]"
RIGHTBRACKET
	sep colonpc
	DW MINUSONE,STATE,STORE,EXIT

;Z HIDE     --		"hide" latest definition
;   LATEST @ DUP C@ 80 OR SWAP C! ;
	DW link
	DB 0
link SET $
	DB 4,"HIDE"
HIDE
	sep colonpc
	DW LATEST,FETCH,DUP,CFETCH,LIT,$80,ORR
	DW SWAP,CSTORE,EXIT

;Z REVEAL   --		"reveal" latest definition
;   LATEST @ DUP C@ 7F AND SWAP C! ;
	DW link
	DB 0
link SET $
	DB 6,"REVEAL"
REVEAL
	sep colonpc
	DW LATEST,FETCH,DUP,CFETCH,LIT,$7F,ANDD
	DW SWAP,CSTORE,EXIT

;C IMMEDIATE   --	make last def'n immediate
;   1 LATEST @ 1- C! ;	set immediate flag
	DW link
	DB 0
link SET $
	DB 9,"IMMEDIATE"
IMMEDIATE
	sep colonpc
	DW ONE,LATEST,FETCH,ONEMINUS,CSTORE
	DW EXIT

;C :	--		begin a colon definition
;   CREATE HIDE ] CFCOMPILE docolon ;
	DW link
	DB 0
link SET $
	DB 1,":"
COLON
	sep colonpc
	DW XCREATE,HIDE,RIGHTBRACKET,CFCOMPILE
	sep colonpc
	DW EXIT

;C ;
;   REVEAL  ,EXIT
;   POSTPONE [  ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 1,";"
SEMICOLON
	sep colonpc
	DW REVEAL,CEXIT
	DW LEFTBRACKET,EXIT

;C [']  --		find word & compile as literal
;   ' COMPILE LIT , ; IMMEDIATE
; When encountered in a colon definition, the
; phrase  ['] xxx  will cause   LIT,xxt  to be
; compiled into the colon definition (where
; (where xxt is the execution token of word xxx).
; When the colon definition executes, xxt will
; be put on the stack.  (All xt's are one cell.)
	DW link
	DB 1
link SET $
	DB 3,"[\']"     ; tick character
BRACTICK 
	sep colonpc
	DW TICK		; get xt of 'xxx'
	DW COMPILE,LIT		; append LIT action
	DW COMMA,EXIT		; append xt literal

;C POSTPONE  --		postpone compile action of word
;   FINDWORD
;   0< IF   -- xt	non immed: add code to current
;			def'n to compile xt later.
;       COMPILE LIT ,	add "LIT,xt,COMMAXT"
;       COMPILE ,XT	to current definition
;   ELSE  ,XT      immed: compile into cur. def'n
;   THEN ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 8,"POSTPONE"
POSTPONE
	sep colonpc
	DW FINDWORD,ZEROLESS,qbranch,POST1
	DW COMPILE,LIT,COMMA
	DW COMPILE
POST1  DW COMMAXT,EXIT
	       
;Z COMPILE   --		append inline execution token
;   R> DUP CELL+ >R @ ,XT ;
; The phrase ['] xxx ,XT appears so often that
; this word was created to combine the actions
; of LIT and ,XT.  It takes an inline literal
; execution token and appends it to the dict.
	DW link
	DB 0
link SET $
	DB 7,"COMPILE"
COMPILE
	sep colonpc
	DW RFROM,DUP,CELLPLUS,TOR
	DW FETCH,COMMAXT,EXIT
    
; CONTROL STRUCTURES ============================

;C IF       -- adrs	conditional forward branch
;   ['] qbranch ,BRANCH HERE DUP ,DEST ;
;   IMMEDIATE
	DW link
	DB 1
link SET $
	DB 2,"IF"
IF
	sep colonpc
	DW LIT,qbranch,COMMABRANCH
	DW HERE,DUP,COMMADEST,EXIT

;C THEN     adrs --	resolve forward branch
;   HERE SWAP !DEST ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 4,"THEN"
THEN
	sep colonpc
	DW HERE,SWAP,STOREDEST,EXIT

;C ELSE     adrs1 -- adrs2	branch for IF..ELSE
;   POSTPONE AHEAD SWAP POSTPONE THEN ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 4,"ELSE"
ELSE
	sep colonpc
	DW AHEAD
	DW SWAP,THEN,EXIT

;C BEGIN    -- adrs		target for bwd. branch
;   HERE ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 5,"BEGIN"
BEGIN
	lbr HERE

;C UNTIL    adrs --		conditional backward branch
;   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward branch
	DW link
	DB 1
link SET $
	DB 5,"UNTIL"
UNTIL
	sep colonpc
	DW LIT,qbranch,COMMABRANCH
	DW COMMADEST,EXIT

;X AGAIN    adrs --		uncond'l backward branch
;   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward branch
	DW link
	DB 1
link SET $
	DB 5,"AGAIN"
AGAIN
	sep colonpc
	DW LIT,branch,COMMABRANCH
	DW COMMADEST,EXIT

;C WHILE    -- adrs		branch for WHILE loop
;   POSTPONE IF SWAP ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 5,"WHILE"
WHILE
	sep colonpc
	DW IF,SWAP,EXIT

;C REPEAT   adrs1 adrs2 --	resolve WHILE loop
;   POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 6,"REPEAT"
REPEAT
	sep colonpc
	DW AGAIN,THEN,EXIT

;Z >L   x --   L: -- x		move to leave stack
;   CELL LP +!  LP @ ! ;	(L stack grows up)
	DW link
	DB 0
link SET $
	DB 2,">L"
TOL
	sep colonpc
	DW CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

;Z L>   -- x   L: x --		move from leave stack
;   LP @ @  CELL NEGATE LP +! ;
	DW link
	DB 0
link SET $
	DB 2,"L>"
LFROM
	sep colonpc
	DW LP,FETCH,FETCH
	DW CELL,NEGATE,LP,PLUSSTORE,EXIT

;C DO       -- adrs   L: -- 0
;    0 	( no fwd branch )
;   BEGINLOOP ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 2,"DO"
DO
	sep colonpc
	DW ZERO	; no forward branch
	DW BEGINLOOP,EXIT

;Z ENDLOOP   baddr faddr xt --   L: 0 a1 a2 .. aN --
;   ,BRANCH  ,DEST		backward loop
;   ?DUP IF			resolve forward
;        POSTPONE THEN		branch if any
;   THEN
;   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
;				resolve LEAVEs
; This is a common factor of LOOP and +LOOP.
	DW link
	DB 0
link SET $
	DB 7,"ENDLOOP"
ENDLOOP
	sep colonpc
	DW COMMABRANCH,COMMADEST
	DW QDUP,qbranch,LOOP1	; is there a fwd branch?
	DW THEN		; resolve fwd branch
LOOP1  DW LFROM,QDUP,qbranch,LOOP2
	DW THEN,branch,LOOP1	; resolve LEAVE branches
LOOP2  DW EXIT

;C LOOP    adrs --   L: 0 a1 a2 .. aN --
;   ['] xloop ENDLOOP ;  IMMEDIATE
	DW link
	DB 1
link SET $
	DB 4,"LOOP"
LOOP
	sep colonpc
	DW LIT,xloop,ENDLOOP,EXIT

;C +LOOP   adrs --   L: 0 a1 a2 .. aN --
;   ['] xplusloop ENDLOOP ;  IMMEDIATE
	DW link
	DB 1
link SET $
	DB 5,"+LOOP"
PLUSLOOP
	sep colonpc
	DW LIT,xplusloop,ENDLOOP,EXIT

;C LEAVE    --    L: -- adrs
;   COMPILE UNLOOP
;   ['] branch ,BRANCH   HERE DUP ,DEST  >L
;   ; IMMEDIATE      unconditional forward branch
	DW link
	DB 1
link SET $
	DB 5,"LEAVE"
LEAVE
	sep colonpc
	DW COMPILE,UNLOOP
	DW LIT,branch,COMMABRANCH
	DW HERE,DUP,COMMADEST,TOL,EXIT

; OTHER OPERATIONS ==============================

;X WITHIN   n1|u1 n2|u2 n3|u3 -- f	n2<=n1<n3?
;  OVER - >R - R> U< ;			per ANS document
	DW link
	DB 0
link SET $
	DB 6,"WITHIN"
WITHIN
	sep colonpc
	DW OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

;C MOVE    addr1 addr2 u --		smart move
;	     VERSION FOR 1 ADDRESS UNIT = 1 CHAR
;  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
;  WITHIN IF  R> CMOVE>	src <= dst < src+n
;       ELSE  R> CMOVE  THEN ;	  otherwise
	DW link
	DB 0
link SET $
	DB 4,"MOVE"
MOVE
	sep colonpc
	DW TOR,TWODUP,SWAP,DUP,RFETCH,PLUS
	DW WITHIN,qbranch,MOVE1
	DW RFROM,CMOVEUP,branch,MOVE2
MOVE1  DW RFROM,CMOVE
MOVE2  DW EXIT

;C DEPTH    -- +n		number of items on stack
;   SP@ S0 SWAP - 2/ ;		16-BIT VERSION!
	DW link
	DB 0
link SET $
	DB 5,"DEPTH"
DEPTH
	sep colonpc
	DW SPFETCH,S0,SWAP,MINUS,TWOSLASH,EXIT

;C ENVIRONMENT?  c-addr u -- false	system query
;		 -- i*x true
;   2DROP 0 ;			the minimal definition!
	DW link
	DB 0
link SET $
	DB 12,"ENVIRONMENT?"
ENVIRONMENTQ
	sep colonpc
	DW TWODROP,ZERO,EXIT

; UTILITY WORDS AND STARTUP =====================

;T WORDS    --			list all words in dict.
;   LATEST @ BEGIN
;       DUP COUNT TYPE SPACE
;       NFA>LFA @
;   DUP 0= UNTIL
;   DROP ;
	DW link
	DB 0
link SET $
	DB 5,"WORDS"
WORDS
	sep colonpc
	DW LATEST,FETCH
WDS1   DW DUP,COUNT,TYPE,SPACE,NFATOLFA,FETCH
	DW DUP,ZEROEQUAL,qbranch,WDS1
	DW DROP,EXIT

;T .S      --			print stack contents
;   SP@ S0 - IF
;       SP@ S0 2 - DO I @ U. -2 +LOOP
;   THEN ;
	DW link
	DB 0
link SET $
	DB 2,".S"
DOTS
	sep colonpc
	DW SPFETCH,S0,MINUS,qbranch,DOTS2
	DW SPFETCH,S0,LIT,2,MINUS,xdo
DOTS1  DW II,FETCH
	DW swapbytes		; parameter stack data is little-endian
	DW UDOT,LIT,-2,xplusloop,DOTS1
DOTS2  DW EXIT

;Z TRUECOLD -- Hidden word invoked at start
; holding IN when pressing enter forces a cold start
; GENBAUD EF4? IF COLD THEN 45296 MAGIC @ = IF WARM THEN COLD ;
TRUECOLD
	sep colonpc
	DW GENBAUD,EF4Q,qbranch,TRUECO1D,COLD
TRUECO1D
	DW LIT,28912,MAGIC,FETCH,EQUAL,qbranch,COLD1,WARM

;Z COLD     --			cold start Forth system
;   UINIT U0 #INIT CMOVE WARM  init user area
	DW link
	DB 0
link SET $
	DB 4,"COLD"
COLD
	sep colonpc
COLD1
	DW UINIT,U0,NINIT,CMOVE,WARM
;Z WARM		--		warm start Forth system
;   ." RCA1802 CamelForth etc."
;   FREE . ." Dictionary Bytes free" ABORT ;
	DW link
	DB 0
link SET $
	DB 4,"WARM"
WARM
	sep colonpc
WARM1
	DW XSQUOTE
	DB 56			; length of sign-on string
	DB "RCA1802 CamelForth v1.1 - MC Edition v0.2  22 Jul 2024"
	DB $0D,$0A
	DW TYPE
	DW UNUSED,UDOT,XSQUOTE
	DB 23
	DB "Dictionary Bytes free"
	DB $0D,$0A
	DW TYPE
	IF FC NE 0
		DW LIT,$13,EMIT ;Fool the state machine so we don't retransmit!
	ENDI
	DW ABORT		; ABORT never returns

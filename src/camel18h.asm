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
;   Source code is for the A180 assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================

; SYSTEM VARIABLES & CONSTANTS ==================

;C BL      -- char	an ASCII space
	.dw link
	.db 0
	.set link,*
	.db 2,"BL"
BL:
	sep constpc
	.dw H'20

;X #TIB  -- n	 size of TIB
	.dw link
	.db 0
	.set link,*
	.db 4,"#TIB"
TIBSIZE:
	sep constpc
	.dw tibend-tibarea

;X TIB     -- a-addr	Terminal Input Buffer
	.dw link
	.db 0
	.set link,*
	.db 3,"TIB"
TIB:
	sep constpc
	.dw tibarea

;Z u0      -- a-addr	current user area adrs
;  0 USER U0
	.dw link
	.db 0
	.set link,*
	.db 2,"U0"
U0:
	sep userpc
	.dw 0

;C >IN     -- a-addr	holds offset into TIB
;  2 USER >IN
	.dw link
	.db 0
	.set link,*
	.db 3,">IN"
TOIN:
	sep userpc
	.dw 2

;C BASE    -- a-addr	holds conversion radix
;  4 USER BASE
	.dw link
	.db 0
	.set link,*
	.db 4,"BASE"
BASE:
	sep userpc
	.dw 4

;C STATE   -- a-addr	holds compiler state
;  6 USER STATE
	.dw link
	.db 0
	.set link,*
	.db 5,"STATE"
STATE:
	sep userpc
	.dw 6

;Z DP      -- a-addr	holds dictionary ptr
;  8 USER DP
	.dw link
	.db 0
	.set link,*
	.db 2,"DP"
DP:
	sep userpc
	.dw 8

;Z 'SOURCE  -- a-addr	two cells: len, adrs
; 10 USER 'SOURCE
	.dw link
	.db 0
	.set link,*
	.db 7,"\'SOURCE"
TICKSOURCE:
	sep userpc
	.dw 10

;Z latest    -- a-addr	last word in dict.
;   14 USER LATEST
	.dw link
	.db 0
	.set link,*
	.db 6,"LATEST"
LATEST:
	sep userpc
	.dw 14

;Z HP       -- a-addr	HOLD pointer
;   16 USER HP
	.dw link
	.db 0
	.set link,*
	.db 2,"HP"
HP:
	sep userpc
	.dw 16

;Z LP       -- a-addr	Leave-stack pointer
;   18 USER LP
	.dw link
	.db 0
	.set link,*
	.db 2,"LP"
LP:
	sep userpc
	.dw 18

;Z MAGIC       -- a-addr magic num used by TRUECOLD to determine start
;   20 USER MAGIC
	.dw link
	.db 0
	.set link,*
	.db 5,"magic"
MAGIC:
	sep userpc
	.dw 20

;Z S0       -- a-addr	end of parameter stack
	.dw link
	.db 0
	.set link,*
	.db 2,"S0"
S0:
	sep constpc
	.dw paramstack

;X PAD       -- a-addr	user PAD buffer
;			= end of hold area!
	.dw link
	.db 0
	.set link,*
	.db 3,"PAD"
PAD:
	sep constpc
	.dw padarea

;Z L0       -- a-addr	bottom of Leave stack
	.dw link
	.db 0
	.set link,*
	.db 2,"L0"
L0:
	sep constpc
	.dw leavestack

;Z R0       -- a-addr	end of return stack
	.dw link
	.db 0
	.set link,*
	.db 2,"R0"
R0:
	sep constpc
	.dw returnstack

;Z UINIT    -- addr	initial values for user area
	.dw link
	.db 0
	.set link,*
	.db 5,"UINIT"
UINIT:
	sep varpc
	.dw 0,0,10,0	; reserved,>IN,BASE,STATE
	.dw enddict	; DP
	.dw 0,0		; SOURCE init'd elsewhere
	.dw lastword	; LATEST
	.dw 0		; HP init'd elsewhere
	.dw 0		; LP init'd elsewhere
	.dw 28912	; Magic word

;Z #INIT    -- n	#bytes of user area init data
	.dw link
	.db 0
	.set link,*
	.db 5,"#INIT"
NINIT:
	sep constpc
	.dw 22

; ARITHMETIC OPERATORS ==========================

;C S>D    n -- d	single -> double prec.
;   DUP 0< ;
	.dw link
	.db 0
	.set link,*
	.db 3,"S>D"
STOD:
	inc psp
	ldn psp		; n hi
	dec psp
	shlc		; sign to carry
	lbdf MINUSONE
	lbr ZERO

;Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;	...a common factor
	.dw link
	.db 0
	.set link,*
	.db 7,"?NEGATE"
QNEGATE:
	inc psp
	lda psp		; n2 hi
	shlc		; sign to carry
	lbdf NEGATE
	sep nextpc

;C ABS     n1 -- +n2	absolute value
;   DUP ?NEGATE ;
	.dw link
	.db 0
	.set link,*
	.db 3,"ABS"
ABS:
	inc psp
	ldn psp		; n1 hi
	dec psp
	shlc		; sign to carry
	lbdf NEGATE
	sep nextpc

;D DNEGATE   d1 -- d2	negate double precision
;   SWAP INVERT SWAP INVERT 1 M+ ;
	.dw link
	.db 0
	.set link,*
	.db 7,"DNEGATE"
DNEGATE:
	sep colonpc
	.dw SWAP,INVERT,SWAP,INVERT,ONE,MPLUS
	.dw EXIT

;Z ?DNEGATE  d1 n -- d2	negate d1 if n negative
;   0< IF DNEGATE THEN ;       ...a common factor
	.dw link
	.db 0
	.set link,*
	.db 8,"?DNEGATE"
QDNEGATE:
	sep colonpc
	.dw ZEROLESS,qbranch,DNEG1,DNEGATE
DNEG1:  .dw EXIT

;D DABS     d1 -- +d2	absolute value dbl.prec.
;   DUP ?DNEGATE ;
	.dw link
	.db 0
	.set link,*
	.db 4,"DABS"
DABS:
	sep colonpc
	.dw DUP,QDNEGATE,EXIT

;C M*     n1 n2 -- d	signed 16*16->32 multiply
;   2DUP XOR >R	carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE ;
	.dw link
	.db 0
	.set link,*
	.db 2,"M*"
MSTAR:
	sep colonpc
	.dw TWODUP,XORR,TOR
	.dw SWAP,ABS,SWAP,ABS,UMSTAR
	.dw RFROM,QDNEGATE,EXIT

;C SM/REM   d1 n1 -- n2 n3	symmetric signed div
;   2DUP XOR >R			sign of quotient
;   OVER >R			sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;
; Ref. dpANS-6 section 3.2.2.1.
	.dw link
	.db 0
	.set link,*
	.db 6,"SM/REM"
SMSLASHREM:
	sep colonpc
	.dw TWODUP,XORR,TOR,OVER,TOR
	.dw ABS,TOR,DABS,RFROM,UMSLASHMOD
	.dw SWAP,RFROM,QNEGATE,SWAP,RFROM,QNEGATE
	.dw EXIT

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
	.dw link
	.db 0
	.set link,*
	.db 6,"FM/MOD"
FMSLASHMOD:
	sep colonpc 
	.dw DUP,TOR 
	.dw TWODUP,XORR,TOR 
	.dw TOR 
	.dw DABS,RFETCH,ABS,UMSLASHMOD 
	.dw SWAP,RFROM,QNEGATE,SWAP 
	.dw RFROM,ZEROLESS,qbranch,FMMOD1 
	.dw NEGATE 
	.dw OVER,qbranch,FMMOD1 
	.dw RFETCH,ROT,MINUS,SWAP,ONEMINUS 
FMMOD1: 
	.dw RFROM,DROP,EXIT 

;C *      n1 n2 -- n3		signed multiply
;   M* DROP ;
	.dw link
	.db 0
	.set link,*
	.db 1,"*"
STAR:
	sep colonpc
	.dw MSTAR,DROP,EXIT

;C /MOD   n1 n2 -- n3 n4	signed divide/rem'dr
;   >R S>D R> FM/MOD ;
	.dw link
	.db 0
	.set link,*
	.db 4,"/MOD"
SLASHMOD:
	sep colonpc
	.dw TOR,STOD,RFROM,FMSLASHMOD,EXIT

;C /      n1 n2 -- n3		signed divide
;   /MOD nip ;
	.dw link
	.db 0
	.set link,*
	.db 1,"/"
SLASH:
	sep colonpc
	.dw SLASHMOD,NIP,EXIT

;C MOD    n1 n2 -- n3		signed remainder
;   /MOD DROP ;
	.dw link
	.db 0
	.set link,*
	.db 3,"MOD"
MOD:
	sep colonpc
	.dw SLASHMOD,DROP,EXIT

;C */MOD  n1 n2 n3 -- n4 n5	n1*n2/n3, rem&quot
;   >R M* R> FM/MOD ;
	.dw link
	.db 0
	.set link,*
	.db 5,"*/MOD"
SSMOD:
	sep colonpc
	.dw TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

;C */     n1 n2 n3 -- n4	n1*n2/n3
;   */MOD nip ;
	.dw link
	.db 0
	.set link,*
	.db 2,"*/"
STARSLASH:
	sep colonpc
	.dw SSMOD,NIP,EXIT

;C MAX    n1 n2 -- n3		signed maximum
;   2DUP < IF SWAP THEN DROP ;
	.dw link
	.db 0
	.set link,*
	.db 3,"MAX"
MAX:
	sep colonpc
	.dw TWODUP,LESS,qbranch,MAX1,SWAP
MAX1:   .dw DROP,EXIT

;C MIN    n1 n2 -- n3		signed minimum
;   2DUP > IF SWAP THEN DROP ;
	.dw link
	.db 0
	.set link,*
	.db 3,"MIN"
MIN:
	sep colonpc
	.dw TWODUP,GREATER,qbranch,MIN1,SWAP
MIN1:   .dw DROP,EXIT

; DOUBLE OPERATORS ==============================

;C 2@    a-addr -- x1 x2	fetch 2 cells
;   DUP CELL+ @ SWAP @ ;
;   the lower address will appear on top of stack
	.dw link
	.db 0
	.set link,*
	.db 2,"2@"
TWOFETCH:
	sep colonpc
	.dw DUP,CELLPLUS,FETCH,SWAP,FETCH,EXIT

;C 2!    x1 x2 a-addr --	store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
	.dw link
	.db 0
	.set link,*
	.db 2,"2!"
TWOSTORE:
	sep colonpc
	.dw SWAP,OVER,STORE,CELLPLUS,STORE,EXIT

;C 2DROP  x1 x2 --		drop 2 cells
;   DROP DROP ;
	.dw link
	.db 0
	.set link,*
	.db 5,"2DROP"
TWODROP:
	sep colonpc
	.dw DROP,DROP,EXIT

;C 2DUP   x1 x2 -- x1 x2 x1 x2	dup top 2 cells
;   OVER OVER ;
	.dw link
	.db 0
	.set link,*
	.db 4,"2DUP"
TWODUP:
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
	.dw link
	.db 0
	.set link,*
	.db 5,"2SWAP"
TWOSWAP:
	sep colonpc
	.dw ROT,TOR,ROT,RFROM,EXIT

;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
;   >R >R 2DUP R> R> 2SWAP ;
	.dw link
	.db 0
	.set link,*
	.db 5,"2OVER"
TWOOVER:
	sep colonpc
	.dw TOR,TOR,TWODUP,RFROM,RFROM
	.dw TWOSWAP,EXIT

; INPUT/OUTPUT ==================================

;C COUNT   c-addr1 -- c-addr2 u	counted->adr/len
;   DUP CHAR+ SWAP C@ ;
	.dw link
	.db 0
	.set link,*
	.db 5,"COUNT"
COUNT:
	sep colonpc
	.dw DUP,CHARPLUS,SWAP,CFETCH,EXIT

;C CR      --			output newline
;   0D EMIT 0A EMIT ;
	.dw link
	.db 0
	.set link,*
	.db 2,"CR"
CR:
	sep colonpc
	.dw lit,H'0D,EMIT,lit,H'0A,EMIT,EXIT

;C SPACE   --			output a space
;   BL EMIT ;
	.dw link
	.db 0
	.set link,*
	.db 5,"SPACE"
SPACE:
	sep colonpc
	.dw BL,EMIT,EXIT

;C SPACES   n --		output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
	.dw link
	.db 0
	.set link,*
	.db 6,"SPACES"
SPACES:
	sep colonpc
SPCS1:  .dw DUP,qbranch,SPCS2
	.dw SPACE,ONEMINUS,branch,SPCS1
SPCS2:  .dw DROP,EXIT

;Z UMIN     u1 u2 -- u		unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
	.dw link
	.db 0
	.set link,*
	.db 4,"UMIN"
UMIN:
	sep colonpc
	.dw TWODUP,UGREATER,QBRANCH,UMIN1,SWAP
UMIN1:  .dw DROP,EXIT

;Z UMAX    u1 u2 -- u		unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
	.dw link
	.db 0
	.set link,*
	.db 4,"UMAX"
UMAX:
	sep colonpc
	.dw TWODUP,ULESS,QBRANCH,UMAX1,SWAP
UMAX1:  .dw DROP,EXIT

;C ACCEPT  c-addr +n -- +n'	get line from term'l
;   OVER + 1- OVER      -- sa ea a
;   BEGIN KEY           -- sa ea a c
;   DUP 0D <> WHILE
;       DUP EMIT        -- sa ea a c
;       DUP 8 = IF  32 EMIT EMIT 1- >R OVER R> UMAX
;             ELSE  OVER C! 1+ OVER UMIN
;       THEN            -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - CR ;
	.dw link
	.db 0
	.set link,*
	.db 6,"ACCEPT"
ACCEPT:
	sep colonpc
	.dw OVER,PLUS,ONEMINUS,OVER
ACC1: 
	.dw KEY,DUP,LIT,H'0D,NOTEQUAL,QBRANCH,ACC5
	.dw DUP,EMIT,DUP,LIT,8,EQUAL,QBRANCH,ACC3
	.dw LIT,H'20,EMIT,EMIT,ONEMINUS,TOR,OVER,RFROM,UMAX
	.dw BRANCH,ACC4
ACC3:
	.dw OVER,CSTORE,ONEPLUS,OVER,UMIN
ACC4:
	.dw BRANCH,ACC1
ACC5:
	.dw DROP,NIP,SWAP,MINUS,CR,EXIT

;C TYPE    c-addr +n --		type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
	.dw link
	.db 0
	.set link,*
	.db 4,"TYPE"
TYPE:
	sep colonpc
	.dw QDUP,QBRANCH,TYP4
	.dw OVER,PLUS,SWAP,XDO
TYP3:   .dw II,CFETCH,EMIT,XLOOP,TYP3
	.dw BRANCH,TYP5
TYP4:   .dw DROP
TYP5:   .dw EXIT

;Z (S")     -- c-addr u		run-time code for S"
;   R> COUNT 2DUP + ALIGNED >R  ;
	.dw link
	.db 0
	.set link,*
	.db 4,"(S\")"
XSQUOTE:
	sep colonpc
	.dw RFROM,COUNT,TWODUP,PLUS,ALIGNED,TOR
	.dw EXIT

;C S"       --			compile in-line string
; [CHAR] " PARSE POSTPONE SLITERAL
; IMMEDIATE 
	.dw link
	.db 1
	.set link,*
	.db 2,"S\""
SQUOTE:
	sep colonpc
	.dw LIT,H'22,PARSE
	.dw SLITERAL,EXIT

;C ."       --			compile string to print
;   S"  POSTPONE TYPE ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 2,".\""
DOTQUOTE:
	sep colonpc
	.dw SQUOTE
	.dw COMPILE,TYPE
	.dw EXIT
			
; NUMERIC OUTPUT ================================
; Numeric conversion is done l.s.digit first, so
; the output buffer is built backwards in memory.

; Some double-precision arithmetic operators are
; needed to implement ANSI numeric conversion.

;Z UD/MOD   ud1 u2 -- u3 ud4	32/16->32 divide
;   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
	.dw link
	.db 0
	.set link,*
	.db 6,"UD/MOD"
UDSLASHMOD:
	sep colonpc
	.dw TOR,ZERO,RFETCH,UMSLASHMOD,ROT,ROT
	.dw RFROM,UMSLASHMOD,ROT,EXIT

;Z UD*      ud1 d2 -- ud3	32*16->32 multiply
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;
	.dw link
	.db 0
	.set link,*
	.db 3,"UD*"
UDSTAR:
	sep colonpc
	.dw DUP,TOR,UMSTAR,DROP
	.dw SWAP,RFROM,UMSTAR,ROT,PLUS,EXIT

;C HOLD  char --		add char to output string
;   -1 HP +!  HP @ C! ;
	.dw link
	.db 0
	.set link,*
	.db 4,"HOLD"
HOLD:
	sep colonpc
	.dw MINUSONE,HP,PLUSSTORE
	.dw HP,FETCH,CSTORE,EXIT

;C <#    --			 begin numeric conversion
;   PAD HP ! ;			(initialize Hold Pointer)
	.dw link
	.db 0
	.set link,*
	.db 2,"<#"
LESSNUM:
	sep colonpc
	.dw PAD,HP,STORE,EXIT

;Z >DIGIT   n -- c		convert to 0..9A..Z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
	.dw link
	.db 0
	.set link,*
	.db 6,">DIGIT"
TODIGIT:
	sep colonpc
	.dw DUP,LIT,9,GREATER,LIT,7,ANDD,PLUS
	.dw LIT,H'30,PLUS,EXIT

;C #     ud1 -- ud2		convert 1 digit of output
;   BASE @ UD/MOD ROT >DIGIT HOLD ;
	.dw link
	.db 0
	.set link,*
	.db 1,"#"
NUM:
	sep colonpc
	.dw BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
	.dw HOLD,EXIT

;C #S    ud1 -- ud2		convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
	.dw link
	.db 0
	.set link,*
	.db 2,"#S"
NUMS:
	sep colonpc
NUMS1:  .dw NUM,TWODUP,ORR,ZEROEQUAL,qbranch,NUMS1
	.dw EXIT

;C #>    ud1 -- c-addr u	end conv., get string
;   2DROP HP @ PAD OVER - ;
	.dw link
	.db 0
	.set link,*
	.db 2,"#>"
NUMGREATER:
	sep colonpc
	.dw TWODROP,HP,FETCH,PAD,OVER,MINUS,EXIT

;C SIGN  n --			add minus sign if n<0
;   0< IF 2D HOLD THEN ;
	.dw link
	.db 0
	.set link,*
	.db 4,"SIGN"
SIGN:
	sep colonpc
	.dw ZEROLESS,qbranch,SIGN1,LIT,H'2D,HOLD
SIGN1:  .dw EXIT

;C U.    u --			display u unsigned
;   <# 0 #S #> TYPE SPACE ;
	.dw link
	.db 0
	.set link,*
	.db 2,"U."
UDOT:
	sep colonpc
	.dw LESSNUM,ZERO,NUMS,NUMGREATER,TYPE
	.dw SPACE,EXIT

;C .     n --			display n signed
;   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
	.dw link
	.db 0
	.set link,*
	.db 1,"."
DOT:
	sep colonpc
	.dw LESSNUM,DUP,ABS,ZERO,NUMS
	.dw ROT,SIGN,NUMGREATER,TYPE,SPACE,EXIT

;C DECIMAL  --			set number base to decimal
;   10 BASE ! ;
	.dw link
	.db 0
	.set link,*
	.db 7,"DECIMAL"
DECIMAL:
	sep colonpc
	.dw LIT,10,BASE,STORE,EXIT

;X HEX     --			set number base to hex
;   16 BASE ! ;
	.dw link
	.db 0
	.set link,*
	.db 3,"HEX"
HEX:
	sep colonpc
	.dw LIT,16,BASE,STORE,EXIT

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr		returns dictionary ptr
;   DP @ ;
	.dw link
	.db 0
	.set link,*
	.db 4,"HERE"
HERE:
	sep colonpc
	.dw DP,FETCH,EXIT

;C ALLOT   n --			allocate n bytes in dict
;   DP +! ;
	.dw link
	.db 0
	.set link,*
	.db 5,"ALLOT"
ALLOT:
	sep colonpc
	.dw DP,PLUSSTORE,EXIT

; Note: , and C, are only valid for combined
; Code and Data spaces.

;C ,    x --			append cell to dict
;   HERE ! CELL ALLOT ;
	.dw link
	.db 0
	.set link,*
	.db 1,","
COMMA:
	sep colonpc
	.dw HERE,STORE,CELL,ALLOT,EXIT

;C C,   char --			append char to dict
;   HERE C! 1 CHARS ALLOT ;
	.dw link
	.db 0
	.set link,*
	.db 2,"C,"
CCOMMA:
	sep colonpc
	.dw HERE,CSTORE,ONE,CHARS,ALLOT,EXIT

; INTERPRETER ===================================
; Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
; are dependent on the structure of the Forth
; header.  This may be common across many CPUs,
; or it may be different.

;C SOURCE   -- adr n	current input buffer
;   'SOURCE 2@ ;	length is at lower adrs
	.dw link
	.db 0
	.set link,*
	.db 6,"SOURCE"
SOURCE:
	sep colonpc
	.dw TICKSOURCE,TWOFETCH,EXIT

;S /STRING  a u n -- a+n u-n	trim string
;   ROT OVER + ROT ROT - ;
	.dw link
	.db 0
	.set link,*
	.db 7,"/STRING"
SLASHSTRING:
	sep colonpc
	.dw ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

;Z >COUNTED  src n dst --	copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
	.dw link
	.db 0
	.set link,*
	.db 8,">COUNTED"
TOCOUNTED:
	sep colonpc
	.dw TWODUP,CSTORE,CHARPLUS,SWAP,CMOVE,EXIT

;C WORD   char -- c-addr 	word delim'd by char
;   DUP  SOURCE >IN @ /STRING	-- c c adr n
;   DUP >R   ROT SKIP		-- c adr' n'
;   OVER >R  ROT SCAN		-- adr" n"
;   DUP IF CHAR- THEN		skip trailing delim.
;   R> R> ROT -   >IN +!	update >IN offset
;   TUCK -			-- adr' N
;   HERE >counted		--
;   HERE			-- a
;   BL OVER COUNT + C!		add trailing blank
	.dw link
	.db 0
	.set link,*
	.db 4,"WORD"
WORD:
	sep colonpc
	.dw DUP,SOURCE,TOIN,FETCH,SLASHSTRING
	.dw DUP,TOR,ROT,SKIP
	.dw OVER,TOR,ROT,SCAN
	.dw DUP,qbranch,WORD1,ONEMINUS  ; char-
WORD1:  .dw RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
	.dw TUCK,MINUS
	.dw HERE,TOCOUNTED,HERE
        .dw BL,OVER,COUNT,PLUS,CSTORE
	.dw EXIT

;Z NFA>LFA   nfa -- lfa		name adr -> link field
;   3 - ;    Used in the inner loop of FIND
	.dw link
	.db 0
	.set link,*
	.db 7,"NFA>LFA"
NFATOLFA:
	ldn psp		; lo
	smi H'3
	str psp
	inc psp
	ldn psp		; hi
	smbi H'0
	stxd
	sep nextpc

;Z NFA>CFA   nfa -- cfa	name adr -> code field
;   COUNT 7F AND + ;	mask off 'smudge' bit
	.dw link
	.db 0
	.set link,*
	.db 7,"NFA>CFA"
NFATOCFA:
	sep colonpc
	.dw COUNT,LIT,H'07F,ANDD,PLUS,EXIT

;Z IMMED?    nfa -- f	fetch immediate flag
;   1- C@ ;		nonzero if immed
	.dw link
	.db 0
	.set link,*
	.db 6,"IMMED?"
IMMEDQ:
	sep colonpc
	.dw ONEMINUS,CFETCH,EXIT

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
	.dw link
	.db 0
	.set link,*
	.db 4,"FIND"
FIND:
	sep colonpc
	.dw LATEST,FETCH
FIND1:  .dw TWODUP,OVER,CFETCH,CHARPLUS
	.dw SEQUAL,DUP,qbranch,FIND2
	.dw DROP,NFATOLFA,FETCH,DUP
FIND2:  .dw ZEROEQUAL,qbranch,FIND1
	.dw DUP,qbranch,FIND3
	.dw NIP,DUP,NFATOCFA
	.dw SWAP,IMMEDQ,ZEROEQUAL,ONE,ORR
FIND3:  .dw EXIT

;C LITERAL  x --		append numeric literal
;   STATE @ IF COMPILE LIT , THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
	.dw link
	.db 1
	.set link,*
	.db 7,"LITERAL"
LITERAL:
	sep colonpc
	.dw STATE,FETCH,qbranch,LITER1
	.dw COMPILE,LIT,COMMA
LITER1: .dw EXIT

;Z DIGIT?   c -- n -1		if c is a valid digit
; 	    -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +	silly looking
;   DUP 140 > 107 AND -   30 -	but it works!
;   DUP BASE @ U< ;
	.dw link
	.db 0
	.set link,*
	.db 6,"DIGIT?"
DIGITQ:
	sep colonpc
	.dw DUP,LIT,H'39,GREATER,LIT,H'100,ANDD,PLUS
	.dw DUP,LIT,H'140,GREATER,LIT,H'107,ANDD
	.dw MINUS,LIT,H'30,MINUS
	.dw DUP,BASE,FETCH,ULESS,EXIT

;Z ?SIGN   adr n -- adr' n' f	get optional sign
;   advance adr/n if sign;	return NZ if negative
;   OVER C@			-- adr n c
;   2C - DUP ABS 1 = AND	-- +=-1, -=+1, else 0
;   DUP IF 1+			-- +=0, -=+2
;       >R 1 /STRING R>		-- adr' n' f
;   THEN ;
	.dw link
	.db 0
	.set link,*
	.db 5,"?SIGN"
QSIGN:
	sep colonpc
	.dw OVER,CFETCH,LIT,H'2C,MINUS,DUP,ABS
	.dw ONE,EQUAL,ANDD,DUP,qbranch,QSIGN1
	.dw ONEPLUS,TOR,ONE,SLASHSTRING,RFROM
QSIGN1: .dw EXIT

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
	.dw link
	.db 0
	.set link,*
	.db 7,">NUMBER"
TONUMBER:
	sep colonpc
TONUM1: .dw DUP,qbranch,TONUM3
	.dw OVER,CFETCH,DIGITQ
	.dw ZEROEQUAL,qbranch,TONUM2,DROP,EXIT
TONUM2: .dw TOR,TWOSWAP,BASE,FETCH,UDSTAR
	.dw RFROM,MPLUS,TWOSWAP
	.dw ONE,SLASHSTRING,branch,TONUM1
TONUM3: .dw EXIT

;Z ?NUMBER  c-addr -- n -1	string->number
; 		 -- c-addr 0	if convert error
;   DUP  0 0 ROT COUNT		-- ca ud adr n
;   ?SIGN >R  >NUMBER		-- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0	-- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1	-- n -1   (ok)
;   THEN ;
	.dw link
	.db 0
	.set link,*
	.db 7,"?NUMBER"
QNUMBER:
	sep colonpc
	.dw DUP,ZERO,DUP,ROT,COUNT
	.dw QSIGN,TOR,TONUMBER,qbranch,QNUM1
	.dw RFROM,TWODROP,TWODROP,ZERO
	.dw branch,QNUM3
QNUM1:  .dw TWODROP,NIP,RFROM,qbranch,QNUM2,NEGATE
QNUM2:  .dw MINUSONE
QNUM3:  .dw EXIT

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
	.dw link
	.db 0
	.set link,*
	.db 9,"INTERPRET"
INTERPRET:
	sep colonpc
	.dw TICKSOURCE,TWOSTORE,ZERO,TOIN,STORE
INTER1: .dw BL,WORD,DUP,CFETCH,qbranch,INTER9
	.dw FIND,QDUP,qbranch,INTER4
	.dw ONEPLUS,STATE,FETCH,ZEROEQUAL,ORR
	.dw qbranch,INTER2
	.dw EXECUTE,branch,INTER3
INTER2: .dw COMMAXT
INTER3: .dw branch,INTER1
INTER4: .dw QNUMBER,qbranch,INTER5
	.dw LITERAL,branch,INTER1
INTER5: .dw COUNT,TYPE,LIT,H'3F,EMIT,CR,ABORT

INTER9: .dw DROP,EXIT

;C EVALUATE  i*x c-addr u -- j*x	interpret string
;   'SOURCE 2@ >R >R  >IN @ >R
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! ;
	.dw link
	.db 0
	.set link,*
	.db 8,"EVALUATE"
EVALUATE:
	sep colonpc
	.dw TICKSOURCE,TWOFETCH,TOR,TOR
	.dw TOIN,FETCH,TOR,INTERPRET
	.dw RFROM,TOIN,STORE,RFROM,RFROM
	.dw TICKSOURCE,TWOSTORE,EXIT

;C QUIT     --    R: i*x --		interpret from kbd
;   L0 LP !  R0 RP!   0 STATE !
;   BEGIN
;       TIB DUP TIBSIZE ACCEPT
;       INTERPRET
;       STATE @ 0= IF CR ." OK" THEN
;   AGAIN ;
	.dw link
	.db 0
	.set link,*
	.db 4,"QUIT"
QUIT:
	sep colonpc
	.dw L0,LP,STORE
	.dw R0,RPSTORE,ZERO,STATE,STORE
QUIT1:  .dw TIB,DUP,TIBSIZE,ACCEPT
	.dw INTERPRET
	.dw STATE,FETCH,ZEROEQUAL,qbranch,QUIT2
	.dw CR,XSQUOTE
	.db 3,"ok "
	.dw TYPE
QUIT2:  .dw branch,QUIT1

;C ABORT    i*x --   R: j*x --	clear stk & QUIT
;   S0 SP!  QUIT ;
	.dw link
	.db 0
	.set link,*
	.db 5,"ABORT"
ABORT:
	sep colonpc
	.dw S0,SPSTORE,QUIT	; QUIT never returns

;Z ?ABORT   f c-addr u --	abort & print msg
;   ROT IF TYPE ABORT THEN 2DROP ;
	.dw link
	.db 0
	.set link,*
	.db 6,"?ABORT"
QABORT:
	sep colonpc
	.dw ROT,qbranch,QABO1,TYPE,ABORT
QABO1:  .dw TWODROP,EXIT

;C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
; 	 i*x x1 --       R: j*x --      x1<>0
;   POSTPONE S" POSTPONE ?ABORT ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 6,"ABORT\""
ABORTQUOTE:
	sep colonpc
	.dw SQUOTE
	.dw COMPILE,QABORT
	.dw EXIT

;C '    -- xt		find word in dictionary
;   FINDWORD DROP
	.dw link
	.db 0
	.set link,* 
	.db 1,"\'"
TICK:   sep colonpc
	.dw FINDWORD,DROP,EXIT

;C CHAR   -- char	parse ASCII character
;   BL WORD 1+ C@ ;
	.dw link
	.db 0
	.set link,*
	.db 4,"CHAR"
CHAR:
	sep colonpc
	.dw BL,WORD,ONEPLUS,CFETCH,EXIT

;C [CHAR]   --		compile character literal
;   CHAR COMPILE LIT  , ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 6,"[CHAR]"
BRACCHAR:
	sep colonpc
	.dw CHAR
	.dw COMPILE,LIT
	.dw COMMA,EXIT

;C (    --		skip input until )
;   [ HEX ] 29 PARSE 2DROP ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 1,"("
PAREN:
	sep colonpc
	.dw LIT,H'29,PARSE,TWODROP,EXIT

; COMPILER ======================================

;Z (CREATE)	-- 	create link, immediate, and name fields
;   LATEST @ , 0 C,	link & immed field
;   HERE LATEST !	new "latest" link
;   BL WORD C@ 1+ ALLOT	name field
	.dw link
	.db 0
	.set link,*
	.db 8,"(CREATE)"
XCREATE:
	sep colonpc
	.dw LATEST,FETCH,COMMA,ZERO,CCOMMA
	.dw HERE,LATEST,STORE
	.dw BL,WORD,CFETCH,ONEPLUS,ALLOT
	.dw EXIT

;C CREATE   --		create an empty definition with 3-byte code field
;   (CREATE) CFCOMPILE docreate COMPILE noop	code field
	.dw link
	.db 0
	.set link,*
	.db 6,"CREATE"
CREATE:
	sep colonpc
	.dw XCREATE,CFCOMPILE
	sep createpc
	.dw COMPILE,noop,EXIT	; default DOES> part

;Z (DOES>)  --	run-time action of DOES>
;   R>	      adrs of headless DOES> def'n
;   LATEST @ NFA>CFA 1+ !   code field to fix up
	.dw link
	.db 0
	.set link,*
	.db 7,"(DOES>)"
XDOES:
	sep colonpc
	.dw RFROM,LATEST,FETCH,NFATOCFA,ONEPLUS,STORE
	.dw EXIT

;C DOES>    --		change action of latest def'n
; ANSI 6.1.1250 says that DOES> only applies to CREATE'd 
; definitions, which have a 3-byte CFA
;   COMPILE (DOES>)
;   CFCOMPILE docolon ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 5,"DOES>"
DOES:
	sep colonpc
	.dw COMPILE,XDOES,CFCOMPILE
	sep colonpc
	.dw EXIT

;C RECURSE  --		recurse current definition
;   LATEST @ NFA>CFA ,XT ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 7,"RECURSE"
RECURSE:
	sep colonpc
	.dw LATEST,FETCH,NFATOCFA,COMMAXT,EXIT

;C [	--		enter interpretive state
;   0 STATE ! ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 1,"["
LEFTBRACKET:
	sep colonpc
	.dw ZERO,STATE,STORE,EXIT

;C ]	--		enter compiling state
;   -1 STATE ! ;
	.dw link
	.db 0
	.set link,*
	.db 1,"]"
RIGHTBRACKET:
	sep colonpc
	.dw MINUSONE,STATE,STORE,EXIT

;Z HIDE     --		"hide" latest definition
;   LATEST @ DUP C@ 80 OR SWAP C! ;
	.dw link
	.db 0
	.set link,*
	.db 4,"HIDE"
HIDE:
	sep colonpc
	.dw LATEST,FETCH,DUP,CFETCH,LIT,H'80,ORR
	.dw SWAP,CSTORE,EXIT

;Z REVEAL   --		"reveal" latest definition
;   LATEST @ DUP C@ 7F AND SWAP C! ;
	.dw link
	.db 0
	.set link,*
	.db 6,"REVEAL"
REVEAL:
	sep colonpc
	.dw LATEST,FETCH,DUP,CFETCH,LIT,H'7F,ANDD
	.dw SWAP,CSTORE,EXIT

;C IMMEDIATE   --	make last def'n immediate
;   1 LATEST @ 1- C! ;	set immediate flag
	.dw link
	.db 0
	.set link,*
	.db 9,"IMMEDIATE"
IMMEDIATE:
	sep colonpc
	.dw ONE,LATEST,FETCH,ONEMINUS,CSTORE
	.dw EXIT

;C :	--		begin a colon definition
;   CREATE HIDE ] CFCOMPILE docolon ;
	.dw link
	.db 0
	.set link,*
	.db 1,":"
COLON:
	sep colonpc
	.dw XCREATE,HIDE,RIGHTBRACKET,CFCOMPILE
	sep colonpc
	.dw EXIT

;C ;
;   REVEAL  ,EXIT
;   POSTPONE [  ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 1,";"
SEMICOLON:
	sep colonpc
	.dw REVEAL,CEXIT
	.dw LEFTBRACKET,EXIT

;C [']  --		find word & compile as literal
;   ' COMPILE LIT , ; IMMEDIATE
; When encountered in a colon definition, the
; phrase  ['] xxx  will cause   LIT,xxt  to be
; compiled into the colon definition (where
; (where xxt is the execution token of word xxx).
; When the colon definition executes, xxt will
; be put on the stack.  (All xt's are one cell.)
	.dw link
	.db 1
	.set link,*
	.db 3,"[\']"     ; tick character
BRACTICK: 
	sep colonpc
	.dw TICK		; get xt of 'xxx'
	.dw COMPILE,LIT		; append LIT action
	.dw COMMA,EXIT		; append xt literal

;C POSTPONE  --		postpone compile action of word
;   FINDWORD
;   0< IF   -- xt	non immed: add code to current
;			def'n to compile xt later.
;       COMPILE LIT ,	add "LIT,xt,COMMAXT"
;       COMPILE ,XT	to current definition
;   ELSE  ,XT      immed: compile into cur. def'n
;   THEN ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 8,"POSTPONE"
POSTPONE:
	sep colonpc
	.dw FINDWORD,ZEROLESS,qbranch,POST1
	.dw COMPILE,LIT,COMMA
	.dw COMPILE
POST1:  .dw COMMAXT,EXIT
	       
;Z COMPILE   --		append inline execution token
;   R> DUP CELL+ >R @ ,XT ;
; The phrase ['] xxx ,XT appears so often that
; this word was created to combine the actions
; of LIT and ,XT.  It takes an inline literal
; execution token and appends it to the dict.
	.dw link
	.db 0
	.set link,*
	.db 7,"COMPILE"
COMPILE:
	sep colonpc
	.dw RFROM,DUP,CELLPLUS,TOR
	.dw FETCH,COMMAXT,EXIT
    
; CONTROL STRUCTURES ============================

;C IF       -- adrs	conditional forward branch
;   ['] qbranch ,BRANCH HERE DUP ,DEST ;
;   IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 2,"IF"
IF:
	sep colonpc
	.dw LIT,qbranch,COMMABRANCH
	.dw HERE,DUP,COMMADEST,EXIT

;C THEN     adrs --	resolve forward branch
;   HERE SWAP !DEST ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 4,"THEN"
THEN:
	sep colonpc
	.dw HERE,SWAP,STOREDEST,EXIT

;C ELSE     adrs1 -- adrs2	branch for IF..ELSE
;   POSTPONE AHEAD SWAP POSTPONE THEN ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 4,"ELSE"
ELSE:
	sep colonpc
	.dw AHEAD
	.dw SWAP,THEN,EXIT

;C BEGIN    -- adrs		target for bwd. branch
;   HERE ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 5,"BEGIN"
BEGIN:
	lbr HERE

;C UNTIL    adrs --		conditional backward branch
;   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward branch
	.dw link
	.db 1
	.set link,*
	.db 5,"UNTIL"
UNTIL:
	sep colonpc
	.dw LIT,qbranch,COMMABRANCH
	.dw COMMADEST,EXIT

;X AGAIN    adrs --		uncond'l backward branch
;   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward branch
	.dw link
	.db 1
	.set link,*
	.db 5,"AGAIN"
AGAIN:
	sep colonpc
	.dw LIT,branch,COMMABRANCH
	.dw COMMADEST,EXIT

;C WHILE    -- adrs		branch for WHILE loop
;   POSTPONE IF SWAP ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 5,"WHILE"
WHILE:
	sep colonpc
	.dw IF,SWAP,EXIT

;C REPEAT   adrs1 adrs2 --	resolve WHILE loop
;   POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 6,"REPEAT"
REPEAT:
	sep colonpc
	.dw AGAIN,THEN,EXIT

;Z >L   x --   L: -- x		move to leave stack
;   CELL LP +!  LP @ ! ;	(L stack grows up)
	.dw link
	.db 0
	.set link,*
	.db 2,">L"
TOL:
	sep colonpc
	.dw CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

;Z L>   -- x   L: x --		move from leave stack
;   LP @ @  CELL NEGATE LP +! ;
	.dw link
	.db 0
	.set link,*
	.db 2,"L>"
LFROM:
	sep colonpc
	.dw LP,FETCH,FETCH
	.dw CELL,NEGATE,LP,PLUSSTORE,EXIT

;C DO       -- adrs   L: -- 0
;    0 	( no fwd branch )
;   BEGINLOOP ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 2,"DO"
DO:
	sep colonpc
	.dw ZERO	; no forward branch
	.dw BEGINLOOP,EXIT

;Z ENDLOOP   baddr faddr xt --   L: 0 a1 a2 .. aN --
;   ,BRANCH  ,DEST		backward loop
;   ?DUP IF			resolve forward
;        POSTPONE THEN		branch if any
;   THEN
;   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
;				resolve LEAVEs
; This is a common factor of LOOP and +LOOP.
	.dw link
	.db 0
	.set link,*
	.db 7,"ENDLOOP"
ENDLOOP:
	sep colonpc
	.dw COMMABRANCH,COMMADEST
	.dw QDUP,qbranch,LOOP1	; is there a fwd branch?
	.dw THEN		; resolve fwd branch
LOOP1:  .dw LFROM,QDUP,qbranch,LOOP2
	.dw THEN,branch,LOOP1	; resolve LEAVE branches
LOOP2:  .dw EXIT

;C LOOP    adrs --   L: 0 a1 a2 .. aN --
;   ['] xloop ENDLOOP ;  IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 4,"LOOP"
LOOP:
	sep colonpc
	.dw LIT,xloop,ENDLOOP,EXIT

;C +LOOP   adrs --   L: 0 a1 a2 .. aN --
;   ['] xplusloop ENDLOOP ;  IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 5,"+LOOP"
PLUSLOOP:
	sep colonpc
	.dw LIT,xplusloop,ENDLOOP,EXIT

;C LEAVE    --    L: -- adrs
;   COMPILE UNLOOP
;   ['] branch ,BRANCH   HERE DUP ,DEST  >L
;   ; IMMEDIATE      unconditional forward branch
	.dw link
	.db 1
	.set link,*
	.db 5,"LEAVE"
LEAVE:
	sep colonpc
	.dw COMPILE,unloop
	.dw LIT,branch,COMMABRANCH
	.dw HERE,DUP,COMMADEST,TOL,EXIT

; OTHER OPERATIONS ==============================

;X WITHIN   n1|u1 n2|u2 n3|u3 -- f	n2<=n1<n3?
;  OVER - >R - R> U< ;			per ANS document
	.dw link
	.db 0
	.set link,*
	.db 6,"WITHIN"
WITHIN:
	sep colonpc
	.dw OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

;C MOVE    addr1 addr2 u --		smart move
;	     VERSION FOR 1 ADDRESS UNIT = 1 CHAR
;  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
;  WITHIN IF  R> CMOVE>	src <= dst < src+n
;       ELSE  R> CMOVE  THEN ;	  otherwise
	.dw link
	.db 0
	.set link,*
	.db 4,"MOVE"
MOVE:
	sep colonpc
	.dw TOR,TWODUP,SWAP,DUP,RFETCH,PLUS
	.dw WITHIN,qbranch,MOVE1
	.dw RFROM,CMOVEUP,branch,MOVE2
MOVE1:  .dw RFROM,CMOVE
MOVE2:  .dw EXIT

;C DEPTH    -- +n		number of items on stack
;   SP@ S0 SWAP - 2/ ;		16-BIT VERSION!
	.dw link
	.db 0
	.set link,*
	.db 5,"DEPTH"
DEPTH:
	sep colonpc
	.dw SPFETCH,S0,SWAP,MINUS,TWOSLASH,EXIT

;C ENVIRONMENT?  c-addr u -- false	system query
;		 -- i*x true
;   2DROP 0 ;			the minimal definition!
	.dw link
	.db 0
	.set link,*
	.db 12,"ENVIRONMENT?"
ENVIRONMENTQ:
	sep colonpc
	.dw TWODROP,ZERO,EXIT

; UTILITY WORDS AND STARTUP =====================

;T WORDS    --			list all words in dict.
;   LATEST @ BEGIN
;       DUP COUNT TYPE SPACE
;       NFA>LFA @
;   DUP 0= UNTIL
;   DROP ;
	.dw link
	.db 0
	.set link,*
	.db 5,"WORDS"
WORDS:
	sep colonpc
	.dw LATEST,FETCH
WDS1:   .dw DUP,COUNT,TYPE,SPACE,NFATOLFA,FETCH
	.dw DUP,ZEROEQUAL,qbranch,WDS1
	.dw DROP,EXIT

;T .S      --			print stack contents
;   SP@ S0 - IF
;       SP@ S0 2 - DO I @ U. -2 +LOOP
;   THEN ;
	.dw link
	.db 0
	.set link,*
	.db 2,".S"
DOTS:
	sep colonpc
	.dw SPFETCH,S0,MINUS,qbranch,DOTS2
	.dw SPFETCH,S0,LIT,2,MINUS,XDO
DOTS1:  .dw II,FETCH
	.dw swapbytes		; parameter stack data is little-endian
	.dw UDOT,LIT,-2,XPLUSLOOP,DOTS1
DOTS2:  .dw EXIT

;Z TRUECOLD -- Hidden word invoked at start
; holding IN when pressing enter forces a cold start
; GENBAUD EF4? IF COLD THEN 45296 MAGIC @ = IF WARM THEN COLD ;
TRUECOLD:
	sep colonpc
	.dw GENBAUD,EF4Q,QBRANCH,TRUECO1D,COLD
TRUECO1D: ;; PseudoSam only allows 8 letter labels?!
	.dw LIT,28912,MAGIC,FETCH,EQUAL,QBRANCH,COLD1,WARM

;Z COLD     --			cold start Forth system
;   UINIT U0 #INIT CMOVE WARM  init user area
	.dw link
	.db 0
	.set link,*
	.db 4,"COLD"
COLD:
	sep colonpc
COLD1:
	.dw UINIT,U0,NINIT,CMOVE,WARM
;Z WARM		--		warm start Forth system
;   ." RCA1802 CamelForth etc."
;   FREE . ." Dictionary Bytes free" ABORT ;
	.dw link
	.db 0
	.set link,*
	.db 4,"WARM"
WARM:
	sep colonpc
WARM1:
	.dw XSQUOTE
	.db 56			; length of sign-on string
	.db "RCA1802 CamelForth v1.1 - MC Edition v0.1  20 Jul 2024"
	.db H'0D,H'0A
	.dw TYPE
	.dw UNUSED,UDOT,XSQUOTE
	.db 23
	.db "Dictionary Bytes free"
	.db H'0D,H'0A
	.dw TYPE,ABORT		; ABORT never returns

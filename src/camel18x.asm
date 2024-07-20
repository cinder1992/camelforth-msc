; LISTING 4.
;
; ===============================================
; CamelForth for the RCA 1802
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
;
; ===============================================
; CAMEL18X.ASM: Additional High-Level Words
;   not included in the original CamelForth
;   Source code is for the A180 assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
; ===============================================

; PARSING WORDS ===============================

;X PARSE        ( char "ccc<char>" -- c-addr u ) 
;  SOURCE    ( c addr u ) 
;  >IN @ /STRING    ( c addr u ) 
;  OVER >R    ( c addr u R: addr ) 
;  DUP >R    ( c addr u R: addr u ) 
;  ROT        ( addr u c ) 
;  SCAN NIP DUP    ( u' u' ) 
;  R> SWAP -    ( u' len R: addr ) 
;  DUP ROT    ( len len u' ) 
;  IF CHAR+ THEN     ( skip trailing delim ) 
;  >IN +!    ( update >IN ) 
;  R> SWAP    ( addr len ) 
; 
	.dw link
	.db 0
	.set link,*
	.db 5,"PARSE"
PARSE:
	sep colonpc
	.dw SOURCE,TOIN,FETCH,SLASHSTRING
	.dw OVER,TOR,DUP,TOR,ROT
	.dw SCAN,NIP,DUP
	.dw RFROM,SWAP,MINUS
	.dw DUP,ROT,qbranch,PARSE1
	.dw CHARPLUS
PARSE1:
	.dw TOIN,PLUSSTORE
	.dw RFROM,SWAP,EXIT

;X .(	--			print to matching right paren
; [ HEX ] 29 PARSE TYPE ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 2,".("
DOTPAREN:
	sep colonpc
	.dw LIT,H'29,PARSE,TYPE,EXIT

;Z (C")     -- c-addr		run-time code for C"
;   R> DUP COUNT + ALIGNED >R  ;
	.dw link
	.db 0
	.set link,*
	.db 4,"(C\")"
XCQUOTE:
	sep colonpc
	.dw RFROM,DUP,COUNT,PLUS,ALIGNED,TOR
	.dw EXIT

;X C"       --		compile in-line string
;  COMPILE (C") 
;  [CHAR] " PARSE    ( c-addr u ) 
;  CCSTR
; IMMEDIATE 
	.dw link
	.db 1
	.set link,*
	.db 2,"C\""
CQUOTE:
	sep colonpc
	.dw COMPILE,XCQUOTE
	.dw LIT,H'22,PARSE
	.dw CCSTR,EXIT

;Z CCSTR  c-addr u -- compile to counted string
;  DUP >R        ( c-addr u ) 
;  HERE >COUNTED        ( copy to dict ) 
;  R> CHAR+ ALLOT ALIGN    ( bump DP ) 
	.dw link
	.db 0
	.set link,*
	.db 5,"CCSTR"
CCSTR:
	sep colonpc
	.dw DUP,TOR,HERE,TOCOUNTED
	.dw RFROM,CHARPLUS
	.dw ALLOT,ALIGN
	.dw EXIT

;S SLITERAL c-addr u --  compile time
;           -- c-addr u  run time
; COMPILE (S") CCSTR
; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 8,"SLITERAL"
SLITERAL:
	sep colonpc
	.dw COMPILE,XSQUOTE
	.dw CCSTR,EXIT

;X \	--			comment to end of line
; \ 0 PARSE 2DROP ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 1,"\\"
BACKSLASH:
	sep colonpc
	.dw ZERO,PARSE,TWODROP,EXIT

;S -TRAILING ( c-addr u1 -- c-addr u2 )
;  BEGIN
;    DUP 0= IF EXIT THEN
;    2DUP + 1- C@
;    BL = 0= IF EXIT THEN
;    1-
;  AGAIN
	.dw link
	.db 0
	.set link,*
	.db 9,"-TRAILING"
DTRAIL:
	sep colonpc
DTRAIL1:
	.dw DUP,ZEROEQUAL,qbranch,DTRAIL2,EXIT
DTRAIL2:
	.dw TWODUP,PLUS,ONEMINUS,CFETCH
	.dw BL,EQUAL,ZEROEQUAL,qbranch,DTRAIL3,EXIT
DTRAIL3:
	.dw ONEMINUS,branch,DTRAIL1

;S COMPARE ( a1 u1 a2 u2 -- n )
;  ROT 2DUP  ( a1 a2 u2 u1 u2 u1 )
;  SWAP - >R UMIN ( a1 a2 minu R: u1-u2 )
;  S= DUP    ( S1-S2 S1-S2 )
;  IF R> DROP ( S1-S2 )
;  ELSE DROP R> ( u1-u2 )
;  THEN
;  DUP IF 
;    0< IF -1 ELSE 1 THEN
;  THEN
	.dw link
	.db 0
	.set link,*
	.db 7,"COMPARE"
COMPARE:
	sep colonpc
	.dw ROT,TWODUP
	.dw SWAP,MINUS,TOR,UMIN
	.dw SEQUAL,DUP
	.dw qbranch,COMP1
	.dw RFROM,DROP,branch,COMP2
COMP1:
	.dw DROP,RFROM
COMP2:
	.dw DUP,qbranch,COMP3
	.dw ZEROLESS,qbranch,COMP4
	.dw MINUSONE,branch,COMP3
COMP4:
	.dw ONE
COMP3:	.dw EXIT

;S SEARCH          ( c-addr1 u1 c-addr2 u2 )
;  2OVER 2>R       ( c-addr1 u1 c-addr2 u2 R: c-addr1 u1 )
;  2DUP  2>R       ( c-addr1 u1 c-addr2 u2 R: c-addr1 u1 c-addr2 u2 )
;  BEGIN           ( c-addr1 u1 c-addr2 u2 )  
;    2 PICK OVER   ( c-addr1 u1 c-addr2 u2 u1 u2 )
;    U< IF         ( c-addr1 u1 c-addr2 u2 )
;      2DROP 2DROP ( clean Pstack )
;      2R>   2DROP ( clean Rstack )
;      2R>         ( c-addr1 u1 )
;      FALSE EXIT  ( c-addr1 u1 F )
;    THEN          ( c-addr1 u1 c-addr2 u2 )
;    3 PICK SWAP   ( c-addr1 u1 c-addr2 c-addr1 u2 )      
;    S=            ( c-addr1 u1 f )
;    0= IF         ( c-addr1 u1 )
;      2R> 2DROP 2R> 2DROP  ( clean Rstack )
;      TRUE EXIT   ( c-addr1 u1 T )
;    THEN
;    1 /STRING     ( c-addr1' u1' ) 
;    2R@           ( c-addr1' u1' c-addr2 u2 )  
;  AGAIN
	.dw link
	.db 0
	.set link,*
	.db 6,"SEARCH"
SEARCH:
	sep colonpc
	.dw TWOOVER,TWOTOR,TWODUP,TWOTOR
SEAR1:	.dw LIT,2,PICK,OVER,ULESS,QBRANCH,SEAR2
	.dw TWODROP,TWODROP,TWORFROM,TWODROP,TWORFROM 
	.dw FALSE,EXIT 
SEAR2:	.dw LIT,3,PICK,SWAP,SEQUAL,ZEROEQUAL,QBRANCH,SEAR3
	.dw TWORFROM,TWODROP,TWORFROM,TWODROP
	.dw TRUE,EXIT
SEAR3:	.dw ONE,SLASHSTRING
	.dw TWORFETCH,BRANCH,SEAR1

;S BLANK addr --  fill memory with space characters
; BL FILL
		.dw link
	.db 0
	.set link,*
	.db 5,"BLANK"
BLANK:
	sep colonpc
	.dw BL,FILL,EXIT

;X ERASE addr --  fill memory with zeroes
; ZERO FILL
	.dw link
	.db 0
	.set link,*
	.db 5,"ERASE"
ERASE:
	sep colonpc
	.dw ZERO,FILL,EXIT

; PRINTING WORDS =========================================

;Z S.RJ   ( w addr u -- print string from PAD right justified )
;  ROT OVER -   ( addr u n )
;  DUP 0> IF 
;    0 DO BL HOLD  ( addr u )
;      -1 /STRING   ( addr' u' )
;    LOOP
;  ELSE
;    DROP
;  THEN TYPE
	.dw link
	.db 0
	.set link,*
	.db 4,"S.RJ"
SDOTRJ:
	sep colonpc
	.dw ROT,OVER,MINUS
	.dw DUP,ZEROGREATER,QBRANCH,SDRJ1
	.dw ZERO,XDO
SDRJ2:	.dw BL,HOLD,MINUSONE,SLASHSTRING
	.dw XLOOP,SDRJ2,BRANCH,SDRJ3
SDRJ1:	.dw DROP
SDRJ3:	.dw TYPE,EXIT

;X U.R  ( u width --  print unsigned right justified )  
;  SWAP 
;  <# 0 #S #>  ( width addr u )
;  S.RJ
	.dw link
	.db 0
	.set link,*
	.db 3,"U.R"
UDOTR:
	sep colonpc
	.dw SWAP,LESSNUM,ZERO,NUMS,NUMGREATER
	.dw SDOTRJ,EXIT

;X .R  ( n width -- print signed right justified)
;  SWAP  ( w n )
;  <# DUP ABS 0 #S ROT SIGN #> ( w addr u )
;  S.RJ
;
	.dw link
	.db 0
	.set link,*
	.db 2,".R"
DOTR:
	sep colonpc
	.dw SWAP,LESSNUM,DUP,ABS,ZERO
	.dw NUMS,ROT,SIGN,NUMGREATER
	.dw SDOTRJ,EXIT

;Z .BYTE ( u -- print byte in hex )
;  DUP 4 RSHIFT [ HEX ] 0F AND >DIGIT EMIT
;  0F AND >DIGIT EMIT
;
	.dw link
	.db 0
	.set link,*
	.db 5,".BYTE"
DOTBYTE:
	sep colonpc
	.dw DUP,LIT,4,RSHIFT
	.dw LIT,H'0F,ANDD,TODIGIT,EMIT
	.dw LIT,H'0F,ANDD,TODIGIT,EMIT
	.dw EXIT

;Z .ADDR ( u --  print address in hex )  
;  DUP >< .BYTE .BYTE 
;
	.dw link
	.db 0
	.set link,*
	.db 5,".ADDR"
DOTADDR:
	sep colonpc
	.dw DUP,SWAPBYTES,DOTBYTE
	.dw DOTBYTE,EXIT

;T DUMP ( addr u -- )  print memory contents
;  OVER .ADDR  ( print starting address )
;  BEGIN  ( addr u )
;    ?DUP WHILE ( are we done? )
;    OVER C@ SPACE .BYTE ( print one byte )
;    1- SWAP 1+   ( u' addr' )
;    DUP [ HEX ] 0F AND 0= IF  ( next line? )
;      CR DUP .ADDR
;    THEN SWAP  ( addr' u' )
;  REPEAT
;  DROP
	.dw link
	.db 0
	.set link,*
	.db 4,"DUMP"
DUMP:
	sep colonpc
	.dw OVER,DOTADDR
DUMP1:	.dw QDUP,QBRANCH,DUMP2
	.dw OVER,CFETCH,SPACE,DOTBYTE
	.dw ONEMINUS,SWAP,ONEPLUS
	.dw DUP,LIT,H'0F,ANDD
	.dw ZEROEQUAL,QBRANCH,DUMP3
	.dw CR,DUP,DOTADDR
DUMP3:	.dw SWAP,BRANCH,DUMP1
DUMP2:	.dw DROP,EXIT

; DEFINING WORDS ===========================================

;X :NONAME	-- xt	begin a nameless colon definition
; LATEST @ , 0 C,	link & immed field (in case of RECURSE)
; HERE LATEST !
; 0 C,			null name field
; HERE ] COMPILE docolon;
	.dw link
	.db 0
	.set link,*
	.db 7,":NONAME"
NONAME:
	sep colonpc
	.dw LATEST,FETCH,COMMA,ZERO,CCOMMA
	.dw HERE,LATEST,STORE
	.dw ZERO,CCOMMA
	.dw HERE,RIGHTBRACKET
	.dw CFCOMPILE
	sep colonpc
	.dw EXIT

;X VALUE	n --	create value object
; VALUE CONSTANT ;
	.dw link
	.db 0
	.set link,*
	.db 5,"VALUE"
VALUE:
	lbr CONSTANT

;Z CFCOMPILE     --       append a 1-byte code field
; It takes a 1-byte inline literal opcode and appends it
; to the dictionary
	.dw link
	.db 0
	.set link,*
	.db 9,"CFCOMPILE"
CFCOMPILE:
	sep colonpc
	.dw RFROM,DUP,ONEPLUS,TOR
	.dw CFETCH,CCOMMA,EXIT

;Z FINDWORD	 ( "ccc" -- xt f  1 = immediate, -1 = regular)
; BL WORD FIND
; ?DUP IF EXIT THEN
; COUNT TYPE [CHAR] ? EMIT ABORT
	.dw link
	.db 0
	.set link,*
	.db 8,"FINDWORD"
FINDWORD:
	sep colonpc
	.dw BL,WORD,FIND
	.dw QDUP,qbranch,FW1
	.dw EXIT
FW1:	.dw COUNT,TYPE
	.dw LIT,H'3F,EMIT,CR,ABORT

;X TO		n --	store in value object
; ' 1+		( skip code field )
; STATE @ IF !
;   COMPILE lit , COMPILE !
; ELSE
;   !
; THEN ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 2,"TO"
TO:
	sep colonpc
	.dw TICK,ONEPLUS
	.dw STATE,FETCH,qbranch,TO1
	.dw COMPILE,LIT		; compiling
	.dw COMMA
	.dw COMPILE
TO1:				; interpreting
	.dw STORE,EXIT

;X [COMPILE] Compilation: ( "<spaces>name" -- )
;  FINDWORD  ( -- xt f )
;  DROP  COMPILE,	( append semantics )
; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 9,"[COMPILE]"
BRACCOMP:
	sep colonpc
	.dw FINDWORD,DROP
	.dw COMMAXT,EXIT

;Z CREATE1   --		create an empty definition with 1-byte code field
;   (CREATE) CFCOMPILE dovar
	.dw link
	.db 0
	.set link,*
	.db 7,"CREATE1"
CREATE1:
	sep colonpc
	.dw XCREATE,CFCOMPILE
	sep varpc
	.dw EXIT

;X CONVERT ud1 c-addr2 -- ud2 c-addr2
; CHAR+ -1 >NUMBER DROP ;
	.dw link
	.db 0
	.set link,*
	.db 7,"CONVERT"
CONVERT:
	sep colonpc
	.dw CHARPLUS,MINUSONE,TONUMBER
	.dw DROP,EXIT

;X MARKER  ( "<spaces>name" -- create marker for forgetting
;  CREATE LATEST @ NFA>LFA , 
;  DOES> @ DUP DP ! 
;   @ LATEST !
;
	.dw link
	.db 0
	.set link,*
	.db 6,"MARKER"
MARKER:
	sep colonpc
	.dw XCREATE,CFCOMPILE
	sep createpc
	.dw COMPILE,MARK1
	.dw LATEST,FETCH,NFATOLFA,COMMA
	.dw EXIT
MARK1:	sep colonpc
	.dw FETCH,DUP,DP,STORE
	.dw FETCH,LATEST,STORE
	.dw EXIT

; MISCELLANEOUS WORDS =============================

;X UNUSED  ( -- u get size of unused dictionary )
;  SP@ HERE -   ( Param stack grows towards dictionary )
;
	.dw link
	.db 0
	.set link,*
	.db 6,"UNUSED"
UNUSED:
	sep colonpc
	.dw SPFETCH,HERE,MINUS,EXIT

;X 0<>     n/u -- flag    return true if TOS<>0
	.dw link
	.db 0
	.set link,*
	.db 3,"0<>"
ZERONOTEQ:
	lda psp
	or
	inc psp
	lbnz TRUE
	lbr FALSE

;X 0>     n/u -- flag    return true if TOS>0
	.dw link
	.db 0
	.set link,*
	.db 2,"0>"
ZEROGREATER:
	sep colonpc
	.dw ZERO,SWAP,LESS,EXIT

; CONTROL FLOW WORDS ===================================

;X ?DO       -- adrs   L: -- 0
;  ['] 2DUP COMPILE,
;  ['] = COMPILE,            ( index = limit? )
;  POSTPONE IF            
;    ['] 2DROP COMPILE, 
;  POSTPONE ELSE   ( fwd branch )
;  BEGINLOOP ; IMMEDIATE
	.dw link
	.db 1
        .set link,*
	.db 3,"?DO"
QDO:
	sep colonpc
	.dw COMPILE,TWODUP
	.dw COMPILE,EQUAL
	.dw IF,COMPILE,TWODROP
	.dw ELSE,BEGINLOOP,EXIT

;Z BEGINLOOP	 ( common factor of DO and ?DO )
;  ['] (do) COMPILE,         ( compile runtime action )
;  HERE             ( bwd branch target )
;  0 >L                ( marker for LEAVEs )
	.dw link
	.db 0
	.set link,*
	.db 9,"BEGINLOOP"
BEGINLOOP:
	sep colonpc
	.dw COMPILE,xdo
	.dw HERE	; target for bwd branch
	.dw ZERO,TOL,EXIT

;T AHEAD
;   ['] branch ,BRANCH HERE DUP ,DEST ; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 5,"AHEAD"
AHEAD:         			; -- adrs
	sep colonpc
	.dw LIT,branch,COMMABRANCH
	.dw HERE,DUP,COMMADEST,EXIT

;X CASE
; 0 CONSTANT CASE IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 4,"CASE"
CASE:
	sep constpc
	.dw 0

;X OF  ( #of -- orig #of+1 / x -- )
;    1+    ( count OFs )
;    >R    ( move off the stack in case the control-flow )
;          ( stack is the data stack. )
;    POSTPONE OVER  POSTPONE = ( copy and test case value)
;    POSTPONE IF    ( add orig to control flow stack )
;    POSTPONE DROP  ( discards case value if = )
;    R>             ( we can bring count back now )
; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 2,"OF"
OF:
	sep colonpc
	.dw ONEPLUS,TOR,COMPILE,OVER,COMPILE,EQUAL
	.dw IF,COMPILE,DROP,RFROM,EXIT

;X ENDOF ( orig1 #of -- orig2 #of )
;    >R   ( move off the stack in case the control-flow )
;         ( stack is the data stack. )
;    POSTPONE ELSE
;    R>   ( we can bring count back now )
; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 5,"ENDOF"
ENDOF:
	sep colonpc
	.dw TOR,ELSE,RFROM,EXIT

;X ENDCASE  ( orig1..orign #of -- )
;    POSTPONE DROP  ( discard case value )
;    ?DUP IF
;        0 DO
;          POSTPONE THEN
;       LOOP
;    THEN
; IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 7,"ENDCASE"
ENDCASE:
	sep colonpc
	.dw COMPILE,DROP,QDUP,qbranch,ENDC1
	.dw ZERO,xdo
ENDC2:	.dw THEN,xloop,ENDC2
ENDC1:	.dw EXIT

; INPUT SOURCE WORDS ===================================

;X SOURCE-ID  ( -- flag )
; 'SOURCE CELL+ @ TIB = 0=	; compare current source 
	.dw link		; to Terminal Input Buffer
	.db 0
	.set link,*
	.db 9,"SOURCE-ID"
SOURCEID:
	sep colonpc
	.dw TICKSOURCE,CELLPLUS,FETCH,TIB
	.dw EQUAL,ZEROEQUAL,EXIT

;X REFILL		( -- flag )
; SOURCE-ID IF  ( string source )
;   FALSE
; ELSE          ( terminal source )
;   TIB #TIB ACCEPT
;   'SOURCE !  ( save char count )
;   0 >IN ! TRUE
; THEN
	.dw link
	.db 0
	.set link,*
	.db 6,"REFILL"
REFILL:
	sep colonpc
	.dw SOURCEID,QBRANCH,REF1
	.dw FALSE,EXIT
REF1:
	.dw TIB,TIBSIZE,ACCEPT,TICKSOURCE,STORE
	.dw ZERO,TOIN,STORE,TRUE,EXIT

;T [ELSE]  ( -- )
;    1 BEGIN                               \ level
;      BEGIN
;        BL WORD COUNT  DUP  WHILE         \ level adr len
;        2DUP  S" [IF]"  COMPARE 0=
;        IF                                \ level adr len
;          2DROP 1+                        \ level'
;        ELSE                              \ level adr len
;          2DUP  S" [ELSE]"
;          COMPARE 0= IF                   \ level adr len
;             2DROP 1- DUP IF 1+ THEN      \ level'
;          ELSE                            \ level adr len
;            S" [THEN]"  COMPARE 0= IF     \ level
;              1-                          \ level'
;            THEN
;          THEN
;        THEN ?DUP 0=  IF EXIT THEN        \ level'
;      REPEAT  2DROP                       \ level
;    REFILL 0= UNTIL                       \ level
;    DROP
;  IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 6,"[ELSE]"
BRACELSE:
	sep colonpc
	.dw ONE
BREL1:	.dw BL,WORD,COUNT,DUP,QBRANCH,BREL6
	.dw TWODUP,XSQUOTE
	.db 4,"[IF]"
        .dw COMPARE,ZEROEQUAL,QBRANCH,BREL2
	.dw TWODROP,ONEPLUS
	.dw BRANCH,BREL3
BREL2:	.dw TWODUP,XSQUOTE
	.db 6,"[ELSE]"
        .dw COMPARE,ZEROEQUAL,QBRANCH,BREL4
        .dw TWODROP,ONEMINUS,DUP,QBRANCH,BREL3
        .dw ONEPLUS,BRANCH,BREL3
BREL4:	.dw XSQUOTE
	.db 6,"[THEN]"
        .dw COMPARE,ZEROEQUAL,QBRANCH,BREL3
 	.dw ONEMINUS
BREL3:	.dw QDUP,ZEROEQUAL,QBRANCH,BREL5
	.dw EXIT
BREL5:	.dw BRANCH,BREL1
BREL6:	.dw TWODROP,REFILL,ZEROEQUAL,QBRANCH,BREL1
        .dw DROP,EXIT

;T [IF]  ( flag -- )
; 0= IF POSTPONE [ELSE] THEN ;  IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 4,"[IF]"
BRACIF:
	sep colonpc
 	.dw ZEROEQUAL,QBRANCH,BRIF1
	.dw BRACELSE
BRIF1:	.dw EXIT

;T [THEN]  ( -- )  ;  IMMEDIATE
	.dw link
	.db 1
	.set link,*
	.db 6,"[THEN]"
BRACTHEN:
	sep nextpc

;X SAVE-INPUT	( -- save-spec n )
; >IN @ 1	( for keyboard and EVALUATE )
	.dw link
	.db 0
	.set link,*
	.db 10,"SAVE-INPUT"
SAVEIN:
	sep colonpc
	.dw TOIN,FETCH,ONE,EXIT

;X RESTORE-INPUT ( save-spec n -- f )
; 1 = IF >IN ! FALSE ELSE TRUE THEN
	.dw link
	.db 0
	.set link,*
	.db 13,"RESTORE-INPUT"
RESTIN:
	sep colonpc
	.dw ONE,EQUAL,QBRANCH,REST1
	.dw TOIN,STORE
	.dw FALSE,EXIT
REST1:	.dw TRUE,EXIT

; STACK MANIPULATION WORDS =======================

;X 2>R	 x y --  R: -- x y	move two to return stack
; R> ROT >R SWAP >R >R
	.dw link
	.db 0
	.set link,*
	.db 3,"2>R"
TWOTOR:
	sep colonpc
	.dw RFROM,ROT,TOR
	.dw SWAP,TOR,TOR
	.dw EXIT

;X 2R>	-- x y R: x y --	move two from return stack
; R> R> R> ROT >R SWAP
	.dw link
	.db 0
	.set link,*
	.db 3,"2R>"
TWORFROM:
	sep colonpc
	.dw RFROM,RFROM,RFROM
	.dw ROT,TOR,SWAP
	.dw EXIT

;X 2R@  -- x y			copy two from return stack
; R> 2R> 2DUP 2>R ROT >R 
	.dw link
	.db 0
	.set link,*
	.db 3,"2R@"
TWORFETCH:
	sep colonpc
	.dw RFROM,TWORFROM,TWODUP
	.dw TWOTOR,ROT,TOR,EXIT	

;T CS-PICK		control stack PICK
	.dw link
	.db 0
	.set link,*
	.db 7,"CS-PICK"
CSPICK:
	br PICK		; same as data stack PICK

;X PICK n -- x		pick n'th entry from stack
; CELLS SP@ + CELL+ @ >< ;	data stack is little-endian
	.dw link
	.db 0
	.set link,*
	.db 4,"PICK"
PICK:
	sep colonpc
	.dw CELLS,SPFETCH,PLUS
	.dw CELLPLUS,FETCH,SWAPBYTES
	.dw EXIT

;T CS-ROLL		control stack ROLL
	.dw link
	.db 0
	.set link,*
	.db 7,"CS-ROLL"
CSROLL:
	br ROLL		; same as data stack ROLL

;X ROLL	( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
;  DUP >R PICK		 get entry to be rolled
;  SP@ DUP CELL+	 get src and dest of move
;  R> CELLS CELL+ CMOVE> DROP
	.dw link
	.db 0
	.set link,*
	.db 4,"ROLL"
ROLL:
	sep colonpc
	.dw DUP,TOR,PICK
	.dw SPFETCH,DUP,CELLPLUS
	.dw RFROM,CELLS,CELLPLUS
	.dw CMOVEUP,DROP,EXIT

; EPILOGUE =========================

	.equ lastword,link	; nfa of last word in dictionary
	.equ enddict,*		; user's code starts here

	.end




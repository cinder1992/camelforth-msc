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
;   Source code is for the A18 assembler.
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
;  scan NIP DUP    ( u' u' ) 
;  R> SWAP -    ( u' len R: addr ) 
;  DUP ROT    ( len len u' ) 
;  IF CHAR+ THEN     ( skip trailing delim ) 
;  >IN +!    ( update >IN ) 
;  R> SWAP    ( addr len ) 
; 
	DW link
	DB 0
link SET $
	DB 5,"PARSE"
PARSE
	sep colonpc
	DW SOURCE,TOIN,FETCH,SLASHSTRING
	DW OVER,TOR,DUP,TOR,ROT
	DW scan,NIP,DUP
	DW RFROM,SWAP,MINUS
	DW DUP,ROT,qbranch,PARSE1
	DW CHARPLUS
PARSE1
	DW TOIN,PLUSSTORE
	DW RFROM,SWAP,EXIT

;X .(	--			print to matching right paren
; [ HEX ] 29 PARSE TYPE ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 2,".("
DOTPAREN
	sep colonpc
	DW LIT,$29,PARSE,TYPE,EXIT

;Z (C")     -- c-addr		run-time code for C"
;   R> DUP COUNT + ALIGNED >R  ;
	DW link
	DB 0
link SET $
	DB 4,"(C\")"
XCQUOTE
	sep colonpc
	DW RFROM,DUP,COUNT,PLUS,ALIGNED,TOR
	DW EXIT

;X C"       --		compile in-line string
;  COMPILE (C") 
;  [CHAR] " PARSE    ( c-addr u ) 
;  CCSTR
; IMMEDIATE 
	DW link
	DB 1
link SET $
	DB 2,"C\""
CQUOTE
	sep colonpc
	DW COMPILE,XCQUOTE
	DW LIT,$22,PARSE
	DW CCSTR,EXIT

;Z CCSTR  c-addr u -- compile to counted string
;  DUP >R        ( c-addr u ) 
;  HERE >COUNTED        ( copy to dict ) 
;  R> CHAR+ ALLOT ALIGN    ( bump DP ) 
	DW link
	DB 0
link SET $
	DB 5,"CCSTR"
CCSTR
	sep colonpc
	DW DUP,TOR,HERE,TOCOUNTED
	DW RFROM,CHARPLUS
	DW ALLOT,ALIGN
	DW EXIT

;S SLITERAL c-addr u --  compile time
;           -- c-addr u  run time
; COMPILE (S") CCSTR
; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 8,"SLITERAL"
SLITERAL
	sep colonpc
	DW COMPILE,XSQUOTE
	DW CCSTR,EXIT

;X \	--			comment to end of line
; \ 0 PARSE 2DROP ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 1,"\\"
BACKSLASH
	sep colonpc
	DW ZERO,PARSE,TWODROP,EXIT

;S -TRAILING ( c-addr u1 -- c-addr u2 )
;  BEGIN
;    DUP 0= IF EXIT THEN
;    2DUP + 1- C@
;    BL = 0= IF EXIT THEN
;    1-
;  AGAIN
	DW link
	DB 0
link SET $
	DB 9,"-TRAILING"
DTRAIL
	sep colonpc
DTRAIL1
	DW DUP,ZEROEQUAL,qbranch,DTRAIL2,EXIT
DTRAIL2
	DW TWODUP,PLUS,ONEMINUS,CFETCH
	DW BL,EQUAL,ZEROEQUAL,qbranch,DTRAIL3,EXIT
DTRAIL3
	DW ONEMINUS,branch,DTRAIL1

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
	DW link
	DB 0
link SET $
	DB 7,"COMPARE"
COMPARE
	sep colonpc
	DW ROT,TWODUP
	DW SWAP,MINUS,TOR,UMIN
	DW SEQUAL,DUP
	DW qbranch,COMP1
	DW RFROM,DROP,branch,COMP2
COMP1
	DW DROP,RFROM
COMP2
	DW DUP,qbranch,COMP3
	DW ZEROLESS,qbranch,COMP4
	DW MINUSONE,branch,COMP3
COMP4
	DW ONE
COMP3	DW EXIT

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
	DW link
	DB 0
link SET $
	DB 6,"SEARCH"
SEARCH
	sep colonpc
	DW TWOOVER,TWOTOR,TWODUP,TWOTOR
SEAR1	DW LIT,2,PICK,OVER,ULESS,qbranch,SEAR2
	DW TWODROP,TWODROP,TWORFROM,TWODROP,TWORFROM 
	DW FALSE,EXIT 
SEAR2	DW LIT,3,PICK,SWAP,SEQUAL,ZEROEQUAL,qbranch,SEAR3
	DW TWORFROM,TWODROP,TWORFROM,TWODROP
	DW TRUE,EXIT
SEAR3	DW ONE,SLASHSTRING
	DW TWORFETCH,branch,SEAR1

;S BLANK addr --  fill memory with space characters
; BL FILL
		DW link
	DB 0
link SET $
	DB 5,"BLANK"
BLANK
	sep colonpc
	DW BL,FILL,EXIT

;X ERASE addr --  fill memory with zeroes
; ZERO FILL
	DW link
	DB 0
link SET $
	DB 5,"ERASE"
ERASE
	sep colonpc
	DW ZERO,FILL,EXIT

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
	DW link
	DB 0
link SET $
	DB 4,"S.RJ"
SDOTRJ
	sep colonpc
	DW ROT,OVER,MINUS
	DW DUP,ZEROGREATER,qbranch,SDRJ1
	DW ZERO,xdo
SDRJ2	DW BL,HOLD,MINUSONE,SLASHSTRING
	DW xloop,SDRJ2,branch,SDRJ3
SDRJ1	DW DROP
SDRJ3	DW TYPE,EXIT

;X U.R  ( u width --  print unsigned right justified )  
;  SWAP 
;  <# 0 #S #>  ( width addr u )
;  S.RJ
	DW link
	DB 0
link SET $
	DB 3,"U.R"
UDOTR
	sep colonpc
	DW SWAP,LESSNUM,ZERO,NUMS,NUMGREATER
	DW SDOTRJ,EXIT

;X .R  ( n width -- print signed right justified)
;  SWAP  ( w n )
;  <# DUP ABS 0 #S ROT SIGN #> ( w addr u )
;  S.RJ
;
	DW link
	DB 0
link SET $
	DB 2,".R"
DOTR
	sep colonpc
	DW SWAP,LESSNUM,DUP,ABS,ZERO
	DW NUMS,ROT,SIGN,NUMGREATER
	DW SDOTRJ,EXIT

;Z .BYTE ( u -- print byte in hex )
;  DUP 4 RSHIFT [ HEX ] 0F AND >DIGIT EMIT
;  0F AND >DIGIT EMIT
;
	DW link
	DB 0
link SET $
	DB 5,".BYTE"
DOTBYTE
	sep colonpc
	DW DUP,LIT,4,RSHIFT
	DW LIT,$0F,ANDD,TODIGIT,EMIT
	DW LIT,$0F,ANDD,TODIGIT,EMIT
	DW EXIT

;Z .ADDR ( u --  print address in hex )  
;  DUP >< .BYTE .BYTE 
;
	DW link
	DB 0
link SET $
	DB 5,".ADDR"
DOTADDR
	sep colonpc
	DW DUP,swapbytes,DOTBYTE
	DW DOTBYTE,EXIT

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
	DW link
	DB 0
link SET $
	DB 4,"DUMP"
DUMP
	sep colonpc
	DW OVER,DOTADDR
DUMP1	DW QDUP,qbranch,DUMP2
	DW OVER,CFETCH,SPACE,DOTBYTE
	DW ONEMINUS,SWAP,ONEPLUS
	DW DUP,LIT,$0F,ANDD
	DW ZEROEQUAL,qbranch,DUMP3
	DW CR,DUP,DOTADDR
DUMP3	DW SWAP,branch,DUMP1
DUMP2	DW DROP,EXIT

; DEFINING WORDS ===========================================

;X :NONAME	-- xt	begin a nameless colon definition
; LATEST @ , 0 C,link & immed field (in case of RECURSE)
; HERE LATEST !
; 0 C,			null name field
; HERE ] COMPILE docolon;
	DW link
	DB 0
link SET $
	DB 7,":NONAME"
NONAME
	sep colonpc
	DW LATEST,FETCH,COMMA,ZERO,CCOMMA
	DW HERE,LATEST,STORE
	DW ZERO,CCOMMA
	DW HERE,RIGHTBRACKET
	DW CFCOMPILE
	sep colonpc
	DW EXIT

;X VALUE	n --	create value object
; VALUE CONSTANT ;
	DW link
	DB 0
link SET $
	DB 5,"VALUE"
VALUE
	lbr CONSTANT

;Z CFCOMPILE     --       append a 1-byte code field
; It takes a 1-byte inline literal opcode and appends it
; to the dictionary
	DW link
	DB 0
link SET $
	DB 9,"CFCOMPILE"
CFCOMPILE
	sep colonpc
	DW RFROM,DUP,ONEPLUS,TOR
	DW CFETCH,CCOMMA,EXIT

;Z FINDWORD	 ( "ccc" -- xt f  1 = immediate, -1 = regular)
; BL WORD FIND
; ?DUP IF EXIT THEN
; COUNT TYPE [CHAR] ? EMIT ABORT
	DW link
	DB 0
link SET $
	DB 8,"FINDWORD"
FINDWORD
	sep colonpc
	DW BL,FWORD,FIND
	DW QDUP,qbranch,FW1
	DW EXIT
FW1	DW COUNT,TYPE
	DW LIT,$3F,EMIT,CR,ABORT

;X TO		n --	store in value object
; ' 1+		( skip code field )
; STATE @ IF !
;   COMPILE lit , COMPILE !
; ELSE
;   !
; THEN ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 2,"TO"
TO
	sep colonpc
	DW TICK,ONEPLUS
	DW STATE,FETCH,qbranch,TO1
	DW COMPILE,LIT		; compiling
	DW COMMA
	DW COMPILE
TO1				; interpreting
	DW STORE,EXIT

;X [COMPILE] Compilation: ( "<spaces>name" -- )
;  FINDWORD  ( -- xt f )
;  DROP  COMPILE,	( append semantics )
; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 9,"[COMPILE]"
BRACCOMP
	sep colonpc
	DW FINDWORD,DROP
	DW COMMAXT,EXIT

;Z CREATE1   --		create an empty definition with 1-byte code field
;   (CREATE) CFCOMPILE dovar
	DW link
	DB 0
link SET $
	DB 7,"CREATE1"
CREATE1
	sep colonpc
	DW XCREATE,CFCOMPILE
	sep varpc
	DW EXIT

;X CONVERT ud1 c-addr2 -- ud2 c-addr2
; CHAR+ -1 >NUMBER DROP ;
	DW link
	DB 0
link SET $
	DB 7,"CONVERT"
CONVERT
	sep colonpc
	DW CHARPLUS,MINUSONE,TONUMBER
	DW DROP,EXIT

;X MARKER  ( "<spaces>name" -- create marker for forgetting
;  CREATE LATEST @ NFA>LFA , 
;  DOES> @ DUP DP ! 
;   @ LATEST !
;
	DW link
	DB 0
link SET $
	DB 6,"MARKER"
MARKER
	sep colonpc
	DW XCREATE,CFCOMPILE
	sep createpc
	DW COMPILE,MARK1
	DW LATEST,FETCH,NFATOLFA,COMMA
	DW EXIT
MARK1	sep colonpc
	DW FETCH,DUP,DP,STORE
	DW FETCH,LATEST,STORE
	DW EXIT

; MISCELLANEOUS WORDS =============================

;X UNUSED  ( -- u get size of unused dictionary )
;  SP@ HERE -   ( Param stack grows towards dictionary )
;
	DW link
	DB 0
link SET $
	DB 6,"UNUSED"
UNUSED
	sep colonpc
	DW SPFETCH,HERE,MINUS,EXIT

;X 0<>     n/u -- flag    return true if TOS<>0
	DW link
	DB 0
link SET $
	DB 3,"0<>"
ZERONOTEQ
	lda psp
	or
	inc psp
	lbnz TRUE
	lbr FALSE

;X 0>     n/u -- flag    return true if TOS>0
	DW link
	DB 0
link SET $
	DB 2,"0>"
ZEROGREATER
	sep colonpc
	DW ZERO,SWAP,LESS,EXIT

; CONTROL FLOW WORDS ===================================

;X ?DO       -- adrs   L: -- 0
;  ['] 2DUP COMPILE,
;  ['] = COMPILE,            ( index = limit? )
;  POSTPONE IF            
;    ['] 2DROP COMPILE, 
;  POSTPONE ELSE   ( fwd branch )
;  BEGINLOOP ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 3,"?DO"
QDO
	sep colonpc
	DW COMPILE,TWODUP
	DW COMPILE,EQUAL
	DW IF,COMPILE,TWODROP
	DW ELSE,BEGINLOOP,EXIT

;Z BEGINLOOP	 ( common factor of DO and ?DO )
;  ['] (do) COMPILE,         ( compile runtime action )
;  HERE             ( bwd branch target )
;  0 >L                ( marker for LEAVEs )
	DW link
	DB 0
link SET $
	DB 9,"BEGINLOOP"
BEGINLOOP
	sep colonpc
	DW COMPILE,xdo
	DW HERE	; target for bwd branch
	DW ZERO,TOL,EXIT

;T AHEAD
;   ['] branch ,BRANCH HERE DUP ,DEST ; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 5,"AHEAD"
AHEAD         			; -- adrs
	sep colonpc
	DW LIT,branch,COMMABRANCH
	DW HERE,DUP,COMMADEST,EXIT

;X CASE
; 0 CONSTANT CASE IMMEDIATE
	DW link
	DB 1
link SET $
	DB 4,"CASE"
CASE
	sep constpc
	DW 0

;X OF  ( #of -- orig #of+1 / x -- )
;    1+    ( count OFs )
;    >R    ( move off the stack in case the control-flow )
;          ( stack is the data stack. )
;    POSTPONE OVER  POSTPONE = ( copy and test case value)
;    POSTPONE IF    ( add orig to control flow stack )
;    POSTPONE DROP  ( discards case value if = )
;    R>             ( we can bring count back now )
; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 2,"OF"
OF
	sep colonpc
	DW ONEPLUS,TOR,COMPILE,OVER,COMPILE,EQUAL
	DW IF,COMPILE,DROP,RFROM,EXIT

;X ENDOF ( orig1 #of -- orig2 #of )
;    >R   ( move off the stack in case the control-flow )
;         ( stack is the data stack. )
;    POSTPONE ELSE
;    R>   ( we can bring count back now )
; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 5,"ENDOF"
ENDOF
	sep colonpc
	DW TOR,ELSE,RFROM,EXIT

;X ENDCASE  ( orig1..orign #of -- )
;    POSTPONE DROP  ( discard case value )
;    ?DUP IF
;        0 DO
;          POSTPONE THEN
;       LOOP
;    THEN
; IMMEDIATE
	DW link
	DB 1
link SET $
	DB 7,"ENDCASE"
ENDCASE
	sep colonpc
	DW COMPILE,DROP,QDUP,qbranch,ENDC1
	DW ZERO,xdo
ENDC2	DW THEN,xloop,ENDC2
ENDC1	DW EXIT

; INPUT SOURCE WORDS ===================================

;X SOURCE-ID  ( -- flag )
; 'SOURCE CELL+ @ TIB = 0=	; compare current source 
	DW link		; to Terminal Input Buffer
	DB 0
link SET $
	DB 9,"SOURCE-ID"
SOURCEID
	sep colonpc
	DW TICKSOURCE,CELLPLUS,FETCH,TIB
	DW EQUAL,ZEROEQUAL,EXIT

;X REFILL		( -- flag )
; SOURCE-ID IF  ( string source )
;   FALSE
; ELSE          ( terminal source )
;   TIB #TIB ACCEPT
;   'SOURCE !  ( save char count )
;   0 >IN ! TRUE
; THEN
	DW link
	DB 0
link SET $
	DB 6,"REFILL"
REFILL
	sep colonpc
	DW SOURCEID,qbranch,REF1
	DW FALSE,EXIT
REF1
	DW TIB,TIBSIZE,ACCEPT,TICKSOURCE,STORE
	DW ZERO,TOIN,STORE,TRUE,EXIT

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
	DW link
	DB 1
link SET $
	DB 6,"[ELSE]"
BRACELSE
	sep colonpc
	DW ONE
BREL1	DW BL,FWORD,COUNT,DUP,qbranch,BREL6
	DW TWODUP,XSQUOTE
	DB 4,"[IF]"
        DW COMPARE,ZEROEQUAL,qbranch,BREL2
	DW TWODROP,ONEPLUS
	DW branch,BREL3
BREL2	DW TWODUP,XSQUOTE
	DB 6,"[ELSE]"
        DW COMPARE,ZEROEQUAL,qbranch,BREL4
        DW TWODROP,ONEMINUS,DUP,qbranch,BREL3
        DW ONEPLUS,branch,BREL3
BREL4	DW XSQUOTE
	DB 6,"[THEN]"
        DW COMPARE,ZEROEQUAL,qbranch,BREL3
 	DW ONEMINUS
BREL3	DW QDUP,ZEROEQUAL,qbranch,BREL5
	DW EXIT
BREL5	DW branch,BREL1
BREL6	DW TWODROP,REFILL,ZEROEQUAL,qbranch,BREL1
        DW DROP,EXIT

;T [IF]  ( flag -- )
; 0= IF POSTPONE [ELSE] THEN ;  IMMEDIATE
	DW link
	DB 1
link SET $
	DB 4,"[IF]"
BRACIF
	sep colonpc
 	DW ZEROEQUAL,qbranch,BRIF1
	DW BRACELSE
BRIF1	DW EXIT

;T [THEN]  ( -- )  ;  IMMEDIATE
	DW link
	DB 1
link SET $
	DB 6,"[THEN]"
BRACTHEN
	sep nextpc

;X SAVE-INPUT	( -- save-spec n )
; >IN @ 1	( for keyboard and EVALUATE )
	DW link
	DB 0
link SET $
	DB 10,"SAVE-INPUT"
SAVEIN
	sep colonpc
	DW TOIN,FETCH,ONE,EXIT

;X RESTORE-INPUT ( save-spec n -- f )
; 1 = IF >IN ! FALSE ELSE TRUE THEN
	DW link
	DB 0
link SET $
	DB 13,"RESTORE-INPUT"
RESTIN
	sep colonpc
	DW ONE,EQUAL,qbranch,REST1
	DW TOIN,STORE
	DW FALSE,EXIT
REST1:	DW TRUE,EXIT

; STACK MANIPULATION WORDS =======================

;X 2>R	 x y --  R: -- x y	move two to return stack
; R> ROT >R SWAP >R >R
	DW link
	DB 0
link SET $
	DB 3,"2>R"
TWOTOR
	sep colonpc
	DW RFROM,ROT,TOR
	DW SWAP,TOR,TOR
	DW EXIT

;X 2R>	-- x y R: x y --	move two from return stack
; R> R> R> ROT >R SWAP
	DW link
	DB 0
link SET $
	DB 3,"2R>"
TWORFROM
	sep colonpc
	DW RFROM,RFROM,RFROM
	DW ROT,TOR,SWAP
	DW EXIT

;X 2R@  -- x y			copy two from return stack
; R> 2R> 2DUP 2>R ROT >R 
	DW link
	DB 0
link SET $
	DB 3,"2R@"
TWORFETCH
	sep colonpc
	DW RFROM,TWORFROM,TWODUP
	DW TWOTOR,ROT,TOR,EXIT	

;T CS-PICK		control stack PICK
	DW link
	DB 0
link SET $
	DB 7,"CS-PICK"
CSPICK
	br PICK		; same as data stack PICK

;X PICK n -- x		pick n'th entry from stack
; CELLS SP@ + CELL+ @ >< ;	data stack is little-endian
	DW link
	DB 0
link SET $
	DB 4,"PICK"
PICK
	sep colonpc
	DW CELLS,SPFETCH,PLUS
	DW CELLPLUS,FETCH,swapbytes
	DW EXIT

;T CS-ROLL		control stack ROLL
	DW link
	DB 0
link SET $
	DB 7,"CS-ROLL"
CSROLL
	br ROLL		; same as data stack ROLL

;X ROLL	( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
;  DUP >R PICK		 get entry to be rolled
;  SP@ DUP CELL+	 get src and dest of move
;  R> CELLS CELL+ CMOVE> DROP
	DW link
	DB 0
link SET $
	DB 4,"ROLL"
ROLL
	sep colonpc
	DW DUP,TOR,PICK
	DW SPFETCH,DUP,CELLPLUS
	DW RFROM,CELLS,CELLPLUS
	DW CMOVEUP,DROP,EXIT

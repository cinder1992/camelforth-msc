
README file for CamelForth for the RCA 1802
===========================================
Version 1.1 2009-04-11

Harold Rabbie hzrabbie@comcast.net

This is an implementation of Brad Rodriguez's Camel Forth, ported to the
RCA 1802 8-bit processor. The data sheet describing the instruction set 
of this processor is available at http://www.intersil.com/data/fn/fn1441.pdf

Refer to the ANSI Forth Standard X3.215-1994 for a description of the standard Forth
words included in this implementation. A draft of the standards document is available 
at http://www.complang.tuwien.ac.at/forth/dpans-html/dpans.htm

This implementation includes full support for the following word sets:

6.1 Core word set
6.2 Core extension word set
15  Programming-Tools and extension word sets
17  String word set

and partial support for the Double-Number word set.

Files included in this archive
------------------------------
RCA 1802 Assembler source code files:

camel18.asm		FORTH primitives and inner interpreter
camel18d.asm		CPU and Model Dependencies
camel18h.asm		Base set of high-level words
camel18x.asm		Additional high-level words

This code may be assembled using the Pseudo-SAM 1802 cross-assembler for MS-DOS.
This cross-assembler is available at http://www.camelforth.com/download.php?view.13

1802sim.c		C source code for an 1802 CPU simulator (for Cygwin or Linux)
makefile		makefile for Cygwin
Readme.txt		The file you're reading now

tester.fr		Test framework for conformance testing 

Building the CamelForth system
------------------------------
Typing "make" in Cygwin cross-assembles the RCA1802 source code and generates 
two files:

CF1802.OBJ	Intel hex object code
CF1802.LST	Assembly listing

Simulating Forth on the RCA 1802
--------------------------------
"make" also builds the RCA 1802 instruction-level simulator, which allows a Linux or
Cygwin host PC to simulate object code running on RCA 1802 hardware. 

Simulator command-line:
1802sim <filename> [-f] [-i] [-e]

<filename> is the name of the .OBJ and .LST files to be simulated (without the extension)
-f turns on Forth-level tracing of all colon definitions
-i turns on instruction-level tracing
-e echoes command lines read via ACCEPT to the standard output

The simulator emulates Forth KEY, EMIT and ACCEPT using standard output and standard input.
"make run" runs the simulation of the CamelForth system

Additional Standard Forth words
-------------------------------
The following Forth standard words are new in this release:

Core extension words:
.( .R 0<> 0> 2>R 2R> 2R@ :NONAME ?DO C" CASE CONVERT ENDCASE ENDOF ERASE
MARKER OF PARSE PICK REFILL RESTORE-INPUT ROLL SAVE-INPUT SOURCE-ID TO
U.R UNUSED VALUE [COMPILE] \ 

String words: 
-TRAILING BLANK COMPARE SEARCH SLITERAL

Programming Tools words: 
AHEAD CS-PICK CS-ROLL DUMP [ELSE] [IF] [THEN]

Useful Non-Standard Forth words
-------------------------------
TRACE ( n -- ) controls simulator tracing at run-time:
1 TRACE turns on Forth-level tracing for the current colon definition
2 TRACE turns on Instruction-level tracing
4 TRACE turns on command-line echoing
0 TRACE turns off all tracing

.SYMBOL ( addr -- ) prints the address symbolically using the symbol table created
by the cross-assembler.

CREATE1 ( "<spaces>name" -- ) creates a dictionary entry similar to CREATE, except that 
it cannot be referred to by DOES> or >BODY, but is smaller and faster.

.ADDR ( u -- ) and .BYTE ( u -- ) print 16-bit and 8-bit values in hexadecimal

Testing Notes
-------------
John Hayes and Gerry Jackson's ANSI Forth conformance test suite version 0.5 is available from 
http://www.qlikz.org/forth/anstests/anstests.html.

This implementation passes all the tests in the core, core extension, core plus, string, and 
tools test sets with a couple of exceptions:

1. The test suite assumes a case-insensitive dictionary.  The CamelForth dictionary is case-sensitive, 
so the included file tester.fr defines lower-case versions of a few words.

2. The core extension test suite includes some tests of SAVE-INPUT and RESTORE-INPUT that are
only valid when executed from an included file. CamelForth does not support file inclusion,
so these tests are inapplicable.

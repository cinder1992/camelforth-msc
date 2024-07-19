: ROR ( c -- c )
	DUP 1 AND 0<> IF 256 OR THEN 2/ ;

: kill-bit
 	1 BEGIN \ d c -- 
	OVER 0 DO LOOP \ do nothing for a few cycles
	4 INP XOR ROR DUP 4 OUTP
	DUP 0= EF4? OR UNTIL DROP DROP ;


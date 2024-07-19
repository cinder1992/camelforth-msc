\ Kill the bit
\ Classic front-panel game
\ Ported to CamelForth-MC by Neil Ray, Jul 19 2024

\ How to play:
\ Enter a difficulty from 1 to 32767 and kill-bit
\ the higher the difficulty, the easier the game!
\ try and flip the switch under the lights that are on
\ to kill the bit!
\ Beware, trying to kill dead bits will make them
\ rise from the grave!

: ROR ( c -- c ) \ Rotate C right by one bit
	DUP 1 AND 0<> IF 256 OR THEN 2/ ;

: kill-bit ( d -- )
 	1 BEGIN \ d c -- 
	OVER 0 DO LOOP \ do nothing for a few cycles
	4 INP XOR ROR DUP 4 OUTP
	DUP 0= EF4? OR UNTIL DROP DROP ;


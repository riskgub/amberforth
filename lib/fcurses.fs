CREATE vmem 80 24 * 2* CHARS ALLOT
30 CONSTANT BLACK  31 CONSTANT RED
32 CONSTANT GREEN  33 CONSTANT YELLOW
34 CONSTANT BLUE   35 CONSTANT MAGENTA
36 CONSTANT CYAN   37 CONSTANT WHITE
VARIABLE vcur 0 vcur !
: vputchar ( c x y -- ) 80 * + vmem + c! ;
: vputattr ( c x y -- ) 80 * + 2* vmem + c! ;
: .| abs 0 <# #s #> type ;
: tattr ( n n -- ) 27 emit 91 emit 10 + .| 59 emit .| 109 emit ;
: vredraw ( -- )
 24 0 DO 80 0 DO
   I J 2DUP AT-XY
   80 * + DUP 2* vmem + C@ DUP 
   7 AND 30 + SWAP 56 AND 3 RSHIFT 40 + tattr 
   vmem + C@ EMIT
  LOOP LOOP ;
: vpage 80 24 * 2* 0 DO 7 I vmem + C! LOOP ;
: vemit vmem vcur @ + c! 1 CHAR vcur +! ;
: vtype ?dup if bounds do i c@ vemit loop else drop then ;
vpage s" hell" vtype vredraw bye

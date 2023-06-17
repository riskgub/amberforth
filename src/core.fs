( CONSTANT SP0
  CONSTANT RP0
  \ are defined by the machine description )
VARIABLE SP \ Stack Pointer
VARIABLE RP \ Return Stack Pointer
VARIABLE DP \ Data Space Pointer
VARIABLE WP \ Dictionary Space Pointer
VARIABLE CP \ Code Space Pointer
VARIABLE IP \ Instruction Pointer

: doVAR ( -- a ) R> ;
VARIABLE UP ( -- a, Pointer to the user area.)
: doUSER ( -- a, Run time routine for user variables.)
  R> @ \ retrieve user area offset
  UP @ + ; \ add to user area base addr
: doVOC ( -- ) R> CONTEXT ! ;
: FORTH ( -- ) doVOC [ 0 , 0 ,
: doUSER ( -- a ) R> @ UP @ + ;

CELL NEGATE CONSTANT -CELL

: HERE ( -- CP ) CP @ ;
: HEX     16 BASE ! ;
: DECIMAL 10 BASE ! ;
: OCTAL    8 BASE ! ;
( STACK OPERATORS )
: SP@   ( -- SP ) SP @ ;
: RP@   ( -- RP ) RP @ ;
: >S    ( x1 -- ) SP @ ! -CELL SP +! ;
: >R    ( x1 -- ) RP @ ! -CELL RP +! ;
: S>    ( -- x1 ) SP @ @  CELL SP +! ;
: R>    ( -- x1 ) RP @ @  CELL RP +! ;
: DEPTH ( -- x1 ) SP@ SP0 - ;
: DUP   ( x1 -- x1 x1 ) SP@ @ ;
: DROP  ( x1 -- )     2 CELLS SP +! ;
: OVER  ( x1 x2 -- x1 x2 x1 ) SP@ CELL + @ ;
: SWAP  ( x1 x2 -- x2 x1 )    OVER OVER SP@ 3 CELLS + ! SP@ CELL + ! ;
: ROT   ( x1 x2 x3 -- x2 x3 x1 ) R> SWAP R> SWAP ;
: NIP   ( x1 x2    -- x2       ) SWAP DROP ;
: 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 ) ROT >R ROT R> ;
: 2DROP ( x1 x2 -- ) DROP DROP ;
: 2DUP  ( x1 x2 -- x1 x2 x1 x2 ) OVER OVER ;
: 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 ) 2>R 2DUP 2R> 2SWAP ;
: ?DUP  ( x1 -- x1 x1 | 0 ) DUP IF DUP THEN ;
: DEPTH ( -- n ) SP@ SP0 @ SWAP - CELL / ;
: PICK  ( +n -- w ) 1 + CELLS SP@ + @ ;
( LOGIC & ARITHMETIC OPERATORS )
: INVERT ( n -- !n ) -1 XOR ;
: NOT POSTPONE INVERT ;
: + ( n1 n2 -- n1+n2) UM+ DROP ;
: D+ ( d d -- d ) >R SWAP >R UM+ R> R> + + ;
: 1+ ( n -- n+1 ) 1 + ;
: NEGATE  ( n -- -n ) INVERT 1+ ;
: DNEGATE ( d -- -d ) INVERT R> INVERT 1 UM+ R> + ;
: - ( n1 n2 -- n1-n2) NEGATE + ;
: 1- ( n -- n-1) 1 - ;
: ABS ( x1 -- x2 ) DUP 0< IF NEGATE THEN ;
: 2*  ( x1 -- x2 ) DUP + ;
: 2/  ( x1 -- x2 ) DUP - ;
: RSHIFT ( x1 x2 -- x3 ) 0 DO DUP 2* LOOP NIP ;
: LSHIFT ( x1 x2 -- x3 ) 0 DO DUP 2/ LOOP NIP ;
( COMPARSION OPERATORS )
: =  ( x1 x2 -- f ) XOR IF 0 EXIT THEN -1 ;
: U< ( u  u  -- f ) 2DUP XOR 0< IF SWAP DROP 0< EXIT THEN - 0< ;
: <  ( n  n  -- f ) 2DUP XOR 0< IF      DROP 0< EXIT THEN - 0< ;
: >  ( n  n  -- f ) SWAP < ;
: <> ( n  n  -- f ) = INVERT ;
: MAX ( n n -- n ) 2DUP < IF SWAP THEN DROP ;
: MIN ( n n -- n ) 2DUP SWAP < IF SWAP THEN DROP ;
: WITHIN ( u ul uh -- f ) OVER - >R - R> U< ;
( MEMORY ALIGNMENT )
: CELL- CELL - ;
: CELL+ CELL + ;
: CELLS CELL * ;
: ALIGNED
  DUP 0 CELL UM/MOD DROP DUP
  IF CELL SWAP - THEN + ;
( MEMORY ACCESS )
: +! ( x1 addr -- ) SWAP OVER @ + SWAP ! ;
: 2! ( d a -- ) SWAP OVER ! CELL+ ! ;
: 2@ ( a -- d ) DUP CELL+ @ SWAP @ ;
: COUNT ( b -- b +n ) DUP 1+ SWAP C@ ;
: HERE ( -- a ) CP @ ;
: PAD  ( -- a ) HERE 128 + ;
: TIB  ( -- a ) #TIB CELL+ @ ;
: @EXECUTE ( a -- ) ?DUP IF EXECUTE THEN ;
: CMOVE ( b b u -- )
  FOR AFT >R DUP C@ R@ C! 1+ R> 1+ THEN NEXT 2DROP ;
: FILL ( b u c -- )
  SWAP FOR SWAP AFT 2DUP C! 1+ THEN NEXT 2DROP ;
: -TRAILING ( b u -- b u )
  FOR AFT BL OVER R@ + C@ <
  IF R> 1+ EXIT THEN THEN
NEXT 0 ;
: PACK$ ( b u a -- a )
  ALIGNED DUP >R OVER
  DUP 0 CELL UM/MOD DROP
  - OVER + 0 SWAP ! 2DUP C! 1 + SWAP CMOVE R> ;
( MISC )
: BL 32 ;
( NUMERIC OUTPUT )
: DIGIT ( u -- c ) 9 OVER < 7 AND + 48 + ;
: EXTRACT ( n base -- n c ) 0 SWAP UM/MOD SWAP DIGIT ;
: <# ( -- ) PAD HLD ! ;
: HOLD ( c -- ) HLD @ 1- DUP HLD ! C! ;
: # ( u -- u ) BASE @ EXTRACT HOLD ;
: #S ( u -- 0 ) BEGIN # DUP WHILE REPEAT ;
: SIGN ( n -- ) 0< IF 45 HOLD THEN ;
: #> ( w -- b u ) DROP HLD @ PAD OVER - ;
: str ( n -- b u ) DUP >R ABS <# #S R> SIGN #> ;
: .R ( n +n -- ) >R STR R> OVER - SPACES TYPE ;
: U.R ( u +n -- ) >R <# #S #> R> OVER - SPACES TYPE ;
: U. ( u -- ) <# #S #> SPACE TYPE ;
: .  ( w -- ) BASE @ 10 XOR IF U. EXIT THEN str SPACE TYPE ;
: ?  ( a -- ) @ . ;
( NUMERIC INPUT )  
: DIGIT? ( c base -- u t )
  >R 48 - 9 OVER <
  IF 7 - DUP 10 < OR THEN DUP R> U< ;
: NUMBER? ( a -- n T | a F )
  BASE @ >R 0 OVER COUNT
  OVER C@ 36 =
  IF HEX SWAP 1+ SWAP 1- THEN
  OVER C@ 45 = >R
  SWAP R@ - SWAP R@ + ?DUP
  IF 1 -
   FOR DUP >R C@ BASE @ DIGIT?
    WHILE SWAP BASE @ * + R> 1 +
   NEXT DROP R@ IF NEGATE THEN SWAP
    ELSE R> R> 2DROP 2DROP 0
    THEN DUP
THEN r> 2DROP R> BASE ! ;
: ?KEY ( -- c T | F ) '?KEY @EXECUTE ;
: KEY  ( -- c ) BEGIN ?KEY UNTIL ;
: EMIT ( c -- ) 'EMIT @EXECUTE ;
: NUF? ( -- f ) ?KEY DUP IF 2DROP KEY = THEN ;
:  PACE ( -- )  11 EMIT ;
: SPACE ( -- )  BL EMIT ;
: EMIT* ( +n c -- ) SWAP 0 MAX FOR AFT DUP EMIT THEN NEXT DROP ;
: SPACES ( +n -- ) BL EMIT* ;
: TYPE ( b u -- ) FOR AFT DUP C@ EMIT 1+ THEN NEXT DROP ;
: CR ( -- ) 13 EMIT 10 EMIT ;
: do$ ( -- a )
  >R R@ R> COUNT + ALIGNED >R SWAP >R ;
: $"| ( -- a ) do$ ;
: ."| ( -- ) do$ COUNT TYPE ; COMPILE-ONLY
( PARSING )
: _parse ( b u c -- b u delta ; <string> )
  tmp ! OVER >R DUP
  IF 1- tmp @ BL =
   IF
    FOR BL OVER C@ - 0< NOT WHILE 1+
    NEXT R> DROP 0 DUP EXIT
     THEN R>
  THEN OVER SWAP
  FOR tmp @ OVER C@ - tmp @ BL =
   IF 0< THEN WHILE 1+
  NEXT DUP >R ELSE R> DROP DUP 1+ >R
  THEN OVER - R> R> - EXIT
THEN OVER R> - ;
: PARSE ( c -- b u ; <string> )
  >R TIB >IN @ + #TIB @ >IN @ - R> _parse >IN +! ;
: .( ( -- ) 41 PARSE TYPE ; IMMEDIATE
: (  ( -- ) 41 PARSE 2DRIO ; IMMEDIATE
: \  ( -- ) #TIB @ >IN ! ; IMMEDIATE

: CHAR ( -- c ) BL PARSE DROP C@ ;

: TOKEN ( -- a ; <string> )
  BL PARSE 31 MIN NP @ OVER - CELL- PACK$ ;
: WORD ( c -- a ; <string> ) PARSE HERE PACK$ ;
( DICTIONARY SEARCH )
: NAME> ( a -- xt ) CELL- CELL- @ ;
: SAME?  ( a a u -- a a f \ -0+ )
  FOR AFT OVER R@ CELLS + @
  OVER R@ CELLS + @ - ?DUP
  IF R> DROP EXIT THEN THEN
NEXT 0 ;
: find ( a va -- xt na | a F )
  SWAP DUP C@ CELL / tmp !
  DUP @ >R
  CELL+ SWAP
  BEGIN @ DUP
   IF DUP @ [ =MASK ] LITERAL AND R@ XOR
    IF CELL+ -1 ELSE CELL+ tmp @ SAME? THEN
    ELSE R> DROP EXIT THEN
   WHILE CELL- CELL-
  REPEAT R> DROP SWAP DROP CELL- DUP NAME> SWAP ;
: NAME? ( a -- xt na | a F )
  CONTEXT DUP 2@ XOR IF CELL - THEN >R
  BEGIN R> CELL+ DUP >R @ ?DUP
  WHILE find ?DUP
  UNTIL R> DROP EXIT THEN R> DROP 0 ;

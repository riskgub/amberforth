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

CREATE PAD 128 CHARS ALLOT
    1 CELLS CONSTANT CELL
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
( LOGIC & ARITHMETIC OPERATORS )
: INVERT ( n -- !n ) -1 XOR ;
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

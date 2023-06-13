: <>  ( n n -- f )  = INVERT ;
: .( [char] ) parse type ;
\ .R
: 0<> ( n  -- f  )  0 <> ;
: 0>  ( n  -- f  )  0 >  ;
: 2>R ( n1 n2 -- )  SWAP R> R>   ;
: 2R> ( -- n1 n2 )  R> R> SWAP   ; 
: 2R@ ( -- n1 n2 )  2R> 2DUP 2>R ;
\ :noname
\ ?DO
\ ACTION-OF
\ AGAIN
\ BUFFER:
\ C"
: CASE 0 ;
: OF POSTPONE OVER POSTPONE = IF POSTPONE DROP ; COMPILE-ONLY
: ENDOF POSTPONE ELSE SWAP 1+ ; COMPILE-ONLY
\ : ENDCASE POSTPONE DROP 0 ?DO POSTPONE THEN LOOP ; COMPILE-ONLY
 

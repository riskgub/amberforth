
: BL    (   -- n ) 32   ;
: 0=    ( n -- f ) 0 =  ;
: 0<    ( n -- f ) 0 <  ;
: 0>    ( n -- f ) 0 >  ;
: 0<>   ( n -- f ) 0 <> ;
: 2DROP ( n1 n2 -- ) DROP DROP ;
: 2DUP  ( n1 n2 -- n1 n2 n1 n2 ) OVER OVER ;
: 2R@   ( -- n1 n2 ) 2DUP >R >R SWAP ;
: 2R>   ( -- n1 n2 ) R> R> SWAP ;
: 2>R   ( n1 n2 -- ) SWAP R> R> ;
: 2R@   ( -- n1 n2 ) 2R> 2DUP 2>R ;
: 2OVER ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
 2>R 2DUP 2R> 2SWAP ;

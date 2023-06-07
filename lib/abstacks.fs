64 CONSTANT STACK-SIZE
CREATE astack STACK-SIZE CELLS ALLOT
CREATE bstack STACK-SIZE CELLS ALLOT
VARIABLE sap VARIABLE sbp
astack STACK-SIZE + CONSTANT sa0
bstack STACK-SIZE + CONSTANT sb0
sa0 sap ! sb0 sbp !

: sap@ ( -- n ) sap @ ;
: sbp@ ( -- n ) sbp @ ;
: >A ( n -- ) 1 CELLS NEGATE sap +! sap @ ! ;
: A> ( -- n ) sap @ @ 1 CELLS sap +! ;
: A@ ( -- n ) sap @ @ ;
: >B ( n -- ) 1 CELLS NEGATE sbp +! sbp @ ! ;
: B> ( -- n ) sbp @ @ 1 CELLS sbp +! ;
: B@ ( -- n ) sbp @ @ ;

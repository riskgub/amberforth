64 CELLS CONSTANT STACK-SIZE
CREATE astack STACK-SIZE CELLS ALLOT
CREATE bstack STACK-SIZE CELLS ALLOT
VARIABLE sap VARIABLE sbp
astack STACK-SIZE + CONSTANT sap0
bstack STACK-SIZE + CONSTANT sbp0
sap0 sap ! sbp0 sbp !

: sap@ (   -- n ) sap @ ;
: sbp@ (   -- n ) sbp @ ;
: >A   ( n --   ) 1 CELLS NEGATE sap +! sap @ ! ;
: A>   (   -- n ) sap @ @ 1 CELLS sap +! ;
: A@   (   -- n ) sap @ @ ;
: >B   ( n --   ) 1 CELLS NEGATE sbp +! sbp @ ! ;
: B>   (   -- n ) sbp @ @ 1 CELLS sbp +! ;
: B@   (   -- n ) sbp @ @ ;

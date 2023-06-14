HEX
000000FF CONSTANT red
0000FF00 CONSTANT green
00FF0000 CONSTANT blue
DECIMAL
 1 CELLS CONSTANT pixel
: bmp_dim    ( bmp -- w h  ) 2@ ;
: bmp_height ( bmp -- h    )  @ ;
: bmp_width  ( bmp -- w    ) bmp_dim DROP ;
: bmp_data   ( bmp -- addr ) 2 CELLS + ;
: bmp_new    ( w h -- bmp  )
 2DUP * CELLS bmp_data ALLOCATE THROW
 DUP ROT ROT 2! ;
: bmp_free   ( bmp --      ) FREE THROW ;
: bmp_fill   ( color bmp -- )
 DUP bmp_data SWAP bmp_dim * CELLS 
 OVER OVER + NIP SWAP DO
  DUP I ! 1 CELLS +LOOP DROP ;
: bmp_xy     ( x y bmp -- )
DUP >R bmp_width * + CELLS R> bmp_data + ;
: bmp@       ( x y bmp -- color ) bmp_xy @ ;
: bmp!       ( color x y bmp -- ) bmp_xy ! ;

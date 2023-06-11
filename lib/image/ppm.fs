: ppm_write ( bmp fid -- )
 DUP s" P6"  ROT WRITE-LINE THROW \ binary
 >R
 OVER bmp_dim SWAP ( bmp fid h w )
 <# BL HOLD #S #> R@ WRITE-FILE THROW
 <# #S #> R> DUP WRITE-LINE THROW
 DUP s" 255" ROT WRITE-LINE THROW \ RGB888
 SWAP DUP bmp_data SWAP bmp_dim * CELLS
 2DUP + NIP SWAP DO
  DUP I 3 ROT WRITE-FILE THROW
 1 CELLS +LOOP
;

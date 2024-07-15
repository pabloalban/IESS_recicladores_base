message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFiltrando resultados de salud' )

# Número de threads que utilizará data table para operar
setDTthreads( threads = 8 )

# Nota ---------------------------------------------------------------------------------------------
# Esta carga consume mucha memoria RAM, no puede ser realizada en una computadora con poca RAM

# Procesando afiliados -----------------------------------------------------------------------------
message( '\tProcesando afiliados' )

load( paste0( parametros$RData_seg, 'IESS_SAL_afiliados.RData' ) )

setnames( afi_sal, 
          c( 'TIPO_ENFERMEDAD', 'TIP_SER', 'SEXO', 'CAPITULO', 'VSOL', 'ID_PACI' ), 
          c( 'enf', 'ser', 'sexo', 'cap', 'X', 'id' ) )

# Filtrando código ICD
afi_sal[ , icd := DIAG_PRINCIPAL ]
afi_sal[ , icd := gsub( '[^[:alnum:]]', '', icd ) ]
afi_sal[ , icd := str_trim( icd ) ]
afi_sal <- afi_sal[ grepl( '^[A-Z]{1}', icd ) ]
afi_sal <- afi_sal[ nchar( icd ) > 0 ]
afi_sal[ , icd := substr( icd, 1, 3 ) ]

# Corrección fecha nacimiento
afi_sal[ , y := as.numeric( substr( FEC_NAC, 8, 9 ) ) ]
afi_sal[ y >= 0 & y <= 19, y := 2000 + y ]
afi_sal[ y >= 20 & y <= 99, y := 1900 + y ]
afi_sal[ , fec_nac := substr( FEC_NAC, 1, 7 ) ]
afi_sal[ , fec_nac := paste0( fec_nac, '-', y ) ]
afi_sal[ , fec_nac := dmy( fec_nac ) ]

# Corrección fecha ingreso
afi_sal[ , y := as.numeric( substr( FEC_ING, 8, 9 ) ) ]
afi_sal[ , y := 2000 + y ]
afi_sal[ , fec_ing := substr( FEC_ING, 1, 7 ) ]
afi_sal[ , fec_ing := paste0( fec_ing, '-', y ) ]
afi_sal[ , fec_ing := dmy( fec_ing ) ]

# Corrección fecha salida
afi_sal[ , y := as.numeric( substr( FEC_SAL, 8, 9 ) ) ]
afi_sal[ , y := 2000 + y ]
afi_sal[ , fec_sal := substr( FEC_SAL, 1, 7 ) ]
afi_sal[ , fec_sal := paste0( fec_sal, '-', y ) ]
afi_sal[ , fec_sal := dmy( fec_sal ) ]
gc()

# Tomando columnas necesarias
afi_sal <- afi_sal[ , list( y, id, sexo, enf, ser, cap, icd, fec_nac, fec_ing, fec_sal, X ) ]
gc()

# Cálculo edad
afi_sal[ , x := round( interval( fec_nac, fec_ing ) / dyears(1), 0 ) ]

# Cálculo tiempo de estadía
afi_sal[ fec_ing < parametros$fec_ini, fec_ing := parametros$fec_ini ]
afi_sal[ fec_sal > parametros$fec_fin, fec_sal := parametros$fec_fin ]
afi_sal[ , D := interval( fec_ing, fec_sal ) / dyears(1) ]

# Año del siniestro
afi_sal[ , y := year( fec_ing ) ]

afi_sal <- afi_sal[ x >= 0 & x <= 110 & D >= 0 ]
afi_sal <- afi_sal[ X > 0 ]
afi_sal <- afi_sal[ y >= 2013 ]

afi_sal[ enf == 'E', u := as.character( cut( x, breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                                             include.lowest = TRUE, right = FALSE, 
                                             ordered_result = FALSE ) ) ]
afi_sal[ enf == 'C', u := as.character( cut( x, breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                                             include.lowest = TRUE, right = FALSE, 
                                             ordered_result = FALSE ) ) ]

afi_sal <- afi_sal[ X < 1e6 ]
afi_sal[ , X_p99 := quantile( X, probs = 0.99999 ), by = list( enf, ser, cap ) ]
afi_sal <- afi_sal[ X <= X_p99 ]
afi_sal[ , X_p01 := quantile( X, probs = 0.01 ), by = list( enf, ser, cap ) ]
afi_sal <- afi_sal[ X >= X_p01 ]

afi_sal[ , D_p95 := quantile( D, probs = 0.95 ), by = list( enf, ser, u ) ]
afi_sal <- afi_sal[ D <= D_p95 ]

afi_sal[ ser == 'HO' & D > 6, D := 6 ]

gc()

save( afi_sal,
      file = paste0( parametros$RData_seg, 'IESS_SAL_afiliados_filtrado.RData' ) )

rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

# Procesando pensionistas --------------------------------------------------------------------------
message( '\tProcesando pensionistas' )

load( paste0( parametros$RData_seg, 'IESS_SAL_pensionistas.RData' ) )

setnames( pen_sal, 
          c( 'TIPO_ENFERMEDAD', 'TIP_SER', 'SEXO', 'CAPITULO', 'VSOL', 'ID_PACI' ), 
          c( 'enf', 'ser', 'sexo', 'cap', 'X', 'id' ) )

# Filtrando código ICD
pen_sal[ , icd := DIAG_PRINCIPAL ]
pen_sal[ , icd := gsub( '[^[:alnum:]]', '', icd ) ]
pen_sal[ , icd := str_trim( icd ) ]
pen_sal <- pen_sal[ grepl( '^[A-Z]{1}', icd ) ]
pen_sal <- pen_sal[ nchar( icd ) > 0 ]
pen_sal[ , icd := substr( icd, 1, 3 ) ]

# Corrección fecha nacimiento
pen_sal[ , y := as.numeric( substr( FEC_NAC, 8, 9 ) ) ]
pen_sal[ y >= 0 & y <= 19, y := 2000 + y ]
pen_sal[ y >= 20 & y <= 99, y := 1900 + y ]
pen_sal[ , fec_nac := substr( FEC_NAC, 1, 7 ) ]
pen_sal[ , fec_nac := paste0( fec_nac, '-', y ) ]
pen_sal[ , fec_nac := dmy( fec_nac ) ]

# Corrección fecha ingreso
pen_sal[ , y := as.numeric( substr( FEC_ING, 8, 9 ) ) ]
pen_sal[ , y := 2000 + y ]
pen_sal[ , fec_ing := substr( FEC_ING, 1, 7 ) ]
pen_sal[ , fec_ing := paste0( fec_ing, '-', y ) ]
pen_sal[ , fec_ing := dmy( fec_ing ) ]

# Corrección fecha salida
pen_sal[ , y := as.numeric( substr( FEC_SAL, 8, 9 ) ) ]
pen_sal[ , y := 2000 + y ]
pen_sal[ , fec_sal := substr( FEC_SAL, 1, 7 ) ]
pen_sal[ , fec_sal := paste0( fec_sal, '-', y ) ]
pen_sal[ , fec_sal := dmy( fec_sal ) ]
gc()

# Tomando columnas necesarias
pen_sal <- pen_sal[ , list( y, id, sexo, enf, ser, cap, icd, fec_nac, fec_ing, fec_sal, X ) ]
gc()

# Cálculo edad
pen_sal[ , x := round( interval( fec_nac, fec_ing ) / dyears(1), 0 ) ]

# Cálculo tiempo de estadía
pen_sal[ fec_ing < parametros$fec_ini, fec_ing := parametros$fec_ini ]
pen_sal[ fec_sal > parametros$fec_fin, fec_sal := parametros$fec_fin ]
pen_sal[ , D := interval( fec_ing, fec_sal ) / ddays(1) ]

# Año del siniestro
pen_sal[ , y := year( fec_ing ) ]

pen_sal <- pen_sal[ x >= 0 & x <= 110 & D >= 0 ]
pen_sal <- pen_sal[ X > 0 ]
pen_sal <- pen_sal[ y >= 2013 ]

pen_sal[ enf == 'E', u := as.character( cut( x, breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                                             include.lowest = TRUE, right = FALSE, 
                                             ordered_result = FALSE ) ) ]
pen_sal[ enf == 'C', u := as.character( cut( x, breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                                             include.lowest = TRUE, right = FALSE, 
                                             ordered_result = FALSE ) ) ]

pen_sal <- pen_sal[ X < 1e6 ]
pen_sal[ , X_p99 := quantile( X, probs = 0.99999 ), by = list( enf, ser, cap ) ]
pen_sal <- pen_sal[ X <= X_p99 ]
pen_sal[ , X_p01 := quantile( X, probs = 0.01 ), by = list( enf, ser, cap ) ]
pen_sal <- pen_sal[ X >= X_p01 ]

pen_sal[ , D_p95 := quantile( D, probs = 0.95 ), by = list( enf, ser, u ) ]
pen_sal <- pen_sal[ D <= D_p95 ]

pen_sal[ ser == 'HO' & D > 6, D := 6 ]

gc()

save( pen_sal,
      file = paste0( parametros$RData_seg, 'IESS_SAL_pensionistas_filtrado.RData' ) )

rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

# Procesando hijos ---------------------------------------------------------------------------------
message( '\tProcesando hijos' )

load( paste0( parametros$RData_seg, 'IESS_SAL_hijos.RData' ) )

setnames( hijos_sal, 
          c( 'TIP_ENF', 'TIP_SER', 'SEXO', 'CAPITULO', 'FAC', 'ID_HIJO' ), 
          c( 'enf', 'ser', 'sexo', 'cap', 'X', 'id' ) )

# Filtrando código ICD
hijos_sal[ , icd := DIAG ]
hijos_sal[ , icd := gsub( '[^[:alnum:]]', '', icd ) ]
hijos_sal[ , icd := str_trim( icd ) ]
hijos_sal <- hijos_sal[ grepl( '^[A-Z]{1}', icd ) ]
hijos_sal <- hijos_sal[ nchar( icd ) > 0 ]
hijos_sal[ , icd := substr( icd, 1, 3 ) ]

# Corrección fecha nacimiento
hijos_sal[ , y := as.numeric( substr( FEC_NAC, 8, 9 ) ) ]
hijos_sal[ y >= 0 & y <= 19, y := 2000 + y ]
hijos_sal[ y >= 20 & y <= 99, y := 1900 + y ]
hijos_sal[ , fec_nac := substr( FEC_NAC, 1, 7 ) ]
hijos_sal[ , fec_nac := paste0( fec_nac, '-', y ) ]
hijos_sal[ , fec_nac := dmy( fec_nac ) ]

# Corrección fecha ingreso
hijos_sal[ , y := as.numeric( substr( FEC_ING, 8, 9 ) ) ]
hijos_sal[ , y := 2000 + y ]
hijos_sal[ , fec_ing := substr( FEC_ING, 1, 7 ) ]
hijos_sal[ , fec_ing := paste0( fec_ing, '-', y ) ]
hijos_sal[ , fec_ing := dmy( fec_ing ) ]

# Corrección fecha salida
hijos_sal[ , y := as.numeric( substr( FEC_SAL, 8, 9 ) ) ]
hijos_sal[ , y := 2000 + y ]
hijos_sal[ , fec_sal := substr( FEC_SAL, 1, 7 ) ]
hijos_sal[ , fec_sal := paste0( fec_sal, '-', y ) ]
hijos_sal[ , fec_sal := dmy( fec_sal ) ]
gc()

# Tomando columnas necesarias
hijos_sal <- hijos_sal[ , list( y, id, sexo, enf, ser, cap, icd, fec_nac, fec_ing, fec_sal, X ) ]
gc()

# Cálculo edad
hijos_sal[ , x := round( interval( fec_nac, fec_ing ) / dyears(1), 0 ) ]

# Cálculo tiempo de estadía
hijos_sal[ fec_ing < parametros$fec_ini, fec_ing := parametros$fec_ini ]
hijos_sal[ fec_sal > parametros$fec_fin, fec_sal := parametros$fec_fin ]
hijos_sal[ , D := interval( fec_ing, fec_sal ) / dyears(1) ]

# Año del siniestro
hijos_sal[ , y := year( fec_ing ) ]

hijos_sal <- hijos_sal[ x >= 0 & x <= 110 & D >= 0 ]
hijos_sal <- hijos_sal[ X > 0 ]
hijos_sal <- hijos_sal[ y >= 2013 ]

hijos_sal[ enf == 'E', u := as.character( cut( x, breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                                               include.lowest = TRUE, right = FALSE, 
                                               ordered_result = FALSE ) ) ]
hijos_sal[ enf == 'C', u := as.character( cut( x, breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                                               include.lowest = TRUE, right = FALSE, 
                                               ordered_result = FALSE ) ) ]

hijos_sal <- hijos_sal[ X < 1e6 ]
hijos_sal[ , X_p99 := quantile( X, probs = 0.99999 ), by = list( enf, ser, cap ) ]
hijos_sal <- hijos_sal[ X <= X_p99 ]
hijos_sal[ , X_p01 := quantile( X, probs = 0.01 ), by = list( enf, ser, cap ) ]
hijos_sal <- hijos_sal[ X >= X_p01 ]

hijos_sal[ , D_p95 := quantile( D, probs = 0.95 ), by = list( enf, ser, u ) ]
hijos_sal <- hijos_sal[ D <= D_p95 ]

hijos_sal[ ser == 'HO' & D > 6, D := 6 ]

gc()

save( hijos_sal,
      file = paste0( parametros$RData_seg, 'IESS_SAL_hijos_filtrado.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

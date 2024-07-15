# Carga beneficos salud ----------------------------------------------------------------------------
# Nueva carga de información de salud, esta se realiza por cada año y guarda los objetos respectivos 
# por año. De esta forma el proceso de carga puede ser incremental.
message( paste( rep('-', 100 ), collapse = '' ) )

# Lectura beneficios de salud ----------------------------------------------------------------------
anios <- seq( 2022, 2014, -1 )

for ( i in 1:length( anios ) ) {
  message( '\tLeyendo el año: ', anios[ i ] )
  
  ## Lectura SOAM ----------------------------------------------------------------------------------
  message( '\tLeyendo base de SOAM para el año: ', anios[ i ] )
  file <- paste0( parametros$Data_seg, anios[ i ], '_SOAM.txt' )
  soam <- fread( 
    file, 
    header = TRUE, 
    sep = 'auto', 
    showProgress = TRUE, 
    encoding = 'Latin-1', 
    fill = TRUE,
    blank.lines.skip = TRUE,
    verbose = FALSE )
  colname <- c( 'anio', 'id_ben', 'val', 'x', 'sexo', 'diagnostico', 'nom_serv', 'tip_ben', 'y', 'tip_serv' )
  setnames( soam, colname )
  soam[ , bd := factor( 'SOAM', levels = c( 'AS400', 'SOAM' ) ) ]
  soam[ , anio := as.integer( anio ) ]
  soam[ , x := as.integer( x ) ]
  soam[ , sexo := factor( sexo, levels = c( 'F', 'M' ) ) ]
  soam[ , val := as.numeric( gsub( '\\,', '.', val ) ) ]
  soam[ tip_serv == 'OTRA', tip_serv := 'OTRO' ]
  
  ## Lectura AS400 ---------------------------------------------------------------------------------
  message( '\tLeyendo base de AS400 para el año: ', anios[ i ] )
  file <- paste0( parametros$Data_seg, anios[ i ], '_FACT.txt' )
  as400 <- fread( 
    file, 
    header = TRUE, 
    sep = 'auto', 
    showProgress = TRUE, 
    encoding = 'Latin-1', 
    fill = TRUE,
    blank.lines.skip = TRUE,
    verbose = FALSE )
  colname <- c( 'anio', 'id_ben', 'tip_serv', 'diagnostico', 'tip_ben', 'sexo', 'x', 'n_reg', 'val' )
  setnames( as400, colname )
  as400[ , bd := factor( 'AS400', levels = c( 'AS400', 'SOAM' ) ) ]
  as400[ , anio := as.integer( anio ) ]
  as400[ , x := as.integer( x ) ]
  as400[ , sexo := factor( sexo, levels = c( 'F', 'M' ) ) ]
  as400[ , val := as.numeric( gsub( '\\,', '.', val ) ) ]
  
  ## Beneficios totales ----------------------------------------------------------------------------
  message( '\tGenerando pago de beneficios totales para el año: ', anios[ i ] )
  cols <- intersect( names( soam ), names( as400 ) )
  beneficios <- rbindlist( list( 
    soam[ , cols, with = FALSE ], 
    as400[ , cols, with = FALSE ] ), 
    use.names = TRUE )
  beneficios[ , id_ben := iconv( id_ben, "UTF-8", "UTF-8", sub = '' ) ]
  
  ## Guardando beneficios totales ------------------------------------------------------------------
  message( '\tGuardando beneficios para el año: ', anios[ i ] )
  file <- paste0( parametros$RData_seg, 'IESS_SAL_beneficios_pagos_totales_', anios[ i ], '.RData' )
  save( soam, as400, beneficios, file = file )
  
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

anios <- seq( 2022, 2014, -1 )

beneficios_anio <- NULL
for ( i in 1:length( anios ) ) {
  
  file <- paste0( parametros$RData_seg, 'IESS_SAL_beneficios_pagos_totales_', anios[ i ], '.RData' )
  load( file )
  rm( soam, as400 )
  
  beneficios[ , qv := quantile( val, 0.9999 ), by = list( sexo, tip_serv, x ) ]
  beneficios <- beneficios[ val > 0 & val <= qv ]
  beneficios_anio <- rbind( 
    beneficios_anio,
    beneficios[ , list( anio = anios[ i ], X = sum( val, na.rm = TRUE ) ) ] )

}
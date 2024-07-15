# Borro ambiente
message( paste( rep('-', 100 ), collapse = '' ) )
# rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
rm( list = ls() )
gc()
# FIN

# ACTUALIZACION DE HIPOTESIS Y PARAMETROS INICIALES ----------------------------
# Hipotesis
load( paste0( "Y:/IESS_2020/RData/", 'IESS_macro_estudio.RData' ) )
rm( parametros )

Hipotesis <- as.data.table( Hipotesis )
Hipotesis[ 4, 2 ] <- 0.02533 # Crecimiento_Salarial
Hipotesis[ 5, 2 ] <- 0.02406 # Crecimiento_SBU
Hipotesis[ 7, 2 ] <- 0.01980 # Inflacion

Hipotesis <- as.data.frame( Hipotesis )

save( list = c( ls(all = TRUE) ),
      file = paste0( "Y:/IESS_2020/RData/", "IESS_macro_estudio.RData")
      )
# FIN ACTUALIZACION ---------------------------------------------------------------

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls() )
gc()

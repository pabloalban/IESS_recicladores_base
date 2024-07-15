message( paste( rep('-', 100 ), collapse = '' ) )

# Número de threads que utilizará data table para operar
setDTthreads( threads = 10 )

# Carga de atenciones médicas ----------------------------------------------------------------------
years <- 2013:2022

for ( y in years ) { # y <- 2013
  
  file <- paste0( parametros$Data, 'SAL/', paste0( y, '_FACT.txt' ) )
  atenciones_fact <- fread( file, header = TRUE, sep = '|', showProgress = TRUE )
  atenciones_fact <- as.data.table( atenciones_fact )
  colnam <- c( 'anio', 'cedula', 'grup_tip_serv', 'diag', 'tip_ben', 'sexo', 'x', 'n_reg', 'monto' )
  setnames( atenciones_fact, colnam )
  
  file <- paste0( parametros$Data, 'SAL/', paste0( y, '_SOAM.txt' ) )
  atenciones_soam <- fread( file, header = TRUE, sep = '|', showProgress = TRUE )
  atenciones_soam <- as.data.table( atenciones_soam )
  colnam <- c( 'anio', 'cedula', 'monto', 'x', 'sexo', 'diag', 'tip_serv', 'tip_ben', 'x1', 'grup_tip_serv' )
  setnames( atenciones_soam, colnam )
  
  save( atenciones_fact, atenciones_soam, 
        file = paste0( parametros$RData_dem, 'IESS_SAL_atenciones_', y, '.RData' ) )
  
}
# ascii = FALSE, compress = 'bzip2', compression_level = 9 

# # Carga anterior, para estudio 2018 --------------------------------------------------------------
# gc()
# cat <- fread( paste0( parametros$Data_seg, 'anexo1_enfermedades_catastroficas_cie10.csv')
#               , header = TRUE, sep = 'auto', showProgress = TRUE 
# )
# View(head(afi_sal))
# cat <- cat[ , list( DIAG_PRINCIPAL = `CIE-10`, TIPO_ENFERMEDAD_CAT = 'C' ) ]
# afi_sal <- merge( afi_sal, cat, all.x = TRUE, by = 'DIAG_PRINCIPAL' ) # 55710439
# afi_sal[ , TIPO_ENFERMEDAD := 'E' ]
# afi_sal[ TIPO_ENFERMEDAD_CAT == 'C', TIPO_ENFERMEDAD := 'C' ]
# gc()
# afi_sal$TIPO_ENFERMEDAD_CAT <- NULL
# #.....
# 
# save( afi_sal, file = paste0( parametros$RData_seg, 'IESS_SAL_afiliados.RData' ),
#       ascii = FALSE, compress = 'bzip2', compression_level = 9 )
# 
# rm( afi_sal )
# gc()
# 
# file <- paste0( parametros$Data_seg, 'pensionistas.txt' )
# pen_sal <- fread( file, header = TRUE, sep = '|', showProgress = TRUE )
# pen_sal <- data.table( pen_sal )
# #.....
# gc()
# View(head(pen_sal))
# 
# pen_sal <- merge( pen_sal, cat, all.x = TRUE, by = 'DIAG_PRINCIPAL' ) # 55710439
# pen_sal[ , TIPO_ENFERMEDAD := 'E' ]
# pen_sal[ TIPO_ENFERMEDAD_CAT == 'C', TIPO_ENFERMEDAD := 'C' ]
# gc()
# pen_sal$TIPO_ENFERMEDAD_CAT <- NULL
# #.....
# save( pen_sal, file = paste0( parametros$RData_seg, 'IESS_SAL_pensionistas.RData' ),
#       ascii = FALSE, compress = 'bzip2', compression_level = 9 )
# 
# file <- paste0( parametros$Data_seg, 'hijos_18.txt' )
# hijos_sal <- fread( file, header = TRUE, sep = ';', showProgress = TRUE )
# hijos_sal <- data.table( hijos_sal )
# #.....
# gc()
# View(head(hijos_sal))
# setnames( cat, 'DIAG_PRINCIPAL', 'DIAG' )
# hijos_sal <- merge( hijos_sal, cat, all.x = TRUE, by = 'DIAG' ) # 55710439
# hijos_sal[ , TIP_ENF := 'E' ]
# hijos_sal[ TIPO_ENFERMEDAD_CAT == 'C', TIP_ENF := 'C' ]
# gc()
# hijos_sal$TIPO_ENFERMEDAD_CAT <- NULL
# #.....
# save( hijos_sal, file = paste0( parametros$RData_seg, 'IESS_SAL_hijos.RData' ),
#       ascii = FALSE, compress = 'bzip2', compression_level = 9 )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

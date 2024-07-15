message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de datos proyectado de salud' )
path <- parametros$Data_seg
file <- paste0( path, 'prestaciones_medicas_2019_11_08.xlsx' )

# Leyendo expuestos al riesgo ----------------------------------------------------------------------
message( '\tLeyendo expuestos al riesgo' )
pre_med_er <- read_xlsx( path = file, sheet = 1, 
                         range = 'A1:U1585', col_names = TRUE, skip = 0,
                         col_types = c( rep( 'text', 5 ), rep( 'numeric', 16 ) ) )

pre_med_er <- data.table( pre_med_er )
setnames( pre_med_er, tolower( names( pre_med_er ) ) )
setnames( pre_med_er, 'capitulo', 'cap' )

pre_med_er <- melt.data.table( data = pre_med_er, 
                               id.vars = c( 'tip_pac', 'tip_ser', 'cap', 'sexo', 'edad' ),
                               variable.name = 'anio',
                               value.name = 'ER' )

# Leyendo número de pacientes ----------------------------------------------------------------------
message( '\tLeyendo número de pacientes' )
pre_med_n <- read_xlsx( path = file, sheet = 2, 
                        range = 'A1:U1585', col_names = TRUE, skip = 0,
                        col_types = c( rep( 'text', 5 ), rep( 'numeric', 16 ) ) )

pre_med_n <- data.table( pre_med_n )
setnames( pre_med_n, tolower( names( pre_med_n ) ) )
setnames( pre_med_n, 'capitulo', 'cap' )

pre_med_n <- melt.data.table( data = pre_med_n, 
                              id.vars = c( 'tip_pac', 'tip_ser', 'cap', 'sexo', 'edad' ),
                              variable.name = 'anio',
                              value.name = 'n' )

# Leyendo tasa uso ---------------------------------------------------------------------------------
message( '\tLeyendo tasa de uso' )
pre_med_uso <- read_xlsx( path = file, sheet = 3, 
                          range = 'A1:U1585', col_names = TRUE, skip = 0,
                          col_types = c( rep( 'text', 5 ), rep( 'numeric', 16 ) ) )

pre_med_uso <- data.table( pre_med_uso )
setnames( pre_med_uso, tolower( names( pre_med_uso ) ) )
setnames( pre_med_uso, 'capitulo', 'cap' )

pre_med_uso <- melt.data.table( data = pre_med_uso, 
                                id.vars = c( 'tip_pac', 'tip_ser', 'cap', 'sexo', 'edad' ),
                                variable.name = 'anio',
                                value.name = 'r' )

# Leyendo frecuencia uso ---------------------------------------------------------------------------
message( '\tLeyendo frecuencia de uso' )
pre_med_fre <- read_xlsx( path = file, sheet = 4, 
                          range = 'A1:U1585', col_names = TRUE, skip = 0,
                          col_types = c( rep( 'text', 5 ), rep( 'numeric', 16 ) ) )

pre_med_fre <- data.table( pre_med_fre )
setnames( pre_med_fre, tolower( names( pre_med_fre ) ) )
setnames( pre_med_fre, 'capitulo', 'cap' )

pre_med_fre <- melt.data.table( data = pre_med_fre, 
                                id.vars = c( 'tip_pac', 'tip_ser', 'cap', 'sexo', 'edad' ),
                                variable.name = 'anio',
                                value.name = 'f' )

# Leyendo días de estadia --------------------------------------------------------------------------
message( '\tLeyendo días de estadia' )
pre_med_est <- read_xlsx( path = file, sheet = 5, 
                          range = 'A1:U1585', col_names = TRUE, skip = 0,
                          col_types = c( rep( 'text', 5 ), rep( 'numeric', 16 ) ) )

pre_med_est <- data.table( pre_med_est )
setnames( pre_med_est, tolower( names( pre_med_est ) ) )
setnames( pre_med_est, 'capitulo', 'cap' )

pre_med_est <- melt.data.table( data = pre_med_est, 
                                id.vars = c( 'tip_pac', 'tip_ser', 'cap', 'sexo', 'edad' ),
                                variable.name = 'anio',
                                value.name = 'D' )

# Leyendo severidad --------------------------------------------------------------------------------
message( '\tLeyendo severidad' )
pre_med_sev <- read_xlsx( path = file, sheet = 6, 
                          range = 'A1:U1585', col_names = TRUE, skip = 0,
                          col_types = c( rep( 'text', 5 ), rep( 'numeric', 16 ) ) )

pre_med_sev <- data.table( pre_med_sev )
setnames( pre_med_sev, tolower( names( pre_med_sev ) ) )
setnames( pre_med_sev, 'capitulo', 'cap' )

pre_med_sev <- melt.data.table( data = pre_med_sev, 
                                id.vars = c( 'tip_pac', 'tip_ser', 'cap', 'sexo', 'edad' ),
                                variable.name = 'anio',
                                value.name = 'X' )

# Leyendo beneficios -------------------------------------------------------------------------------
message( '\tLeyendo beneficios proyectados' )
pre_med_ben <- read_xlsx( path = file, sheet = 7, 
                          range = 'A1:U1585', col_names = TRUE, skip = 0,
                          col_types = c( rep( 'text', 5 ), rep( 'numeric', 16 ) ) )

pre_med_ben <- data.table( pre_med_ben )
setnames( pre_med_ben, tolower( names( pre_med_ben ) ) )
setnames( pre_med_ben, 'capitulo', 'cap' )

pre_med_ben <- melt.data.table( data = pre_med_ben, 
                                id.vars = c( 'tip_pac', 'tip_ser', 'cap', 'sexo', 'edad' ),
                                variable.name = 'anio',
                                value.name = 'B' )

# Uniendo resultados -------------------------------------------------------------------------------
mer_vect <- c( 'tip_pac', 'tip_ser', 'cap', 'sexo', 'edad', 'anio' )
pre_med <- merge( pre_med_er, pre_med_n, by = mer_vect )
pre_med <- merge( pre_med, pre_med_uso, by = mer_vect )
pre_med <- merge( pre_med, pre_med_fre, by = mer_vect )
pre_med <- merge( pre_med, pre_med_est, by = mer_vect )
pre_med <- merge( pre_med, pre_med_sev, by = mer_vect )
pre_med <- merge( pre_med, pre_med_ben, by = mer_vect )

message( '\tGuardando información' )
save( pre_med, file = paste0( parametros$RData_seg, 
                              'IESS_SAL_informacion_proyecciones_anteriores.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

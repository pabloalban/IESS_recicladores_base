message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tEstimación resultados de salud' )

load( paste0( parametros$RData_seg, 'IESS_SAL_afiliados_filtrado.RData' ) )  # OjO : tiene más de 53 millones de registros
load( paste0( parametros$RData_seg, 'IESS_SAL_pensionistas_filtrado.RData' ) )  # OjO : tiene más de 11.5 millones de registros
load( paste0( parametros$RData_seg, 'IESS_SAL_hijos_filtrado.RData' ) )  # OjO : tiene más de 18.5 millones de registros

afi_proc <- copy( afi_sal[ , list( t = fec_ing, grupo = 'a', id, sexo, x, u, enf, ser, cap, icd, D, X ) ] )
rm( afi_sal )
pen_proc <- copy( pen_sal[ , list( t = fec_ing, grupo = 'p', id, sexo, x, u, enf, ser, cap, icd, D, X ) ] )
rm( pen_sal )
hij_proc <- copy( hijos_sal[ , list( t = fec_ing, grupo = 'h', id, sexo, x, u, enf, ser, cap, icd, D, X ) ] )
rm( hijos_sal )
gc()

# Uniendo resultados
proc <- rbind( afi_proc, pen_proc, hij_proc )
rm( afi_proc, pen_proc, hij_proc )
gc()

proc <- proc[ sexo %in% c( 'F', 'M' ) ]
proc[ ser != 'HO', D := 1 ]
proc <- proc[ D > 0 ]

# proc <- proc[ , list( X = sum( X ), 
#                       D = mean( D ) ),
#               by = list( t, id, sexo, x, u, enf, ser, cap, icd ) ]

# Estimación duración, severidad y probabilidad ----------------------------------------------------
ben_est_sev <- proc[ , list( N = as.numeric( sum( .N, na.rm = TRUE ) ),
                             D = sum( D, na.rm = TRUE ),
                             D2 = sum( D^2, na.rm = TRUE ),
                             X = sum( X, na.rm = TRUE ),
                             X2 = sum( X^2, na.rm = TRUE ),
                             LD = sum( log( D ), na.rm = TRUE ),
                             LD2 = sum( log( D )^2, na.rm = TRUE ),
                             LX = sum( log( X ), na.rm = TRUE ),
                             LX2 = sum( log( X )^2, na.rm = TRUE ) ), 
                     by = list( sexo,
                                u,
                                enf, 
                                ser, 
                                cap,
                                icd ) ]


setorder( ben_est_sev, sexo, u, enf, ser, -N )

ben_est_sev[ , Y := 1 ]
ben_est_sev[ , Y := cumsum( Y ), list( sexo, u, enf, ser ) ]
ben_est_sev[ Y > 10, icd := 'OTRAS' ]
ben_est_sev <- ben_est_sev[ , list( N = sum( N, na.rm = TRUE ),
                                    D = sum( D, na.rm = TRUE ),
                                    D2 = sum( D2, na.rm = TRUE ),
                                    LD = sum( LD, na.rm = TRUE ),
                                    LD2 = sum( LD2, na.rm = TRUE ),
                                    LX = sum( LX, na.rm = TRUE ),
                                    LX2 = sum( LX2, na.rm = TRUE ),
                                    X = sum( X, na.rm = TRUE ),
                                    X2 = sum( X2, na.rm = TRUE ) ), 
                            by = list( sexo,
                                       u,
                                       enf, 
                                       ser, 
                                       cap,
                                       icd ) ]

# Duración promedio
ben_est_sev[ ser == 'HO' & D / N >= 1, ED := 365 * D / ( 6 * N )  ]
ben_est_sev[ ser == 'HO' & D / N < 1, ED := 365 * D / N  ]
ben_est_sev[ ser != 'HO', ED := 1  ]

# Severidad promedio
ben_est_sev[ ser == 'CE', EX := X / N ]
ben_est_sev[ ser == 'EM', EX := X / N ]
ben_est_sev[ ser == 'HO', EX := X / ( N * ED ) ]

# Probabilidades de transición
# Probabilidad transicion de enfermo a un estado de gravedad
ben_est_sev[ , q_c := sum( N ), by = list( sexo, u ) ]
ben_est_sev[ , q_c := sum( N ) / q_c, by = list( sexo, u, enf ) ]

# Probabilidad transicion de un estado de gravedad a ser atendido en un determinado servicio
ben_est_sev[ , q_s := sum( N ), by = list( sexo, u, enf ) ]
ben_est_sev[ , q_s := sum( N ) / q_s, by = list( sexo, u, enf, ser ) ]

# Probabilidad transicion de ser atendido en determinado servicio y presentar una cierta patología
ben_est_sev[ , q_p := sum( N ), by = list( sexo, u, enf, ser ) ]
ben_est_sev[ , q_p := N / q_p, by = list( sexo, u, enf, ser, cap, icd ) ]

ben_est_sev <- ben_est_sev[ , list( sexo, u, enf, ser, cap, icd, N, 
                                    D, D2, LD, LD2, X, X2, LX, LX2, ED, EX, 
                                    q_p, q_s, q_c ) ]

setorder( ben_est_sev, sexo, u, enf, ser, -N )

gc()

# Estimación frecuencia ----------------------------------------------------------------------------
ben_est_fre <- proc[ , list( n = as.numeric( .N ),
                             D = sum( D, na.rm = TRUE ),
                             X = sum( X, na.rm = TRUE ) ),
                     by = list( sexo,
                                u,
                                enf,
                                t ) ]

setorder( ben_est_fre, enf, sexo, u, t )
ben_est_fre[ , ts := shift( t, n = -1 ), by = list( sexo, u, enf ) ]
ben_est_fre[ , W := interval( t, ts ) / dyears( 1 ), by = list( sexo, u, enf ) ]
ben_est_fre[ , ts := NULL ]
ben_est_fre <- ben_est_fre[ !is.na( W ) ]
ben_est_fre[ , L := cumsum( W ), by = list( sexo, u, enf ) ]
ben_est_fre[ , S := cumsum( X ), by = list( sexo, u, enf ) ]
ben_est_fre[ , U := cumsum( D ), by = list( sexo, u, enf ) ]
ben_est_fre[ , N := cumsum( n ), by = list( sexo, u, enf ) ]
ben_est_fre[ , lambda := N / L ]

# Cargando población -------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_V3_informacion_demografica.RData' ) )
load( paste0( parametros$RData, 'INEC_censo_iess_fertilidad_alisado_2010.RData' ) )

cot <- cotizantes[ sector %in% c( 'PRI', 'PRI-SSC', 'PRI-VOL', 'PRI-VOL-SSC', 'PUB', 'PUB-PRI', 
                                  'PUB-PRI-SSC', 'PUB-PRI-VOL', 'PUB-SSC', 'PUB-VOL', 'VOL', 
                                  'VOL-SSC', 'PUB-VOL-SSC', 'PUB-PRI-VOL-SSC', 'PRI-TNRH', 
                                  'PRI-TNRH-SSC', 'PRI-VOL-TNRH', 'PUB-TNRH', 'PUB-TNRH-SSC', 
                                  'VOL-TNRH', 'PUB-PRI-TNRH', 'VOL-TNRH-SSC', 'PUB-VOL-TNRH' ) ]
cot <- cot[ t >= 2013 & x >= 15 & x <= 110, list( l = sum( exposicion_riesgo ) ), by = list( t, sexo, x ) ]

pen <- pensionistas[ seg %in% c( 'SGO', 'SGRT' ) & x >= 18 & x <= 110 ]
pen <- pen[ , list( l = sum( ER ) ), by = list( t, sexo, x ) ]

mon <- montepios[ seg %in% c( 'SGO', 'SGRT' ) & x >= 18 & x <= 110 ]     
mon <- mon[ , list( l = sum( ER ) ), by = list( t, sexo, x ) ]

dep <- cen_iess_hij_alis[ y < 18 ]
dep[ sexo == 'M', sexo := 'F' ]
dep[ sexo == 'H', sexo := 'M' ]
dep[ sexo_dep == 'M', sexo_dep := 'F' ]
dep[ sexo_dep == 'H', sexo_dep := 'M' ]
dep <- merge( dep[ , list( x, y, q, sexo, sexo_dep ) ], 
              cot, by = c( 'x', 'sexo' ), allow.cartesian = TRUE )
dep[ , x := NULL ]
dep[ , sexo := NULL ]
dep <- dep[ , list( l = sum( l * q ) ), by = list( t, sexo = sexo_dep, x = y ) ]
setorder( dep, t, sexo, x )

pob <- rbind( cot, pen, mon, dep )
pob <- pob[ , list( l = sum( l ) ), by = list( t, sexo, x ) ]
pob_cat <- copy( pob )
pob[ , u := cut( x, 
                 breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                 include.lowest = TRUE, right = FALSE, 
                 ordered_result = FALSE ) ]
pob[ , u := as.character( u ) ]
pob <- pob[ , list( l = sum( l ) ), by = list( y = t, sexo, u ) ]
pob[ , enf := 'E' ]

pob_cat[ , u := cut( x, 
                     breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                     include.lowest = TRUE, right = FALSE, 
                     ordered_result = FALSE ) ]
pob_cat[ , u := as.character( u ) ]
pob_cat <- pob_cat[ , list( l = sum( l ) ), by = list( y = t, sexo, u ) ]
pob_cat[ , enf := 'C' ]

pob <- rbind( pob, pob_cat )

ben_est_fre[ , y := year( t ) ]
ben_est_fre <- merge( ben_est_fre, pob, by = c( 'y', 'sexo', 'u', 'enf' ), all.x = TRUE )

ben_est_fre[ !is.na( l ), lambda := lambda / l ]
ben_est_fre[ is.na( l ), lambda := 1 ]
ben_est_fre[ , lambda_mean := mean( lambda ), by = list( sexo, u, enf ) ]
ben_est_fre[ , t_max := max( t ), by = list( sexo, u, enf ) ]

setorder( ben_est_fre, enf, sexo, u, t )

# Agregación por grupos de afiliados ---------------------------------------------------------------
save( ben_est_sev, ben_est_fre,
      file = paste0( parametros$RData_seg, 'IESS_SAL_estimacion.RData' ) )

save( proc,
      file = paste0( parametros$RData_seg, 'IESS_SAL_filtrado_total.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

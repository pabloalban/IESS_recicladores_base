# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )

# ATENCIÓN!! OBJETO MUY PESADO----------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SAL_filtrado_total.RData' ) )
# RESULTA EL OBJETO "proc"
load( file = paste0( parametros$RData_seg, 'IESS_SAL_icd10_pri_niv.RData' ) )

proc <- merge( proc, icd10_pri_niv, by = 'icd', all.x = TRUE )
proc[ is.na(descripcion), descripcion := 'Otras enfermedades' ]

save_list <- NULL

# Atenciones por año -------------------------------------------------------------------------------
ben_anio <- proc[ , list( N = as.numeric( .N ),
                          D = sum( D, na.rm = TRUE ),
                          X = sum( X, na.rm = TRUE ) ),
                 by = list( y = year( t ) ) ]

setorder( ben_anio, y )
save_list <- c( save_list, 'ben_anio' )

# Atenciones por año y gravedad --------------------------------------------------------------------
ben_enf <- proc[ , list( N = as.numeric( .N ),
                         D = sum( D, na.rm = TRUE ),
                         X = sum( X, na.rm = TRUE ) ),
                 by = list( y = year( t ), enf ) ]

ben_enf[ , N := N / sum( N, na.rm = TRUE ), by = y ]
ben_enf[ , D := D / sum( D, na.rm = TRUE ), by = y ]
ben_enf[ , X := X / sum( X, na.rm = TRUE ), by = y ]
ben_enf[ enf == "C", enf := 'Catastrófico' ]
ben_enf[ enf == "E", enf := 'No catastrófico' ]
setorder( ben_enf, y, enf )

save_list <- c( save_list, 'ben_enf' )

# Atenciones por año y servicio --------------------------------------------------------------------
ben_ser <- proc[ , list( N = as.numeric( .N ),
                         D = sum( D, na.rm = TRUE ),
                         X = sum( X, na.rm = TRUE ) ),
                 by = list( y = year( t ), ser ) ]

ben_ser[ , N := N / sum( N, na.rm = TRUE ), by = y ]
ben_ser[ , D := D / sum( D, na.rm = TRUE ), by = y ]
ben_ser[ , X := X / sum( X, na.rm = TRUE ), by = y ]

setorder( ben_ser, y, ser )
save_list <- c( save_list, 'ben_ser' )

# Atenciones por sexo y servicio -------------------------------------------------------------------
ben_sex_ser <- proc[ , list( N = as.numeric( .N ),
                             D = sum( D, na.rm = TRUE ),
                             X = sum( X, na.rm = TRUE ) ),
                     by = list( sexo, ser ) ]

ben_sex_ser[ , N := N / sum( N, na.rm = TRUE ), by = sexo ]
ben_sex_ser[ , D := D / sum( D, na.rm = TRUE ), by = sexo ]
ben_sex_ser[ , X := X / sum( X, na.rm = TRUE ), by = sexo ]

setorder( ben_sex_ser, ser )
save_list <- c( save_list, 'ben_sex_ser' )

# Atenciones por grupo edad ------------------------------------------------------------------------
ben_u <- proc[ , list( N = as.numeric( .N ),
                       D = sum( D, na.rm = TRUE ),
                       X = sum( X, na.rm = TRUE ) ),
               by = list( enf, u ) ]

ben_u[ , N := N / sum( N, na.rm = TRUE ), by = enf ]
ben_u[ , D := D / sum( D, na.rm = TRUE ), by = enf ]
ben_u[ , X := X / sum( X, na.rm = TRUE ), by = enf ]

setorder( ben_u, enf, u )
save_list <- c( save_list, 'ben_u' )

# Atenciones por capítulo --------------------------------------------------------------------------
ben_cap <- proc[ , list( N = as.numeric( .N ),
                         D = sum( D, na.rm = TRUE ),
                         X = sum( X, na.rm = TRUE ) ),
                 by = list( y = year( t ), cap ) ]

ben_cap[ , N := N / sum( N, na.rm = TRUE ), by = y ]
ben_cap[ , D := D / sum( D, na.rm = TRUE ), by = y ]
ben_cap[ , X := X / sum( X, na.rm = TRUE ), by = y ]

setorder( ben_cap, y, cap )
save_list <- c( save_list, 'ben_cap' )

# Atenciones por icd y año -------------------------------------------------------------------------
ben_icd_anio <- proc[ , list( N = as.numeric( .N ),
                              D = sum( D, na.rm = TRUE ),
                              X = sum( X, na.rm = TRUE ) ),
                      by = list( y = year( t ), descripcion ) ]

# ben_icd_anio[ , N := N / sum( N, na.rm = TRUE ), by = y ]
# ben_icd_anio[ , D := D / sum( D, na.rm = TRUE ), by = y ]
# ben_icd_anio[ , X := X / sum( X, na.rm = TRUE ), by = y ]

setorder( ben_icd_anio, y )
save_list <- c( save_list, 'ben_icd_anio' )

# Atenciones por icd y sexo ------------------------------------------------------------------------
ben_icd_sex <- proc[ , list( N = as.numeric( .N ),
                             D = sum( D, na.rm = TRUE ),
                             X = sum( X, na.rm = TRUE ) ),
                     by = list( sexo, descripcion ) ]
ben_icd_sex[ , N := N / sum( N, na.rm = TRUE ), by = sexo ]
ben_icd_sex[ , D := D / sum( D, na.rm = TRUE ), by = sexo ]
ben_icd_sex[ , X := X / sum( X, na.rm = TRUE ), by = sexo ]

setorder( ben_icd_sex, sexo, descripcion )
save_list <- c( save_list, 'ben_icd_sex' )

# Atenciones por icd - N ---------------------------------------------------------------------------
ben_icd_N <- proc[ , list( N = as.numeric( .N ),
                           D = sum( D, na.rm = TRUE ),
                           X = sum( X, na.rm = TRUE )),
                   by = list( descripcion ) ]
ben_icd_N[ , N := N / sum( N, na.rm = TRUE ) ]
ben_icd_N[ , D := D / sum( D, na.rm = TRUE ) ]
ben_icd_N[ , X := X / sum( X, na.rm = TRUE ) ]

setorder( ben_icd_N, -N )
save_list <- c( save_list, 'ben_icd_N' )

# Atenciones por icd - grupo de edad ---------------------------------------------------------------
ben_icd_u <- proc[ , list( N = as.numeric( .N ),
                           D = sum( D, na.rm = TRUE ),
                           X = sum( X, na.rm = TRUE )),
                   by = list( enf, u, descripcion ) ]
# ben_icd_u[ , N := N / sum( N, na.rm = TRUE ), by = descripcion ]
# ben_icd_u[ , D := D / sum( D, na.rm = TRUE ), by = descripcion ]
# ben_icd_u[ , X := X / sum( X, na.rm = TRUE ), by = descripcion ]

setorder( ben_icd_u, u )
save_list <- c( save_list, 'ben_icd_u' )

# Estadísticas por sexo ----------------------------------------------------------------------------
ben_sexo <- proc[ , list( N = as.numeric( .N ),
                          D = sum( D, na.rm = TRUE ),
                          X = sum( X, na.rm = TRUE ) ),
                  by = list( sexo ) ]

ben_sexo[ , N := N / sum( N, na.rm = TRUE ) ]
ben_sexo[ , D := D / sum( D, na.rm = TRUE ) ]
ben_sexo[ , X := X / sum( X, na.rm = TRUE ) ]

setorder( ben_sexo, sexo )
save_list <- c( save_list, 'ben_sexo' )

# Estadísticas por sexo y año ----------------------------------------------------------------------
ben_sexo_anio <- proc[ , list( N = as.numeric( .N ),
                               D = sum( D, na.rm = TRUE ),
                               X = sum( X, na.rm = TRUE ) ),
                       by = list( y = year( t ), sexo ) ]

ben_sexo_anio[ , N := N / sum( N, na.rm = TRUE ), by = y ]
ben_sexo_anio[ , D := D / sum( D, na.rm = TRUE ), by = y ]
ben_sexo_anio[ , X := X / sum( X, na.rm = TRUE ), by = y ]

setorder( ben_sexo_anio, y, sexo )
save_list <- c( save_list, 'ben_sexo_anio' )

# Estadísticas del tiempo de estadía por año -------------------------------------------------------
ben_est_anio_D <- proc[ ser == "HO", list( MIN = 365.25 * min( D ),
                                           Q25 = 365.25 * quantile( D, probs = 0.25 ),
                                           MED = 365.25 * median( D ),
                                           MEAN = 365.25 * mean( D ),
                                           SD = 365.25 * sd( D ),
                                           Q75 = 365.25 * quantile( D, probs = 0.75 ),
                                           MAX = 365.25 * max( D ) ),
                        list( y = year( t ), enf, ser ) ]
setorder( ben_est_anio_D, y, enf, ser )
save_list <- c( save_list, 'ben_est_anio_D' )

# Estadísticas del tiempo de estadía por ICD -------------------------------------------------------
ben_est_icd_D <- proc[ ser == "HO", list( MIN = 365.25 * min( D ),
                                          Q25 = 365.25 * quantile( D, probs = 0.25 ),
                                          MED = 365.25 * median( D ),
                                          MEAN = 365.25 * mean( D ),
                                          SD = 365.25 * sd( D ),
                                          Q75 = 365.25 * quantile( D, probs = 0.75 ),
                                          MAX = 365.25 * max( D ) ),
                       list( descripcion ) ]
setorder( ben_est_icd_D, MEAN )
save_list <- c( save_list, 'ben_est_icd_D' )

# Estadísticas del tiempo de estadía por grupo de edad----------------------------------------------
ben_est_u_D <- proc[ ser == "HO", list( MIN = 365.25 * min( D ),
                                        Q25 = 365.25 * quantile( D, probs = 0.25 ),
                                        MED = 365.25 * median( D ),
                                        MEAN = 365.25 * mean( D ),
                                        SD = 365.25 * sd( D ),
                                        Q75 = 365.25 * quantile( D, probs = 0.75 ),
                                        MAX = 365.25 * max( D ) ),
                     list( enf, u ) ]
setorder( ben_est_u_D, enf, u )
save_list <- c( save_list, 'ben_est_u_D' )

# Estadísticas del beneficio por año ---------------------------------------------------------------
ben_est_anio_X <- proc[ , list( MIN = min( X ),
                                Q25 = quantile( X, probs = 0.25 ),
                                MED = median( X ),
                                MEAN = mean( X ),
                                SD = sd( X ),
                                Q75 = quantile( X, probs = 0.75 ),
                                MAX = max( X ) ),
                        list( y = year( t ), enf, ser ) ]

setorder( ben_est_anio_X, y, enf, ser )
save_list <- c( save_list, 'ben_est_anio_X' )

# Estadísticas del beneficio por icd ---------------------------------------------------------------
ben_est_icd_X <- proc[ , list( MIN = min( X ),
                               Q25 = quantile( X, probs = 0.25 ),
                               MED = median( X ),
                               MEAN = mean( X ),
                               SD = sd( X ),
                               Q75 = quantile( X, probs = 0.75 ),
                               MAX = max( X ) ),
                       list( descripcion ) ]

setorder( ben_est_icd_X, MEAN )
save_list <- c( save_list, 'ben_est_icd_X' )

# Estadísticas del beneficio por grupo de edad -----------------------------------------------------
ben_est_u_X <- proc[ , list( MIN = min( X ),
                             Q25 = quantile( X, probs = 0.25 ),
                             MED = median( X ),
                             MEAN = mean( X ),
                             SD = sd( X ),
                             Q75 = quantile( X, probs = 0.75 ),
                             MAX = max( X ) ),
                     list( enf, u ) ]
ben_est_u_X[ enf == "C", enf := 'Catastrófico' ]
ben_est_u_X[ enf == "E", enf := 'No catastrófico' ]
setorder( ben_est_u_X, enf, u )
save_list <- c( save_list, 'ben_est_u_X' )

# Estadísticas del beneficio por servicio ----------------------------------------------------------
ben_est_ser_X <- proc[ , list( MIN = min( X ),
                               Q25 = quantile( X, probs = 0.25 ),
                               MED = median( X ),
                               MEAN = mean( X ),
                               SD = sd( X ),
                               Q75 = quantile( X, probs = 0.75 ),
                               MAX = max( X ) ),
                       list( ser ) ]

setorder( ben_est_ser_X, ser )
save_list <- c( save_list, 'ben_est_ser_X' )

# Estadísticas del beneficio por capítulo ----------------------------------------------------------
ben_est_cap_X <- proc[ , list( MIN = min( X ),
                               Q25 = quantile( X, probs = 0.25 ),
                               MED = median( X ),
                               MEAN = mean( X ),
                               SD = sd( X ),
                               Q75 = quantile( X, probs = 0.75 ),
                               MAX = max( X ) ),
                       list( cap ) ]

setorder( ben_est_cap_X, cap )
save_list <- c( save_list, 'ben_est_cap_X' )

# Guardando estadísticas de salud ------------------------------------------------------------------
save( list = save_list, 
      file = paste0( parametros$RData_seg, 'IESS_SAL_estadisticas_beneficios.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

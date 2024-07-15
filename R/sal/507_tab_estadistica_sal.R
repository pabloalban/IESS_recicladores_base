message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tTablas de estimaciones del modelo de beneficios para salud' )

load( file = paste0( parametros$RData_seg, 'IESS_SAL_estadisticas_beneficios.RData' ) )
load( file = paste0( parametros$RData_dem, 'INEC_censo_iess_fertilidad_alisado_2010.RData' ) )

# Función de tildes a latex-------------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Clases -------------------------------------------------------------------------------------------
class_u_c <- as.character( cut( c( 0, 5, seq( 20, 60, 20 ) ), 
                                breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                                include.lowest = TRUE, right = FALSE, 
                                ordered_result = FALSE ) )

class_u_e <- as.character( cut( c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ) ), 
                                breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                                include.lowest = TRUE, right = FALSE, 
                                ordered_result = FALSE ) )
# --------------------------------------------------------------------------------------------------
# Hijos de los afiliados ---------------------------------------------------------------------------
bend_afi <- cen_2010[ aseg == 'iess_sgo' & tipo == 'H' & y<=18 ]

# Estadísticas de la edad - bendiciones ------------------------------------------------------------
afi_est_ben_e <- bend_afi[ , list( MIN = min( y ),
                                   Q25 = quantile( y, probs = 0.25 ),
                                   MED = median( y ),
                                   MEAN = mean( y ),
                                   SD = sd( y ),
                                   Q75 = quantile( y, probs = 0.75 ),
                                   MAX = max( y ) ),
                           list( sexo_dep ) ]
setorder( afi_est_ben_e, sexo_dep )

xtb_aux <- xtable( afi_est_ben_e, digits = c( 0, 0, rep(2, ncol(afi_est_ben_e)-1) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_afi_est_ben_e.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Proporción de hombres y mujeres - bendiciones ----------------------------------------------------
afi_est_ben_p <- bend_afi[ , list( N = as.numeric( .N ) ),
                       by = list( sexo_dep ) ]
afi_est_ben_p[ , N := N / sum( N, na.rm = TRUE ) ]

xtb_aux <- xtable( afi_est_ben_p, digits = c( 0, 0, rep(3, ncol(afi_est_ben_p)-1) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_afi_est_ben_p.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )


# --------------------------------------------------------------------------------------------------
# Cónyugues de los afiliados -----------------------------------------------------------------------
cony_afi <- cen_2010[ aseg == 'iess_sgo' & tipo == 'C' ]

# Estadísticas de la edad - cónyugues --------------------------------------------------------------
afi_est_cony_e <- cony_afi[ , list( MIN = min( y ),
                                   Q25 = quantile( y, probs = 0.25 ),
                                   MED = median( y ),
                                   MEAN = mean( y ),
                                   SD = sd( y ),
                                   Q75 = quantile( y, probs = 0.75 ),
                                   MAX = max( y ) ),
                           list( sexo_dep  ) ]
setorder( afi_est_cony_e, sexo_dep )

xtb_aux <- xtable( afi_est_cony_e, digits = c( 0, 0, rep(2, ncol(afi_est_cony_e)-1) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_afi_est_cony_e.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Proporción de hombres y mujeres - cónyugues ------------------------------------------------------
afi_est_cony_p <- cony_afi[ , list( N = as.numeric( .N ) ),
                           by = list( sexo_dep ) ]
afi_est_cony_p[ , N := N / sum( N, na.rm = TRUE ) ]

xtb_aux <- xtable( afi_est_cony_p, digits = c( 0, 0, rep(2, ncol(afi_est_cony_p)-1) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_afi_est_cony_p.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
# Número de atenciones por año ---------------------------------------------------------------------
aux <- ben_anio[ , list(y, N, X)]
xtb_aux <- xtable( aux, digits = c( 0, 0, rep(2, ncol(aux)-1) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_anio.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Estadísticas del tiempo de estadía por año -------------------------------------------------------
ben_est_anio_D[ enf == "C", enf := 'Catastr\\\'{o}fico' ]
ben_est_anio_D[ enf == "E", enf := 'No catastr\\\'{o}fico' ]
xtb_aux <- xtable( ben_est_anio_D[ , list(y, enf, ser, Q25, MED, Q75) ], 
                   digits = c( 0, 0, rep(2, ncol(ben_est_anio_D[ , list(y, enf, ser, Q25, MED, Q75) ])-1) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_est_anio_D.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Estadísticas del tiempo de estadía por ICD -------------------------------------------------------
xtb_aux <- xtable( ben_est_icd_D[ , list(descripcion, Q25, MED, Q75) ], 
                   digits = c( 0, 0, rep(2, ncol(ben_est_icd_D[ , list(descripcion, Q25, MED, Q75) ])-1) ) )
xtb_aux <- tildes_a_latex(xtb_aux)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_est_icd_D.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Estadísticas del tiempo de estadía por grupo de edad----------------------------------------------
ben_est_u_D[ , u := factor( u, levels = c(class_u_c, class_u_e), ordered = TRUE ) ]
setorder(ben_est_u_D, u)

ben_est_u_D[ , u := paste0('$', u, '$') ]
ben_est_u_D[ enf == "C", enf := 'Catastr\\\'{o}fico' ]
ben_est_u_D[ enf == "E", enf := 'No catastr\\\'{o}fico' ]

xtb_aux <- xtable( ben_est_u_D[ , list(enf, u, Q25, MED, Q75) ], 
                   digits = c( 0, 0, rep(2, ncol(ben_est_u_D[ , list(enf, u, Q25, MED, Q75) ])-1) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_est_u_D.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = 5, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
ben_est_anio_X[ enf == "C", enf := 'Catastr\\\'{o}fico' ]
ben_est_anio_X[ enf == "E", enf := 'No catastr\\\'{o}fico' ]

xtb_aux <- xtable( ben_est_anio_X[ , list(y, enf, ser, MED, MEAN, SD) ], 
                   digits = c( 0, 0, rep(2, ncol(ben_est_anio_X[ , list(y, enf, ser, MED, MEAN, SD) ])-1) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_est_anio_X.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
xtb_aux <- xtable( ben_est_icd_X[ , list(descripcion, MED, MEAN, SD) ],
                   digits = c( 0, 0, rep(2, ncol(ben_est_icd_X[ , list(descripcion, MED, MEAN, SD) ])-1) ) )
xtb_aux <- tildes_a_latex(xtb_aux)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_est_icd_X.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Estadísticas del beneficio por grupo de edad -----------------------------------------------------
ben_est_u_X[ , u := factor( u, levels = c(class_u_c, class_u_e), ordered = TRUE ) ]
setorder(ben_est_u_X, u)

ben_est_u_X[ , u := paste0('$', u, '$') ]
xtb_aux <- xtable( ben_est_u_X[ , list(enf, u, MED, MEAN, SD) ], 
                   digits = c( 0, 0, rep(2, ncol(ben_est_u_X[ , list(enf, u, MED, MEAN, SD) ])-1) ) )
xtb_aux <- tildes_a_latex(xtb_aux)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_est_u_X.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = 5, sanitize.text.function = identity )

# Estadísticas del beneficio por servicio ----------------------------------------------------------
xtb_aux <- xtable( ben_est_ser_X[ , list(ser, MED, MEAN, SD) ], 
                   digits = c( 0, 0, rep(2, ncol(ben_est_ser_X[ , list(ser, MED, MEAN, SD) ])-1) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_est_ser_X.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
xtb_aux <- xtable( ben_est_cap_X[ , list(cap, MED, MEAN, SD) ],
                   digits = c( 0, 0, rep(2, ncol(ben_est_cap_X[ , list(cap, MED, MEAN, SD) ])-1) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_est_cap_X.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Atenciones por año y gravedad --------------------------------------------------------------------
ben_enf[ , N := 100 * N ]
ben_enf[ , D := 100 * D ]
ben_enf[ , X := 100 * X ]
ben_enf[ enf %in% c("Catastrófico", "C"), enf := 'Catastr\\\'{o}fico' ]
ben_enf[ enf %in% c("No catastrófico"), enf := 'No catastr\\\'{o}fico' ]
ben_enf[ enf %in% c("Enfermedad", "E"), enf := 'No catastr\\\'{o}fico' ]
xtb_aux <- xtable( ben_enf, digits = c( 0, 0, rep(2, ncol(ben_enf)-1) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_enf.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = c(2, 4, 6, 8, 10), sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
ben_ser[ , N := 100 * N ]
ben_ser[ , D := 100 * D ]
ben_ser[ , X := 100 * X ]
xtb_aux <- xtable( ben_ser, digits = c( 0, 0, rep(2, ncol(ben_ser)-1) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_ser.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = c(3, 6, 9, 12, 15), sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
ben_sex_ser[ , N := 100 * N ]
ben_sex_ser[ , D := 100 * D ]
ben_sex_ser[ , X := 100 * X ]
setorder( ben_sex_ser, sexo)
xtb_aux <- xtable( ben_sex_ser, digits = c( 0, 0, rep(2, ncol(ben_ser)-1) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_sex_ser.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = c(3), sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
ben_sexo_anio[ , N := 100 * N ]
ben_sexo_anio[ , D := 100 * D ]
ben_sexo_anio[ , X := 100 * X ]
xtb_aux <- xtable( ben_sexo_anio, digits = c( 0, 0, rep(2, ncol(ben_sexo_anio)-1) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_sexo_anio.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = c(2, 4, 6, 8, 10), sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
ben_icd_N[ , N := 100 * N ]
ben_icd_N[ , D := 100 * D ]
ben_icd_N[ , X := 100 * X ]
xtb_aux <- xtable( ben_icd_N, digits = c( 0, 0, rep(2, ncol(ben_icd_N)-1) ) )
xtb_aux <- tildes_a_latex(xtb_aux)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_icd_N.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
aux <- copy(ben_icd_u[ enf == 'C' ])
aux[ , N := N/1000 ]
aux[ , u := factor( u, levels = class_u_c, ordered = TRUE ) ]
setorder(aux, u)
# aux[ , u := paste0('$', u, '$') ]
aux <- dcast.data.table(data = aux, formula = descripcion ~ u, value.var = c('N'), fun.aggregate = sum)
aux <- rbind( aux, data.table( descripcion='TOTAL', aux[ , lapply(.SD, sum, na.rm = TRUE), 
                                                         .SDcols = !'descripcion' ] ) )
aux[ , TOTAL := rowSums(.SD), .SDcols = 2:ncol(aux) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, rep(2, ncol(aux)-1) ) )
xtb_aux <- tildes_a_latex(xtb_aux)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_icd_u_n_c.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = nrow(aux)-1, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
aux <- copy(ben_icd_u[ enf == 'E' ])
aux[ , N := N/1000 ]
aux[ , u := factor( u, levels = class_u_e, ordered = TRUE ) ]
setorder(aux, u)
# aux[ , u := paste0('$', u, '$') ]
aux <- dcast.data.table(data = aux, formula = descripcion ~ u, value.var = c('N'), fun.aggregate = sum)
aux <- rbind( aux, data.table( descripcion='TOTAL', aux[ , lapply(.SD, sum, na.rm = TRUE), 
                                                         .SDcols = !'descripcion' ] ) )
aux[ , TOTAL := rowSums(.SD), .SDcols = 2:ncol(aux) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, rep(2, ncol(aux)-1) ) )
xtb_aux <- tildes_a_latex(xtb_aux)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_icd_u_n_e.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = nrow(aux)-1, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
aux <- copy(ben_icd_u[ enf == 'C' ])
aux[ , X := X/1000000 ]
aux[ , u := factor( u, levels = class_u_c, ordered = TRUE ) ]
setorder(aux, u)
# aux[ , u := paste0('$', u, '$') ]
aux <- dcast.data.table(data = aux, formula = descripcion ~ u, value.var = c('X'))
aux <- rbind( aux, data.table( descripcion='TOTAL', aux[ , lapply(.SD, sum, na.rm = TRUE), 
                                                         .SDcols = !'descripcion' ] ) )
aux[ , TOTAL := rowSums(.SD), .SDcols = 2:ncol(aux) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, rep(2, ncol(aux)-1) ) )
xtb_aux <- tildes_a_latex(xtb_aux)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_icd_u_x_c.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = nrow(aux)-1, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
aux <- copy(ben_icd_u[ enf == 'E' ])
aux[ , X := X/1000000 ]
aux[ , u := factor( u, levels = class_u_e, ordered = TRUE ) ]
setorder(aux, u)
# aux[ , u := paste0('$', u, '$') ]
aux <- dcast.data.table(data = aux, formula = descripcion ~ u, value.var = c('X'))
aux <- rbind( aux, data.table( descripcion='TOTAL', aux[ , lapply(.SD, sum, na.rm = TRUE), 
                                                         .SDcols = !'descripcion' ] ) )
aux[ , TOTAL := rowSums(.SD), .SDcols = 2:ncol(aux) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, rep(2, ncol(aux)-1) ) )
xtb_aux <- tildes_a_latex(xtb_aux)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_ben_icd_u_x_e.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       hline.after = nrow(aux)-1, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
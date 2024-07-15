message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tTablas de estimaciones del modelo de beneficios para salud' )

# Carga de datos ------------------------------------------------------------------------------
load( file = parametros$sal_rdata_est_sev )

# Clases -------------------------------------------------------------------------------------------
class_u_c <- as.character( cut( c( 0, 5, seq( 20, 60, 20 ) ), 
                                breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                                include.lowest = TRUE, right = FALSE, 
                                ordered_result = FALSE ) )

class_u_e <- as.character( cut( c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ) ), 
                                breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                                include.lowest = TRUE, right = FALSE, 
                                ordered_result = FALSE ) )

# Estimación duración y severidad ------------------------------------------------------------------
# Enfermedades no catastróficas consulta externa
aux_sev_c <- copy( ben_est_sev[ enf == 'C' ] )
aux_sev_c[ , u := factor( u, levels = class_u_c, ordered = TRUE ) ]

aux_sev_e <- copy( ben_est_sev[ enf == 'E' ] )
aux_sev_e[ , u := factor( u, levels = class_u_e, ordered = TRUE ) ]

aux <- aux_sev_e[ ser == 'CE', list( sexo, u, cap, icd, p = N, ED, EX ) ]
setorder( aux, sexo, u, -p )
aux[ , p := 1000 * p / sum( p ), by = list( sexo, u ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2, 2, 2 ) )
hln <- which( !duplicated( aux[ , list( sexo, u ) ] ) )
hln <- hln[ hln > 1 ] - 1

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_est_sev_no_cat_ce.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = hln, sanitize.text.function = identity )

# Enfermedades no catastróficas emergencia
aux <- aux_sev_e[ ser == 'EM', list( sexo, u, cap, icd, p = N, ED, EX ) ]
setorder( aux, sexo, u, -p )
aux[ , p := 1000 * p / sum( p ), by = list( sexo, u ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2, 2, 2 ) )
hln <- which( !duplicated( aux[ , list( sexo, u ) ] ) )
hln <- hln[ hln > 1 ] - 1

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_est_sev_no_cat_em.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = hln, sanitize.text.function = identity )

# Enfermedades no catastróficas hospitalización
aux <- aux_sev_e[ ser == 'HO', list( sexo, u, cap, icd, p = N, ED, EX ) ]
setorder( aux, sexo, u, -p )
aux[ , p := 1000 * p / sum( p ), by = list( sexo, u ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2, 2, 2 ) )
hln <- which( !duplicated( aux[ , list( sexo, u ) ] ) )
hln <- hln[ hln > 1 ] - 1

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_est_sev_no_cat_ho.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = hln, sanitize.text.function = identity )

# Enfermedades catastróficas consulta externa
aux <- aux_sev_c[ ser == 'CE', list( sexo, u, cap, icd, p = N, ED, EX ) ]
setorder( aux, sexo, u, -p )
aux[ , p := 1000 * p / sum( p ), by = list( sexo, u ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2, 2, 2 ) )
hln <- which( !duplicated( aux[ , list( sexo, u ) ] ) )
hln <- hln[ hln > 1 ] - 1

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_est_sev_cat_ce.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = hln, sanitize.text.function = identity )

# Enfermedades catastróficas emergencia
aux <- aux_sev_c[ ser == 'EM', list( sexo, u, cap, icd, p = N, ED, EX ) ]
setorder( aux, sexo, u, -p )
aux[ , p := 1000 * p / sum( p ), by = list( sexo, u ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2, 2, 2 ) )
hln <- which( !duplicated( aux[ , list( sexo, u ) ] ) )
hln <- hln[ hln > 1 ] - 1

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_est_sev_cat_em.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = hln, sanitize.text.function = identity )

# Enfermedades catastróficas hospitalización
aux <- aux_sev_c[ ser == 'HO', list( sexo, u, cap, icd, p = N, ED, EX ) ]
setorder( aux, sexo, u, -p )
aux[ , p := 1000 * p / sum( p ), by = list( sexo, u ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2, 2, 2 ) )
hln <- which( !duplicated( aux[ , list( sexo, u ) ] ) )
hln <- hln[ hln > 1 ] - 1

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_est_sev_cat_ho.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = hln, sanitize.text.function = identity )

# Estimación frecuencia ----------------------------------------------------------------------------
# Enfermedades no catastróficas
aux_c <- copy( ben_est_fre[ enf == 'C' ] )
aux_c[ , u := factor( u, levels = class_u_c, ordered = TRUE ) ]

aux_e <- copy( ben_est_fre[ enf == 'E' ] )
aux_e[ , u := factor( u, levels = class_u_e, ordered = TRUE ) ]

aux <- aux_e[ t == t_max , list( sexo, u, enf, lambda ) ]
setorder( aux, enf, sexo, u )

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 2 ) )
hln <- which( !duplicated( aux[ , list( sexo, enf ) ] ) )
hln <- hln[ hln > 1 ] - 1

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_est_fre_no_cat.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = hln, sanitize.text.function = identity )

# Enfermedades catastróficas
aux <- aux_c[ t == t_max, list( sexo, u, enf, lambda ) ]
setorder( aux, enf, sexo, u )

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 2 ) )
hln <- which( !duplicated( aux[ , list( sexo, enf ) ] ) )
hln <- hln[ hln > 1 ] - 1

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_est_fre_cat.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = hln, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

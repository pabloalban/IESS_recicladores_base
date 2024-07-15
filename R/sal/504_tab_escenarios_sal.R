message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de parámetros escenarios' )

#---------------------------------------------------------------------------------------------------
escenario <- paste0( 'escenario_', 1:5 )
nombres <- c( 'Legal', 'Base', 'Alternativo', 'Pesimista', 'Propuesto' )

#---------------------------------------------------------------------------------------------------
var_nom <- c( 'Tasa actuarial $i_a$',
              'Tasa crecimiento salarios $i_r$',
              'Tasa crecimiento salario básico unificado $i_s$',
              'Tasa crecimiento pensiones $i_p$',
              'Tasa crecimiento auxilios de funerales $i_f$',
              'Tasa crecimiento de gastos médicos $i_m$',
              'Porcentaje aporte estatal $\\alpha_{est}^2$',
              'Porcentaje aporte estatal $\\alpha_{est}^4$',
              'Porcentaje aporte estatal $\\alpha_{est}^5$',
              'Porcentaje aporte estatal $\\alpha_{est}^7$',
              'Porcentaje aporte estatal $\\alpha_{est}^8$',
              'Porcentaje aporte estatal $\\alpha_{est}^9$',
              'Porcentaje aporte estatal $\\alpha_{est}^{11}$',
              'Porcentaje aporte estatal para catastróficas $\\alpha_{est, cat}^2$',
              'Porcentaje aporte estatal para catastróficas $\\alpha_{est, cat}^4$',
              'Porcentaje aporte estatal para catastróficas $\\alpha_{est, cat}^5$',
              'Porcentaje aporte estatal para catastróficas $\\alpha_{est, cat}^7$',
              'Porcentaje aporte estatal para catastróficas $\\alpha_{est, cat}^8$',
              'Porcentaje aporte estatal para catastróficas $\\alpha_{est, cat}^9$',
              'Porcentaje aporte estatal para catastróficas $\\alpha_{est, cat}^{11}$',
              'Porcentaje gasto administrativo' )

#---------------------------------------------------------------------------------------------------
aux_tot <- NULL
aux_geom_mean <-
  function( x, t = TRUE ) { 
    if ( t ) {
      n <- 1:length(x)
    } else {
      n <- 0:( length(x) - 1 )
    }
    y <- ( cumprod( x + 1 ) )^( 1 / n ) - 1
    return( last( y ) ) }

for( i in 1:length( escenario ) ){ # i <- 2
  
  load( file = paste0( parametros$sal_rdata_icomp_conf_esc, escenario[i], '.RData' ) )
  
  aux <- data.table( 
    nom = var_nom,
    val = c( aux_geom_mean( esc$apo_act$i_a[c(-1)] ), 
             aux_geom_mean( esc$apo_act$i_r[c(-1)] ),
             aux_geom_mean( esc$apo_act$i_sbu[c(-1)] ), 
             aux_geom_mean( esc$apo_act$i_p[c(-1)] ),
             aux_geom_mean( esc$apo_act$i_f[c(-1)] ), 
             aux_geom_mean( esc$apo_act$i_m[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_2[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_4[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_5[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_7[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_8[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_9[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_11[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_cat_2[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_cat_4[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_cat_5[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_cat_7[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_cat_8[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_cat_9[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_est_cat_11[c(-1)] ), 
             aux_geom_mean( esc$apo_act$por_apo_gas[c(-1)] ) ) )
  
  aux[ , val := 100 * val ]
  aux_tot <- cbind( aux_tot, aux[ , list( val ) ] )
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ),  )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_tab_conf_', escenario[i], '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
}

setnames( aux_tot, escenario )
aux_tot <- aux_tot[ , 1:4, with = FALSE ]
aux_tot[ , nom := var_nom ]
aux_tot <- aux_tot[ , c( ncol( aux_tot ), 1:( ncol( aux_tot ) - 1 ) ), with = FALSE ]
xtb_aux_tot <- xtable( aux_tot, digits = c( 0, 0, rep( 2, ncol( aux_tot ) - 1 ) ) )

#---------------------------------------------------------------------------------------------------
print( xtb_aux_tot,
       file = paste0( parametros$resultado_tablas, 'iess_tab_conf_escenarios.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

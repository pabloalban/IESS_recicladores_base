message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )

escenarios_lista <- paste0( 'escenario_', 1:5 )

for ( i in 1:length( escenarios_lista ) ) { # i <- 1
  
  escenario <- escenarios_lista[i]
  load( file = paste0( parametros$sal_rdata_icomp_balance, escenario, '.RData' ) )
  
  if( i == 1 ){
    # Masa salarial --------------------------------------------------------------------------------
    aux <- balance_anual[ , list( t = t + parametros$anio_ini, M ) ]
    aux[, t := as.character( t ) ]
    xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ) )
    print( xtb_aux,
           file = paste0( parametros$resultado_tablas, 'iess_balance_ms.tex' ),
           type = 'latex',
           include.colnames = FALSE, include.rownames = FALSE, 
           format.args = list( decimal.mark = ',', big.mark = '.' ), 
           only.contents = TRUE, 
           hline.after = NULL, sanitize.text.function = identity )
  }
  
  # Balance corriente ------------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, A_afi, A_est, A_ext, B, G, V_cor, V_cap ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_corriente_', escenario, '.tex' ),
         type = 'latex',
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, A2, A4, A5, A7, A8, A9, A11, A_afi, A_est, A ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_aportes_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE,
         hline.after = NULL, sanitize.text.function = identity )
  
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, B2, B_pen, B_dep, B12, B ) ]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico  ------------------------------------------------------------------------------
  # Balance dinámico (actuarial) -------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, A_afi_vap, A_est_vap, A_ext_vap, B_vap, G_vap, V0, V ) ]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_actuarial_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (aportes) ---------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, 
                                A2_vap, A4_vap, A5_vap, A7_vap, A8_vap, A9_vap, A11_vap,
                                A_afi_vap, A_est_vap, A_vap ) ]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_aportes_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (beneficios) ------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, 
                                B2_vap, B_pen_vap, B_dep_vap, B12_vap, B_vap ) ]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (resumen) ---------------------------------------------------------------------
  aux <- balance_anual[ t == max( t ), 
                        list( V0, 
                              A2_vap, 
                              A4_vap,
                              A5_vap,
                              A7_vap, 
                              A8_vap,
                              A9_vap,
                              A11_vap,
                              A_ext_vap,
                              A_est_vap, 
                              A_afi_vap, 
                              A_vap,
                              activo = V0 + A_vap,
                              B2_vap, 
                              B4_vap, 
                              B5_vap, 
                              B7_vap,
                              B8_vap, 
                              B9_vap, 
                              B11_vap, 
                              B12_vap,
                              B_vap, 
                              G_vap, 
                              pasivo = B_vap + G_vap, 
                              V ) ]
  aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
  aux2 <- data.table( V0        = 'Reserva inicial', # 1
                      A2_vap    = '\\quad \\quad Aportes de activos', # 2
                      A4_vap    = '\\quad \\quad Aportes de pensionistas de vejez', # 3
                      A5_vap    = '\\quad \\quad Aportes de pensionistas de invalidez', # 4
                      A7_vap    = '\\quad \\quad Aportes de pensionistas de viudedad', # 5
                      A8_vap    = '\\quad \\quad Aportes de pensionistas de orfandad', # 6
                      A9_vap    = '\\quad \\quad Aportes por extensión de cobertura para cónyuges', # 7
                      A11_vap   = '\\quad \\quad Aportes para cobertura de hijos menores de 18 años', # 8
                      A_afi_vap = '\\quad Aportes de afiliados', # 9
                      A_ext_vap = '\\quad Aportes por extensión de cobertura', # 10
                      A_est_vap = '\\quad Contribución estatal', # 11
                      A_vap     = 'Total aportes', # 12
                      activo    = 'Activo actuarial', # 13
                      B2_vap    = '\\quad \\quad Beneficios afiliados cotizantes', # 14
                      B4_vap    = '\\quad \\quad Beneficios pensionistas vejez', # 15
                      B5_vap    = '\\quad \\quad Beneficios pensionistas invalidez', # 16
                      B7_vap    = '\\quad \\quad Beneficios pensionistas de viudedad', # 17
                      B8_vap    = '\\quad \\quad Beneficios pensionistas de orfandad', # 18
                      B9_vap    = '\\quad \\quad Beneficios por extensión de cobertura para cónyuges', # 19
                      B11_vap   = '\\quad \\quad Beneficios para cobertura de hijos menores de 18 años', # 20
                      B12_vap   = '\\quad \\quad Pago de subsidios', # 21
                      B_vap     = 'Total beneficios', # 22
                      G_vap     = 'Gastos administrativos', # 23
                      pasivo    = 'Pasivo actuarial', # 24
                      V         = 'Balance actuarial' ) # 25
  aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
  aux <- merge( aux2, aux1, by = 'variable', all.x = TRUE )
  setnames( aux, c( 'item', 'descripcion', 'valor' ) )
  xtb_aux <- xtable( aux[ , list( descripcion, valor ) ], digits = c( 0, 0, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_bal_act_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = c( 1, 8, 11, 12, 13, 21, 22, 23, 24 ), sanitize.text.function = identity )
  
  rm( aportes, beneficios, balance_anual )
}

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
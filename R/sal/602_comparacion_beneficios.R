message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGeneración de proporciones comparativas' )
library( openxlsx )

# Solo para el escenario base
escenarios <- paste0( 'escenario_', 1 )

for ( escenario in escenarios ) { # escenario <- escenarios[1]
  
  load( paste0( parametros$RData_seg, 'IESS_SAL_balances_', escenario, '.RData' ) )
  
  comparacion_beneficios <- balance_anual[ t == 20, 
                                           list( B_vap, B2_vap, B3_vap, B4_vap, B6_vap, 
                                                 B7_vap, B8_vap, B9_vap,
                                                 
                                                 B_cat_vap, B2_cat_vap, B3_cat_vap, B4_cat_vap, 
                                                 B6_cat_vap, B7_cat_vap, B8_cat_vap,
                                                 
                                                 B_ncat_vap, B2_ncat_vap, B3_ncat_vap, B4_ncat_vap, 
                                                 B6_ncat_vap, B7_ncat_vap, B8_ncat_vap ) ]

  comparacion_beneficios[ , q2 := B2_vap / B_vap ]
  comparacion_beneficios[ , q3 := B3_vap / B_vap ]
  comparacion_beneficios[ , q4 := B4_vap / B_vap ]
  comparacion_beneficios[ , q6 := B6_vap / B_vap ]
  comparacion_beneficios[ , q7 := B7_vap / B_vap ]
  comparacion_beneficios[ , q8 := B8_vap / B_vap ]
  comparacion_beneficios[ , q9 := B9_vap / B_vap ]
  
  comparacion_beneficios[ , q2_cat := B2_cat_vap / B_vap ]
  comparacion_beneficios[ , q3_cat := B3_cat_vap / B_vap ]
  comparacion_beneficios[ , q4_cat := B4_cat_vap / B_vap ]
  comparacion_beneficios[ , q6_cat := B6_cat_vap / B_vap ]
  comparacion_beneficios[ , q7_cat := B7_cat_vap / B_vap ]
  comparacion_beneficios[ , q8_cat := B8_cat_vap / B_vap ]
  
  comparacion_beneficios[ , q2_ncat := B2_ncat_vap / B_vap ]
  comparacion_beneficios[ , q3_ncat := B3_ncat_vap / B_vap ]
  comparacion_beneficios[ , q4_ncat := B4_ncat_vap / B_vap ]
  comparacion_beneficios[ , q6_ncat := B6_ncat_vap / B_vap ]
  comparacion_beneficios[ , q7_ncat := B7_ncat_vap / B_vap ]
  comparacion_beneficios[ , q8_ncat := B8_ncat_vap / B_vap ]
  
  write.xlsx( comparacion_beneficios,
              file = paste0( 'comparacion_beneficios_', escenario, '.xlsx' ),
              startRow = 1,
              asTable = FALSE,
              colNames = TRUE,
              overwrite = TRUE )
  
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

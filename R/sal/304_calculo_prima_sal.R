message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tCálculo de la prima' )

escenarios <- paste0( 'escenario_', 1:5 )

for ( escenario in escenarios ) { # escenario <- 'escenario_1'
  message( '\tCálculo de la prima para el ', escenario )
  load( file = paste0( parametros$sal_rdata_icomp_balance, escenario, '.RData' ) )
  
  prima <- balance_anual[ t > 0, ]
  
  # Prima de reparto puro --------------------------------------------------------------------------
  # Sin aporte estatal
  prima[ , pri_rep_pur := ( B + G ) / M ]
  
  # Con aporte estatal AE
  prima[ , pri_rep_pur_apo_est := ( B + G - A_est ) / M ]
  
  # Solo dependientes hijos menores a 18
  prima[ , pri_rep_pur_11 := B11 / M ]
  
  # Para catastróficas
  prima[ , pri_rep_pur_cat := B_cat / ( M + P ) ]
  
  # Para pensionistas
  prima[ , pri_rep_pur_pen := B_pen / M ]
  
  
  # Prima media general en cada horizonte ----------------------------------------------------------
  # En cada una de estas primas se reparte el gasto total en función del porcentaje de aporte.
  # pmg = prima media general
  # 
  # Porcentajes de division del gasto administrativo
  prima[ , per_cat := B_cat_vap / B_vap ]
  prima[ , per_ncat := B_ncat_vap / B_vap ]
  prima[ , per_2_cat := B2_cat_vap / B_vap ]
  prima[ , per_2_ncat := ( B2_ncat_vap + B12_vap ) / B_vap ]
  prima[ , per_2 := ( B2_vap + B12_vap ) / B_vap ]
  prima[ , per_pen_cat := ( B4_cat_vap + B5_cat_vap + B7_cat_vap + B8_cat_vap ) / B_vap ]
  prima[ , per_pen_ncat := ( B4_ncat_vap + B5_ncat_vap + B7_ncat_vap + B8_ncat_vap ) / B_vap ]
  prima[ , per_pen := ( B4_vap + B5_vap + B7_vap + B8_vap ) / B_vap ]
  prima[ , per_4_cat := B4_cat_vap / B_vap ]
  prima[ , per_4_ncat := B4_ncat_vap / B_vap ]
  prima[ , per_4 := B4_vap / B_vap ]
  prima[ , per_5_cat := B5_cat_vap / B_vap ]
  prima[ , per_5_ncat := B5_ncat_vap / B_vap ]
  prima[ , per_5 := B5_vap / B_vap ]
  prima[ , per_7_cat := B7_cat_vap / B_vap ]
  prima[ , per_7_ncat := B7_ncat_vap / B_vap ]
  prima[ , per_7 := B7_vap / B_vap ]
  prima[ , per_8_cat := B8_cat_vap / B_vap ]
  prima[ , per_8_ncat := B8_ncat_vap / B_vap ]
  prima[ , per_8 := B8_vap / B_vap ]
  prima[ , per_pen_cat := ( B4_cat_vap + B5_cat_vap + B7_cat_vap + B8_cat_vap ) / B_vap ]
  prima[ , per_pen_ncat := ( B4_ncat_vap + B5_ncat_vap + B7_ncat_vap + B8_ncat_vap ) / B_vap ]
  prima[ , per_pen := ( B4_vap + B5_vap + B7_vap + B8_vap ) / B_vap ]
  prima[ , per_9_cat := B9_cat_vap / B_vap ]
  prima[ , per_9_ncat := B9_ncat_vap / B_vap ]
  prima[ , per_9 := B9_vap / B_vap ]
  prima[ , per_11_cat := B11_cat_vap / B_vap ]
  prima[ , per_11_ncat := B11_ncat_vap / B_vap ]
  prima[ , per_11 := B11_vap / B_vap ]
  prima[ , per_dep_cat := ( B9_cat_vap + B11_cat_vap ) / B_vap ]
  prima[ , per_dep_ncat := ( B9_ncat_vap + B11_ncat_vap ) / B_vap ]
  prima[ , per_dep := ( B9_vap + B11_vap ) / B_vap ]
  
  # prima$per_2 + prima$per_pen + prima$per_dep
  # prima$per_2 + prima$per_pen + prima$per_9 + prima$per_11
  # prima$per_cat + prima$per_ncat
  
  # Prima media general total
  prima[ , pmg := ( B_vap + G_vap - A_est_vap ) / M_vap ]
  prima[ , pmg_cat := ( B_cat_vap + per_cat * G_vap - A_est_cat_vap ) / M_vap ]
  prima[ , pmg_ncat := ( B_ncat_vap + per_ncat * G_vap - A_est_ncat_vap ) / M_vap ]
  # prima$pmg - ( prima$pmg_cat + prima$pmg_ncat )
  
  # Prima media general para activos
  prima[ , pmg_2 := ( B2_vap + B12_vap + per_2 * G_vap - A_est_2_vap ) / M_vap ]
  prima[ , pmg_2_cat := ( B2_cat_vap + per_2_cat * G_vap - A_est_2_cat_vap ) / M_vap ]
  prima[ , pmg_2_ncat := ( B2_ncat_vap + B12_vap + per_2_ncat * G_vap - A_est_2_ncat_vap ) / M_vap ]
  # prima$pmg_2 - ( prima$pmg_2_cat + prima$pmg_2_ncat )
  
  # Prima media general para pensionistas
  prima[ , pmg_pen := ( B_pen_vap + per_pen * G_vap - A_est_pen_vap ) / M_vap ]
  prima[ , pmg_pen_cat := ( B_pen_cat_vap + per_pen_cat * G_vap - A_est_pen_cat_vap ) / M_vap ]
  prima[ , pmg_pen_ncat := ( B_pen_ncat_vap + per_pen_ncat * G_vap - A_est_pen_ncat_vap ) / M_vap ]
  # prima$pmg_pen - ( prima$pmg_pen_cat + prima$pmg_pen_ncat )
  
  # Prima media general para pensionistas de vejez
  prima[ , pmg_4 := ( B4_vap + per_4 * G_vap - A_est_4_vap ) / M_vap ]
  prima[ , pmg_4_cat := ( B4_cat_vap + per_4_cat * G_vap - A_est_4_cat_vap ) / M_vap ]
  prima[ , pmg_4_ncat := ( B4_ncat_vap + per_4_ncat * G_vap - A_est_4_ncat_vap ) / M_vap ]
  # prima$pmg_4 - ( prima$pmg_4_cat + prima$pmg_4_ncat )
  
  # Prima media general para pensionistas de invalidez
  prima[ , pmg_5 := ( B5_vap + per_5 * G_vap - A_est_5_vap ) / M_vap ]
  prima[ , pmg_5_cat := ( B5_cat_vap + per_5_cat * G_vap - A_est_5_cat_vap ) / M_vap ]
  prima[ , pmg_5_ncat := ( B5_ncat_vap + per_5_ncat * G_vap - A_est_5_ncat_vap ) / M_vap ]
  # prima$pmg_5 - ( prima$pmg_5_cat + prima$pmg_5_ncat )
  
  # Prima media general para pensionistas de viudedad
  prima[ , pmg_7 := ( B7_vap + per_7 * G_vap - A_est_7_vap ) / M_vap ]
  prima[ , pmg_7_cat := ( B7_cat_vap + per_7_cat * G_vap - A_est_7_cat_vap ) / M_vap ]
  prima[ , pmg_7_ncat := ( B7_ncat_vap + per_7_ncat * G_vap - A_est_7_ncat_vap ) / M_vap ]
  # prima$pmg_7 - ( prima$pmg_7_cat + prima$pmg_7_ncat )
  
  # Prima media general para pensionistas de orfandad
  prima[ , pmg_8 := ( B8_vap + per_8 * G_vap - A_est_8_vap ) / M_vap ]
  prima[ , pmg_8_cat := ( B8_cat_vap + per_8_cat * G_vap - A_est_8_cat_vap ) / M_vap ]
  prima[ , pmg_8_ncat := ( B8_ncat_vap + per_8_ncat * G_vap - A_est_8_ncat_vap ) / M_vap ]
  # prima$pmg_8 - ( prima$pmg_8_cat + prima$pmg_8_ncat )
  
  # Prima media general para dependientes
  prima[ , pmg_dep := ( B_dep_vap + per_dep * G_vap - A_est_dep_vap ) / M_vap ]
  prima[ , pmg_dep_cat := ( B_dep_cat_vap + per_dep_cat * G_vap - A_est_dep_cat_vap ) / M_vap ]
  prima[ , pmg_dep_ncat := ( B_dep_ncat_vap + per_dep_ncat * G_vap - A_est_dep_ncat_vap ) / M_vap ]
  # prima$pmg_dep - ( prima$pmg_dep_cat + prima$pmg_dep_ncat )
  
  # Prima media general para cónyuges
  prima[ , pmg_9 := ( B9_vap + per_9 * G_vap - A_est_9_vap ) / M_vap ]
  prima[ , pmg_9_cat := ( B9_cat_vap + per_9_cat * G_vap - A_est_9_cat_vap ) / M_vap ]
  prima[ , pmg_9_ncat := ( B9_ncat_vap + per_9_ncat * G_vap - A_est_9_ncat_vap ) / M_vap ]
  # prima$pmg_9 - ( prima$pmg_9_cat + prima$pmg_9_ncat )
  
  # Prima media general para hijos menores de 18 años
  prima[ , pmg_11 := ( B11_vap + per_11 * G_vap - A_est_11_vap ) / M_vap ]
  prima[ , pmg_11_cat := ( B11_cat_vap + per_11_cat * G_vap - A_est_11_cat_vap ) / M_vap ]
  prima[ , pmg_11_ncat := ( B11_ncat_vap + per_11_ncat * G_vap - A_est_11_ncat_vap ) / M_vap ]
  # prima$pmg_11 - ( prima$pmg_11_cat + prima$pmg_11_ncat )
  
  # Prima media general para activos y dependientes
  prima[ , pmg_act_dep := pmg_2 + pmg_dep ]
  prima[ , pmg_act_dep_cat := pmg_2_cat + pmg_dep_cat ]
  prima[ , pmg_act_dep_ncat := pmg_2_ncat + pmg_dep_ncat ]

  # prima$pmg - ( prima$pmg_2 + prima$pmg_pen + prima$pmg_dep )
  # prima$pmg_cat - ( prima$pmg_2_cat + prima$pmg_pen_cat + prima$pmg_dep_cat )
  # prima$pmg_ncat - ( prima$pmg_2_ncat + prima$pmg_pen_ncat + prima$pmg_dep_ncat )
  # prima$pmg - ( prima$pmg_2 + prima$pmg_4 + prima$pmg_5 + prima$pmg_7 + prima$pmg_8 + prima$pmg_9 + prima$pmg_11 )
  
  save( prima, 
        file = paste0( parametros$sal_rdata_icomp_primas, escenario, '.RData' ) )  
  
}

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

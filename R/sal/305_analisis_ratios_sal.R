message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tAnálsis de ratios para SAL' )

escenarios <- paste0( 'escenario_', 1:5 )

for ( escenario in escenarios ) { # escenario <- escenarios[1]
  message( '\tAnálisis ratios para el ', escenario )
  
  load( paste0( parametros$sal_rdata_icomp_balance, escenario, '.RData' ) )
  
  ratios <- aportes[ t > 0, list( l2 = sum( l2 ), 
                                  l4 = sum( l4 ),
                                  l5 = sum( l5 ),
                                  l7 = sum( l7 ),
                                  l8 = sum( l8 ),
                                  l9 = sum( l9 ),
                                  l11 = sum( l11 ) ), by = list( t ) ]
  ratios <- merge( 
    ratios, 
    balance_anual[ t > 0, list( 
      t, M, A, A_est, 
      B ) ],
    by = c( 't' ) )
  
  ratios[ , sal_mean := M / l2  ]
  ratios[ , ben_mean := B / ( l2 + l4 + l5 + l7 + l8 + l9 + l11 ) ]
  ratios[ , ben_sal := B / M ]
  ratios[ , dep_tasa := l2 / ( l4 + l5 + l7 + l8 + l9 + l11 ) ]
  ratios[ , rem_tasa := dep_tasa * ben_sal  ]
  
  save( ratios, 
        file = paste0( parametros$sal_rdata_icomp_ratios, escenario, '.RData' ) )
}

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

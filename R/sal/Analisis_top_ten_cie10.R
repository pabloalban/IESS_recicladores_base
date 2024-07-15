load( paste0( parametros$RData_seg, 'IESS_SAL_estadisticas_beneficios.RData' ) )


tst <- copy( ben_est_sev )
tst[ , icd_best10 := ifelse( icd == 'OTRAS', 'OTRAS', 'BEST10')]

tst1 <- tst[ , list( N = sum( N, na.rm = TRUE ),
                     X = sum( EX, na.rm = TRUE ) ), 
             by = list( enf, ser,  icd_best10 ) ]   # list( sexo, u, enf, ser,  icd_best10 )
tst1[ , S := N * X ]

# severidad
tst1[ , Xtot := sum( X, na.rm = TRUE ), by = list( enf, ser ) ]
tst1[ , porcX_best10 := X / Xtot ]

# frecuencia
tst1[ , Ntot := sum( N, na.rm = TRUE ), by = list( enf, ser ) ]
tst1[ , porcN_best10 := N / Ntot ]

# perdida agregada
tst1[ , Stot := sum( S, na.rm = TRUE ), by = list( enf, ser ) ]
tst1[ , porcS_best10 := S / Stot ]

setorder( tst1, icd_best10, enf, ser, -porcN_best10 )




aux <- tst1[ icd_best10 == 'BEST10', list( porc_best10 ) ]

histogram(  aux$porc_best10 )

quantile( aux$porc_best10, probs = seq( 0.6, 0.8, 0.02) )
tst18 <- proc[ x <= 18, list( N_reg = .N,
                              X = sum( X, na.rm = TRUE ) ), 
               by = year( t ) ]

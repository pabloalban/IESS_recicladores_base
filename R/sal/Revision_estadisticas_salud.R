load ( paste0( parametros$RData_seg, 'IESS_SAL_estadisticas_beneficios.RData' ) )

correc <- 410
tst18 <- balance_anual[ , list( anio = t + 2020, M, B11, B11_corr = correc * B11, prima = correc * 100 * B11 / M, prima2 = correc * 100 * B11_vap / M_vap )]

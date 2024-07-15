# Configuraciones particulares ---------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tConfiguraciones particulares para el seguro SAL' )

parametros$sal_hacer_ana_dem <- FALSE
parametros$sal_calcular_balance <- FALSE

# Calcular escenarios adicionales
parametros$sal_cal_add_esc <- FALSE

parametros$sal_horizonte <- 10 # en años  
parametros$anio_fin <- parametros$anio_ini + parametros$sal_horizonte
parametros$sal_fec_fin <- ymd( '2020-12-31' )
parametros$sal_fec_ini <- ymd( '2013-01-01' ) # fecha inicio del periodo de observación
parametros$sal_reserva_ini <- 8708847480.93 # reserva inicial

# Reporte-------------------------------------------------------------------------------------------
parametros$sal_rep_gen <- paste0( parametros$work_dir, 'R/sal/600_reporte_latex_sal.R' )

# Objetos de salida, usualmente .RData -------------------------------------------------------------
parametros$sal_rdata_bal_fin <- paste0( parametros$RData_seg, 'IESS_SAL_balance_financieros_2020.RData' )
parametros$sal_rdata_icomp_balance <- paste0( parametros$RData_seg, 'IESS_SAL_balances_2020_' )
parametros$sal_rdata_icomp_conf_esc <- paste0( parametros$RData_seg, 'IESS_SAL_configuracion_2020_' )
parametros$sal_rdata_icomp_proy_benef <- paste0( parametros$RData_seg, 'IESS_SAL_proyeccion_beneficios_2020_' )
parametros$sal_rdata_icomp_primas <- paste0( parametros$RData_seg, 'IESS_SAL_primas_2020_' )
parametros$sal_rdata_icomp_ratios <- paste0( parametros$RData_seg, 'IESS_SAL_analisis_ratios_2020_' )
parametros$sal_rdata_icomp_sal_proy <- paste0( parametros$RData_seg, 'IESS_SAL_proyeccion_salarios_', parametros$anio_ini, '_' )
parametros$sal_rdata_est_sev <- paste0( parametros$RData_seg, 'IESS_SAL_estimacion.RData' )
parametros$sal_rdata_ana_sen <- paste0( parametros$RData_seg, 'IESS_SAL_analisis_sensibilidad_total_', parametros$anio_ini, '.RData' )

# Variables globales que son sobre escritas por cada seguro ----------------------------------------
parametros$horizonte <- parametros$sal_horizonte

message( paste( rep( '-', 100 ), collapse = '' ) )

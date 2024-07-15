message( '\tEstableciendo información ser insertada en el reporte' )

#REP <- new.env()

REP$corte <- 2022


# Carga información --------------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_est_dem )

# Afiliados ----------------------------------------------------------------------------------------

REP$cre_prom_anual <- format( est_sal_anio[ , .(anio, ER_act, st = lag(ER_act) ) ]
                              [ anio < 2023, var := mean( ( ER_act / st - 1 ) * 100, na.rm = T ) ]
                              [ anio == REP$corte, var ], digits = 3, big.mark = '.',  
                              decimal.mark = ',', format = 'f' )

REP$prom_anual_sgo <- format( 
  mean( est_sal_anio$ER_act, na.rm = TRUE ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_sgo <- format( 
  est_sal_anio[ anio == 2022, ]$ER_act, 
  nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$cre_afi_sgo <- format( 
  100 * ( est_sal_anio[ anio == 2022 ]$ER_act / 
            est_sal_anio[ anio == 2012 ]$ER_act - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_var <- format( 
  100 * ( est_sal_anio[ anio == 2022 ]$ER_act / 
            est_sal_anio[ anio == 2019 ]$ER_act - 1 ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_sgo_h <- format( 
  100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ER_act / 
    sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_act ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_sgo_m <- format(
  100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ER_act / 
    sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_act ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- est_sal_anio_sexo_edad[ 
  anio == REP$corte & sexo == 'H', 
  list( x_mean = sum( x * ER_act, na.rm = TRUE ) / sum( ER_act, na.rm = TRUE ) ) ]
REP$edad_prom_h <- format(
  aux$x_mean,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- est_sal_anio_sexo_edad[ 
  anio == REP$corte & sexo == 'M', 
  list( x_mean = sum( x * ER_act, na.rm = TRUE ) / sum( ER_act, na.rm = TRUE ) ) ]
REP$edad_prom_m <- format( 
  aux$x_mean,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Afiliados TNRH -----------------------------------------------------------------------------------

REP$cre_prom_anua_tnrh <- format( est_sal_anio[ anio > 2015 , .(anio, ER_tnrh_act, st = lag(ER_tnrh_act) ) ]
                                  [ anio < 2023, var := mean( ( ER_tnrh_act / st - 1 ) * 100, na.rm = T ) ]
                                  [ anio == REP$corte, var ], digits = 3, big.mark = '.',  
                                  decimal.mark = ',', format = 'f' )

REP$prom_anual_tnrh <- format(
  mean( est_sal_anio$ER_tnrh_act, na.rm = TRUE ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_tnrh <-format( 
  est_sal_anio[ anio == 2022, ]$ER_tnrh_act, 
  nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$cre_afi_tnrh <- format(
  100 * ( est_sal_anio[ anio == 2022 ]$ER_tnrh_act / 
            est_sal_anio[ anio == 2015 ]$ER_tnrh_act - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_tnrh_2019 <- format( 
  100 * ( est_sal_anio[ anio == 2022 ]$ER_tnrh_act / 
            est_sal_anio[ anio == 2019 ]$ER_tnrh_act - 1 ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_tnrh_h <- format( 
  100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ER_tnrh_act / 
    sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_tnrh_act ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_tnrh_m <- format( 
  100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ER_tnrh_act / 
    sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_tnrh_act ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- est_sal_anio_sexo_edad[ 
  anio == REP$corte & sexo == 'H', 
  list( x_mean = sum( x * ER_tnrh_act, na.rm = TRUE ) / sum( ER_tnrh_act, na.rm = TRUE ) ) ]
REP$edad_prom_tnrh_h <- format(
  aux$x_mean,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- est_sal_anio_sexo_edad[ 
  anio == REP$corte & sexo == 'M', 
  list( x_mean = sum( x * ER_tnrh_act, na.rm = TRUE ) / sum( ER_tnrh_act, na.rm = TRUE ) ) ]
REP$edad_prom_tnrh_m <- format( 
  aux$x_mean,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Masa salarial ------------------------------------------------------------------------------------

REP$masa_salarial <- format(
  est_sal_anio[ anio == REP$corte ]$S, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_salarial_var <- format(
  100 * ( est_sal_anio[ anio == REP$corte ]$S / est_sal_anio[ anio == 2012 ]$S - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_sal_prom_2012_2015 <- format( est_sal_anio[ anio < 2016 , .(anio, S, st = lag(S) ) ]
                                       [ , var := mean( ( S / st - 1 ) * 100, na.rm = T ) ]
                                       [ anio == 2015, var ], digits = 3, big.mark = '.',  
                                       decimal.mark = ',', format = 'f' )

REP$masa_salarial_prom <- format( 
  mean( est_sal_anio$S ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_salarial_var_2020 <- format( 
  100 * ( est_sal_anio[ anio == REP$corte ]$S / est_sal_anio[ anio == 2021 ]$S - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom_h <- format( 
  est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ESm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom_m <- format( 
  est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ESm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom <- format( 
  est_sal_anio[ anio == REP$corte ]$ESm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Masa salarial TNRH -------------------------------------------------------------------------------

REP$masa_salarial_cre_prom <- format( est_sal_anio[ anio > 2014, .(anio, ST, st = lag(ST) ) ]
                                      [ , var := mean( ( ST / st - 1 ) * 100, na.rm = T ) ]
                                      [ anio == REP$corte, var ], digits = 3, big.mark = '.',  
                                      decimal.mark = ',', format = 'f' )


REP$masa_salarial_cre <- format( 100 * ( est_sal_anio[ anio == REP$corte ]$ST / 
                                           est_sal_anio[ anio == 2021 ]$ST - 1 ), 
                                 nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', 
                                 format = 'f' )

REP$masa_salarial_tnrh <- format(
  est_sal_anio[ anio == REP$corte ]$ST, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_salarial_tnrh_var <- format(
  100 * ( est_sal_anio[ anio == REP$corte ]$ST / est_sal_anio[ anio == 2021 ]$ST - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_salarial_tnrh_prom <-  format( 
  mean( est_sal_anio[ anio >= 2015 ]$ST ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom_tnrh_h <- format( 
  est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ESTm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom_tnrh_m <- format(
  est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ESTm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom_tnrh <- format(
  est_sal_anio[ anio == REP$corte ]$ESTm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Afiliados exposición al riesgo IVM ---------------------------------------------------------------

REP$afi_er_sgo <- format( 
  sum( est_act_ina_er[ tipo == 'ER2' ]$total ), 
  nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_er_tnrh <- format( 
  sum( est_act_tnrh_ina_er[ tipo == 'ER2' ]$total ), 
  nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_er_sgo_act_h <- format(
  100* ( est_act_ina_er[ tipo == 'ER2' & sexo == 'H' ]$por ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )                              

REP$afi_er_sgo_act_m <- format(
  100 * ( est_act_ina_er[ tipo == 'ER2' & sexo == 'M' ]$por ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )                              

REP$afi_er_sgo_ina_h <- format(
  100 * ( est_act_ina_er[ tipo == 'ER3' & sexo == 'H' ]$por ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )                              

REP$afi_er_sgo_ina_m <- format(
  100 * (est_act_ina_er[ tipo == 'ER3' & sexo == 'M' ]$por ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )                              

REP$afi_er_tnrh_act_h <- format(
  100 * ( est_act_tnrh_ina_er[ tipo == 'ER2' & sexo == 'H' ]$por ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )  

REP$afi_er_tnrh_act_m <- format(
  100 * ( est_act_tnrh_ina_er[ tipo == 'ER2' & sexo == 'M' ]$por ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )  

REP$afi_er_tnrh_ina_h <- format(
  100 * ( est_act_tnrh_ina_er[ tipo == 'ER3' & sexo == 'H' ]$por ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_er_tnrh_ina_m <- format(
  100 * ( est_act_tnrh_ina_er[ tipo == 'ER3' & sexo == 'M' ]$por ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

grupos <- paste0( 'Grupo ', 1:4 )
grup_nom <- paste0( 'g', 1:4 )

for ( i in 1:length( grupos ) ) {
  
  grp <- grupos[ i ]
  expr <- expression({
    
    REP$afi_er_sgo_h_GRUP <- format( 
      100 * est_act_er_risk_h[ riesgo == grp ]$por, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$afi_er_sgo_m_GRUP <- format( 
      100 * est_act_er_risk_m[ riesgo == grp ]$por, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$afi_er_sgo_ina_h_GRUP <- format( 
      100 * est_ina_er_risk_h[ riesgo == grp ]$por, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$afi_er_sgo_ina_m_GRUP <- format( 
      100 * est_ina_er_risk_m[ riesgo == grp ]$por, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$afi_er_tnrh_h_GRUP <- format( 
      100 * est_act_tnrh_er_risk_h[ riesgo == grp ]$por, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$afi_er_tnrh_m_GRUP <- format( 
      100 * est_act_tnrh_er_risk_m[ riesgo == grp ]$por, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$afi_er_tnrh_ina_h_GRUP <- format( 
      100 * est_ina_tnrh_er_risk_h[ riesgo == grp ]$por, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$afi_er_tnrh_ina_m_GRUP <- format( 
      100 * est_ina_tnrh_er_risk_m[ riesgo == grp ]$por, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
  })
  expr <- gsub( '(GRUP)', grup_nom[ i ], deparse( expr ) )
  eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
  
}

# Variables para cada grupo de pensionistas --------------------------------------------------------

REP$pen_sgo_tot <- format(
  est_pen_anio[ anio == REP$corte ]$ER, 
  nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_gas_tot <- format(
  est_pen_anio[ anio == REP$corte ]$P, 
  nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

tipos <- c( 'VEJEZ', 'INVALIDEZ', 'DISCAPACIDAD', 'VIUDEDAD', 'ORFANDAD' )
tip_nom <- c( 'vej', 'inv', 'dis', 'viu', 'orf' )

for ( i in 1:length( tipos ) ) {
  
  expr <- expression({
    
    REP$pen_sgo_TIP <- format( 
      100 * sum ( est_pen_anio_tipo[ anio == REP$corte & tipo == tipos[ i ] ]$ER ) / 
        est_pen_anio[ anio == REP$corte ]$ER, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_sgo_gas_TIP <- format( 
      100 * sum ( est_pen_anio_tipo[ anio == REP$corte & tipo == tipos[ i ] ]$P ) / 
        est_pen_anio[ anio == REP$corte ]$P, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_sgo_TIP_h <- format( 
      100 * est_pen_anio_tipo_sexo[ anio == REP$corte & sexo == 'H' & tipo == tipos[ i ] ]$ER / 
        est_pen_anio[ anio == REP$corte ]$ER, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_sgo_TIP_m <- format( 
      100 * est_pen_anio_tipo_sexo[ anio == REP$corte & sexo == 'M' & tipo == tipos[ i ] ]$ER / 
        est_pen_anio[ anio == REP$corte ]$ER, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    aux <- est_pen_anio_tipo_sexo_x[ anio == REP$corte & sexo == 'H' & tipo == tipos[ i ] ]
    aux <- aux[ ER == max( ER ) ]
    REP$pen_edad_TIP_h <- format( 
      aux$x[ 1 ], 
      nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    aux <- est_pen_anio_tipo_sexo_x[ anio == REP$corte & sexo == 'M' & tipo == tipos[ i ] ]
    aux <- aux[ ER == max( ER ) ]
    REP$pen_edad_TIP_m <- format( 
      aux$x[ 1 ], 
      nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_TIP_pob <- format( 
      est_pen_anio_tipo[ anio == REP$corte & tipo == tipos[ i ] ]$ER, 
      nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_TIP_2012 <- format(
      est_pen_anio_tipo[ anio == 2012 & tipo == tipos[ i ] ]$ER, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_TIP_cre <- format(
      100 * ( est_pen_anio_tipo[ anio == REP$corte & tipo == tipos[ i ] ]$ER / 
                est_pen_anio_tipo[ anio == 2015 & tipo == tipos[ i ] ]$ER - 1 ),
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_TIP_prom <- format( 
      mean( est_pen_anio_tipo[ anio > 2014 & anio < 2023 & tipo == tipos[ i ] ]$td, na.rm = TRUE ), 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_TIP_prop_h <- format( 
      100 * est_pen_anio_tipo_sexo[ anio == REP$corte & sexo == 'H' & tipo == tipos[ i ] ]$ER / 
        est_pen_anio_tipo[ anio == REP$corte & tipo == tipos[ i ] ]$ER, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_TIP_prop_m <- format( 
      100 * est_pen_anio_tipo_sexo[ anio == REP$corte & sexo == 'M' & tipo == tipos[ i ] ]$ER / 
        est_pen_anio_tipo[ anio == REP$corte & tipo == tipos[ i ] ]$ER, 
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_TIP_pen_prom_h <- format( 
      est_pen_anio_tipo_sexo[ anio == REP$corte & sexo == 'H' & tipo == tipos[ i ] ]$EPm,
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pen_TIP_pen_prom_m <- format( 
      est_pen_anio_tipo_sexo[ anio == REP$corte & sexo == 'M' & tipo == tipos[ i ] ]$EPm,
      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    aux <- est_pen_anio_tipo_sexo_x[ 
      anio == REP$corte & sexo == 'H', 
      list( x_mean = sum( x * ER, na.rm = TRUE ) / sum( ER, na.rm = TRUE ) ) ]
    REP$pen_TIP_edad_prom_h <- format( 
      aux$x_mean,
      nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f', digits = 2 )
    
    aux <- est_pen_anio_tipo_sexo_x[ 
      anio == REP$corte & sexo == 'M', 
      list( x_mean = sum( x * ER, na.rm = TRUE ) / sum( ER, na.rm = TRUE ) ) ]
    REP$pen_TIP_edad_prom_m <- format( 
      aux$x_mean,
      nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f', digits = 2 )
    
  })
  expr <- gsub( '(TIP)', tip_nom[ i ], deparse( expr ) )
  eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
  
}

# # Información por escenarios -----------------------------------------------------------------------
# escenarios <- paste0( 'escenario_', 1:5 )
# esc_nom <- paste0( 'esc_', 1:5 )
# 
# for ( i in 1:length( escenarios ) ) { # i <- 1
#   
#   escenario <- escenarios[ i ]
#   
#   message( '\tGenerando auto información para el ', escenario )
#   
#   load( paste0( parametros$ivm_rdata_icomp_conf_esc, escenario, '.RData' ) )
#   load( paste0( parametros$ivm_rdata_icomp_primas, escenario, '.RData' ) )
#   load( paste0( parametros$ivm_rdata_icomp_balance, escenario, '.RData' ) )
#   load( paste0( parametros$ivm_rdata_icomp_ratios, escenario, '.RData' ) )
#   
#   expr <- expression({
#     
#     REP$bal_act_ESC <- format( 
#       balance_anual[ t == parametros$horizonte ]$V*-1, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$bal_cap_ESC <- format( 
#       balance_anual[ t == parametros$horizonte ]$V_cap, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$duracion_ESC <- max( which( balance_anual$V_cap > 0 ) ) + 2022 - 1
#     
#     REP$sup_apo_ESC <- min( balance_anual[ A_est >= A & t > 0 ]$t ) + 2022
#     
#     REP$cap_ini_ESC <- format( 
#       esc$V0, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$dep_tas_ini_ESC <- format( 
#       ratios[ t == 1 ]$dep_tasa, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$dep_tas_fin_ESC <- format( 
#       ratios[ t == parametros$horizonte ]$dep_tasa, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$sus_tas_ini_ESC <- format( 
#       100 * ratios[ t == 1 ]$rem_tasa, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$sus_tas_fin_ESC <- format( 
#       100 * ratios[ t == parametros$horizonte ]$rem_tasa, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$pri_med_niv_ESC <- format( 
#       100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$apo_est_ESC <- format( 
#       100 * esc$apo_act$por_apo_est[1], 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_act_ESC <- format( 
#       100 * esc$apo_act$i_a[1], 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#   })
#   expr <- gsub( '(ESC)', esc_nom[ i ], deparse( expr ) )
#   eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
#   
# }

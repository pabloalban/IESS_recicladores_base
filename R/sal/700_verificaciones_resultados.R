rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )

# Carga plantilla gráficos -------------------------------------------------------------------------
source( paste0( parametros$work_dir, "R/401_graf_plantilla.R" ), encoding = 'UTF-8', echo = FALSE )
graf_width <- 25
graf_height <- 17

# Carga de datos -----------------------------------------------------------------------------------
escenario <- 'escenario_1.RData'
load( parametros$demo_rdata_sgo_pob_proy )
load( paste0( parametros$sal_rdata_icomp_balance, escenario ) )
load( paste0( parametros$sal_rdata_icomp_primas, escenario ) )
load( parametros$demo_rdata_sgo_est_dem )

# Balances -----------------------------------------------------------------------------------------
balance_anual$V / 1e6
balance_anual$V_cor / 1e6

# Beneficios totales -------------------------------------------------------------------------------
dat <- balance_anual[ , list( t, anio, V, V_cor ) ]

dat <- balance_anual[ anio > parametros$anio_ini ]
balance_anual$B / 1e6
plot( dat$anio, dat$B / 1e6, type = 'l', 
      lwd = 3, col = 'orange', ylim = c( 0, 4e3 ), xlim = c( 2013, 2030 ) )
points( c( 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 ), 
        c( 1413, 1691, 1647, 1922, 2302, 1841, 2140, 1538, 2029, 1848 ), 
        pch = 16, lwd = 3, type = 'b', col = 'blue' )

# Notación:
# cat = catastróficas
# ncat = no catastróficas

# ( 2 ) Flujos de activos --------------------------------------------------------------------------
balance_anual$A2 / 1e6
balance_anual$A_ext_2 / 1e6

balance_anual$B2 / 1e6
balance_anual$B2_ncat / 1e6
balance_anual$B2_cat / 1e6

plot( dat$anio, dat$A2 / 1e6, type = 'l', 
      lwd = 3, col = 'orange', ylim = c( 0, 4e3 ), xlim = c( 2013, 2030 ) )
points( c( 2020, 2021, 2022 ), 
        c( 1533, 1383, 1400 ), 
        pch = 16, cex = 1, col = 'blue' )

plot( dat$anio, dat$B2 / 1e6, type = 'l', 
      lwd = 3, col = 'orange', ylim = c( 0, 4e3 ), xlim = c( 2013, 2030 ) )
points( c( 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 ), 
        c( 956, 1191, 1135, 1223, 1347, 1072, 1231, 879, 1153, 985 ), 
        pch = 16, cex = 1, col = 'blue' )

# ( 4 ) Flujos de pensionistas de vejez ------------------------------------------------------------
balance_anual$A4 / 1e6
balance_anual$A_ext_4 / 1e6

balance_anual$B4 / 1e6
balance_anual$B4_ncat / 1e6
balance_anual$B4_cat / 1e6

( balance_anual$B4 + balance_anual$B5 ) / 1e6

# ( 5 ) Flujos de pensionistas de invalidez --------------------------------------------------------
balance_anual$A5 / 1e6
balance_anual$A_ext_5 / 1e6

balance_anual$B5 / 1e6
balance_anual$B5_ncat / 1e6
balance_anual$B5_cat / 1e6

# ( 7 ) Flujos de pensionistas de viudedad ---------------------------------------------------------
balance_anual$A7 / 1e6
balance_anual$A_ext_7 / 1e6

balance_anual$B7 / 1e6
balance_anual$B7_ncat / 1e6
balance_anual$B7_cat / 1e6

# ( 8 ) Flujos de pensionistas de orfandad ---------------------------------------------------------
balance_anual$A8 / 1e6
balance_anual$A_ext_8 / 1e6

balance_anual$B8 / 1e6
balance_anual$B8_ncat / 1e6
balance_anual$B8_cat / 1e6

# ( 9 ) Flujos de dependientes cónyuges ------------------------------------------------------------
balance_anual$A9 / 1e6

balance_anual$B9 / 1e6
balance_anual$B9_ncat / 1e6
balance_anual$B9_cat / 1e6

plot( dat$anio, dat$B9 / 1e6, type = 'l', 
      lwd = 3, col = 'orange', ylim = c( 0, 2e2 ), xlim = c( 2013, 2030 ) )
points( c( 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 ), 
        c( 4, 16, 22, 49, 35, 46, 54, 36, 52, 51 ), 
        pch = 16, cex = 1, col = 'blue' )

# ( 11 ) Flujos de dependientes hijos menores de 18 ------------------------------------------------
balance_anual$A11 / 1e6

balance_anual$B11 / 1e6
balance_anual$B11_ncat / 1e6
balance_anual$B11_cat / 1e6

balance_anual$B11_vap / 1e6

plot( dat$anio, dat$B11 / 1e6, type = 'l', 
      lwd = 3, col = 'orange', ylim = c( 0, 5e2 ), xlim = c( 2013, 2030 ) )
points( c( 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 ), 
        c( 142, 202, 212, 262, 257, 275, 324, 198, 206, 287 ), 
        pch = 16, cex = 1, col = 'blue' )

balance_anual$B11 - ( balance_anual$B11_ncat + balance_anual$B11_cat )

balance_anual$B4 + balance_anual$B5 + balance_anual$B7 + balance_anual$B8
balance_anual$B_pen / 1e6

pob_proy_tot$l2 + pob_proy_tot$l4 + pob_proy_tot$l5

pob_proy_tot$l7
pob_proy_tot$l8

pob_proy_tot$l11
a <- pob_proy_tot$l11 / ( pob_proy_tot$l2 + pob_proy_tot$l4 + pob_proy_tot$l5 )

pob_proy_tot$l9
b <- pob_proy_tot$l9 / ( pob_proy_tot$l2 + pob_proy_tot$l4 + pob_proy_tot$l5 )
plot( a )
plot( b )

# Prima media general ------------------------------------------------------------------------------
prima$pmg
prima$pmg_2

# Reservas por escenario ---------------------------------------------------------------------------
escenarios <- paste0( 'escenario_', 1:5 )

balances <- NULL
for ( i in 1:length( escenarios ) ) {
  message( '\r\tProcesando ', escenarios[ i ] )
  load( paste0( parametros$sal_rdata_icomp_balance, escenarios[ i ], '.RData' ) )
  if ( i == 1 ) {
    balances <- balance_anual[ , list( anio, escenario = i, V_cor, V ) ]
  } else {
    balances <- rbindlist( l = list( balances, balance_anual[ , list( anio, escenario = i, V_cor, V ) ] ) )
  }
}

balances <- dcast.data.table( balances, formula = anio ~ escenario, value.var = c( 'V_cor', 'V' ) )

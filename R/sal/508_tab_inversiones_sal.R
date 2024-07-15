message( paste( rep( '-', 100 ), collapse = '' ) )

# Función de tildes a latex-------------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura portafolio de inversiones' )
file_inversiones <- paste0( parametros$RData_seg, 'IESS_SAL_inversiones', '.RData' )
load( file = file_inversiones )

message( '\tTablas del portafolio de inversiones' )
# Tabla de la evolución del portafolio SALUD -------------------------------------------------------
aux <- recurs_adm_biess
aux$rendimiento_ponderado<-aux$rendimiento_ponderado*100
aux$rendimiento_neto<-aux$rendimiento_neto*100
aux$rendimiento_neto_real<-aux$rendimiento_neto_real*100
aux$ano <- as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 6), 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_total_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla Rendimientos SALUD al 2020------------------------------------------------------------------
aux <- (inver_corte) %>% dplyr::select( -ano )
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rendimiento_ponderado_real<-aux$rendimiento_ponderado_real*100
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 4) ) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_corte', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list( pos = list( nrow(aux_xtab) - 1 ),
                          command = c( paste("\\hline \n") ) ) )

# Tabla Rendimientos SALUD con ingresos y gastos -----------------------------------------------------
aux <- rendimientos_netos
aux$rendimiento_bruto<-aux$rendimiento_bruto*100
aux$rendimiento_neto_menos_g_adm<-aux$rendimiento_neto_menos_g_adm*100
aux$corte<-format(aux$corte, "%b/%Y")
aux$corte<-as.character(aux$corte)
aux_xtab <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rend_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla del detalle de los ingresos que produjeron las inversiones --------------------------------
aux <- ingresos
aux_xtab <- xtable( aux, digits = c( 0,rep(2,10) ) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingre_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list( pos = list( nrow(aux_xtab) - 1 ),
                          command = c( paste("\\hline \n") ) ) )

# Tabla del detalle de los gastos que produjeron las inversiones ----------------------------------
aux <- gastos_opera
aux_xtab <- xtable( aux, digits = c( 0, rep(2, 10 ) ) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list( pos = list( nrow(aux_xtab) - 1 ),
                          command = c( paste("\\hline \n") )) )

# Tabla inversiones en créditos -----------------------------------------------------------------
aux <- creditos
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rendimiento_ponderado_real<-aux$rendimiento_ponderado_real*100
aux$ano <- as.character( aux$ano )
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 6) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_creditos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla evolución Inversiones en Bonos del Estado Ecuatoriano --------------------------------------
aux <- inv_instrumento %>%
  mutate(rdto_prom_pond = rdto_prom_pond * 100,
         rendimiento_ponderado_real = rendimiento_ponderado_real * 100 ) %>%
  filter( sectores == 'Bonos del Estado',
          valor_nominal > 0) %>%
  dplyr::select( -sectores, -inflacion)

aux$ano <- as.character( aux$ano )
aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla evolución Inversiones en sector público en renta fija---------------------------------------
aux <- sp_saldo
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 5) ) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sector_publico_saldo_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list( pos = list( nrow(aux_xtab) - 1 ),
                          command = c( paste("\\hline \n") ) ) )

aux <- sp_rendimiento
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 5) ))
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sector_publico_rend_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux <- sp_plazo
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 5) ))
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sector_publico_plazo_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla detalle Inversiones en Bonos del Estado Ecuatoriano al corte--------------------------------
aux <- detalle_bonos
aux_xtab <- xtable( aux, digits = c( 0, 0, 0,  2, 2, 2, 0, 0, 0) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Tabla recuperación de bonos y cupones del estado--------------------------------------------------
aux <- recuperacion_bonos
aux$fecha_cupon <- as.character( aux$fecha_cupon )
aux$fecha_vcmto <- as.character( aux$fecha_vcmto )
aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 0, 0) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_recuperacion_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(14,15),#nrow(aux)-1,
       sanitize.text.function = identity )

# Tabla Resumen recuperación de bonos y cupones por año---------------------------------------------
aux <- resumen_recup
aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_resumen_bonos_recuperacion_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list( pos = list( nrow(aux_xtab) - 1 ),
                          command = c(paste("\\hline \n") )) )

# Tabla saldo sector público------------------------------------------------------------------------
aux <- sp_saldo
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 5) ))
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sp_saldo_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list( pos = list( nrow(aux_xtab)-1 ),
                          command = c( paste("\\hline \n") ) ) )

# Tabla rendimiento sector público------------------------------------------------------------------
aux <- sp_rendimiento
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 5) ) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sp_rendimiento_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list( pos = list( nrow(aux_xtab) - 1 ),
                          command = c(paste("\\hline \n") ) ) )

# Tabla plazos promedio sector público--------------------------------------------------------------
aux <- sp_plazo
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 5) ) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sp_plazo_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list( pos = list( nrow(aux_xtab) - 1 ),
                          command = c(paste("\\hline \n") )) )

# Tabla Evolución Inversiones en Obligaciones-------------------------------------------------------
aux <- inv_instrumento %>%
  mutate(rdto_prom_pond = rdto_prom_pond * 100,
         rendimiento_ponderado_real = rendimiento_ponderado_real * 100 ) %>%
  filter( sectores == 'Obligaciones',
          valor_nominal > 0) %>%
  dplyr::select( -sectores, -inflacion )

aux$ano <- as.character( aux$ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_obligaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla detalle de las inversiones en obligaciones--------------------------------------------------
aux <- detalle_obligaciones
aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 4), 0, 0 ) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_oblig_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla Evolución Inversiones en Titularizaciones---------------------------------------------------
aux <- inv_instrumento %>%
  mutate(rdto_prom_pond = rdto_prom_pond * 100,
         rendimiento_ponderado_real = rendimiento_ponderado_real * 100 ) %>%
  filter( sectores == 'Titularizaciones' ) %>%
  dplyr::select( -sectores, -inflacion )

aux$ano <- as.character( aux$ano )
aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_titularizaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla Tasas de aportes----------------------------------------------------------------------------
aux <- primas_sal %>%
  mutate( cd1 = cd1 * 100,
          cd2 = cd2 * 100 )
aux$anio <- as.character(aux$anio)

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_primas_aporte_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
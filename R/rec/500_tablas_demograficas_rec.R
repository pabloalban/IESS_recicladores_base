message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura tabla información finaciera ' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_REC_tablas_demografia.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Tabla de reclicladores por edad y sexo-------------------------------------------------------------
message( '\tTabla de reclicladores por edad y sexo' )
aux <- edad_sexo

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 0, 0 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla de porcentaje de reclicladores por edad y sexo------------------------------------------------
message( '\tTabla de porcentaje de reclicladores por edad y sexo' )
aux <- porc_edad_sexo

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_porc_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla de reclicladores por instrucción y sexo------------------------------------------------------
message( '\tTabla de reclicladores por instrucción y sexo' )
aux <- instr_sexo

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_instr_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla de ingresos promedio por edad y sexo----------------------------------------------------------
message( '\tTabla de ingresos promedio por edad y sexo' )
aux <- edad_sal_prom

aux_xtab <- xtable( aux, digits = c( 0, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_edad_sal_prom', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla de ingresos de reciclaje por edad y sexo-----------------------------------------------------
message( '\tTabla de ingresos de reciclaje por edad y sexo' )
aux <- edad_sal_reciclaje

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_edad_sal_reciclaje', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla de ingresos totales por edad y sexo----------------------------------------------------------
message( '\tTabla de ingresos totales por edad y sexo' )
aux <- edad_sal_total

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_edad_sal_total', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla de afiliados por sexo------------------------------------------------------------------------
message( '\tTabla del número de afiliados por sexo' )
aux <- afiliados_sexo

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_afiliados_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla de afiliados antiguos por sexo------------------------------------------------------------------------
message( '\tTabla del número de personas que estuvieron afiliadas por sexo' )
aux <- afiliados_antiguos_sexo

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_afiliados_antiguos_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )


message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura tabla información finaciera ' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_REC_tablas_demografia.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Tabla de reclicladores por edad y sexo-------------------------------------------------------------

message( '\tTabla de reclicladores por edad y sexo' )
aux <- edad_sexo 

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla de reclicladores por instrucción y sexo------------------------------------------------------

message( '\tTabla de reclicladores por instrucción y sexo' )

aux <- instr_sexo %>% 
  mutate( mujer = as.integer( mujer ),
          hombre = as.integer( hombre ),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_instr_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,nrow(aux)),
       sanitize.text.function = identity )

#Tabla de reclicladores por provincia y sexo---------------------------------------------------------

message( '\tTabla de reclicladores por provincia y sexo' )
aux <- prov_sexo %>% 
  mutate( mujer = as.integer( mujer ),
          hombre = as.integer( hombre ),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_prov_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,nrow(aux)),
       sanitize.text.function = identity )

#Tabla de ingresos promedio por edad y sexo----------------------------------------------------------

message( '\tTabla de ingresos promedio por edad y sexo' )
aux <- edad_sexo_ingreso

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, ncol( aux ) - 1 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_edad_sal_prom', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,nrow(aux)),
       sanitize.text.function = identity )

#Tabla de  rangos de ingreso de reciclaje por sexo-------------------------------------------------------

message( '\tTabla de rangos de ingreso total por sexo' )
aux <- rang_sal_rec %>%
  mutate( sexo_reciclador_mujer = as.integer(sexo_reciclador_mujer), 
          sexo_reciclador_hombre = as.integer(sexo_reciclador_hombre),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_rang_sal_rec', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux )-1, nrow( aux ) ),
       sanitize.text.function = identity )


#Tabla de  rangos de ingreso total por sexo---------------------------------------------------------

message( '\tTabla de rangos de ingreso total por sexo' )

aux <- rang_sal_total %>%
  mutate( sexo_reciclador_mujer = as.integer( sexo_reciclador_mujer ), 
          sexo_reciclador_hombre = as.integer( sexo_reciclador_hombre ),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_rang_sal_total', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla de  rangos de ingreso total en jovenes-------------------------------------------------------

message( '\tTabla de rangos de ingreso en jovenes' )

aux <- rang_sal_total_joven %>%
  mutate( sexo_reciclador_mujer = as.integer( sexo_reciclador_mujer ), 
          sexo_reciclador_hombre = as.integer( sexo_reciclador_hombre ),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_rang_sal_joven', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )


#Tabla de afiliados por sexo------------------------------------------------------------------------
message( '\tTabla del número de afiliados por sexo' )
aux <- afiliados_sexo %>%
  mutate( mujer = as.integer( mujer ), 
          hombre = as.integer( hombre ),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_afiliados_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla de afiliados antiguos por sexo------------------------------------------------------------------------
message( '\tTabla del número de personas que estuvieron afiliadas por sexo' )

aux <- afiliados_antiguos_sexo %>%
  mutate( mujer = as.integer( mujer ), 
          hombre = as.integer( hombre ),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rec_afiliados_antiguos_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,nrow(aux)),
       sanitize.text.function = identity )

#Tabla de discapacitados con carnet-----------------------------------------------------------------
message( '\tTabla del número de personas discapacitadas con carnet' )

aux <- discapacidad_con_carnet

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, ncol( aux ) - 1 ) ) )

#aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_discapacidad_con_carnet', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )

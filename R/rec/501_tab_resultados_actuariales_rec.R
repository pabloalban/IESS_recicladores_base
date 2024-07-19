message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura resultados actuariales ' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_resultados_actuariales.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Tabla de tasas de aportación de la CD 515 para voluntarios-----------------------------------------

message( '\tTabla de tasas de aportación de la CD 515 para voluntarios' )

aux <- tasas_legal 

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, ncol( aux ) - 1 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_tasas_legal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla de Aportes según CD 515----------------------------------------------------------------------

message( '\tTabla de aportes según la CD 515' )

aux <- aportes_legal 

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, ncol( aux ) - 1 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_aportes_legal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla de Pensiones mínimas-------------------------------------------------------------------------

message( '\tTabla de pensiones mínimas' )

aux <- minimos_pen 

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_minimos_pen', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla de resultados de valuaciones actuariales-----------------------------------------------------

message( '\tTabla de resultados de valuaciones actuariales' )

aux <- resultados 

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, ncol( aux ) - 1 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_resultados', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,nrow( aux ) ),
       sanitize.text.function = identity )

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )

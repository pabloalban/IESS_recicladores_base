message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tTabla Activo del Fondo de Salud al 31 de diciembre de cada año' )
# Tabla afiliados activos 2005-2018 a diciembre-----------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SAL_analisis_financiero.RData' ) )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_situacion_actual.RData' ) )

aux <- copy( activo )
aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual ]
aux[ , anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_activo_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # COMPONENTES DEL ACTIVO
# message( '\tTabla Análisis de los componentes del activo del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_comp_activo.RData' ) ) 
# aux <- copy( comp_activo )
# #aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2  ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_componentes_activo_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = 1,
#        sanitize.text.function = identity )
# 
# # ANALISIS HORIZONTAL ACTIVO ---------------------------------------------------------------------
message( '\tTabla Análisis horizontal del activo del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_activo.RData' ) ) 
aux <- copy( Anali_horiz_act )
aux[ , A1 := 100 * A1 ]
aux[ , A2 := 100 * A2 ]
aux[ , A3 := 100 * A3 ]
aux[ , A4 := 100 * A4 ]
aux[ , A5 := 100 * A5 ]
aux[ , A6 := 100 * A6 ]
aux[ , A7 := 100 * A7 ]
aux[ , A8 := 100 * A8 ]
aux[ , A9 := 100 * A9 ]
aux[ , A10 := 100 * A10 ] 
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_horiz_activo_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 11,
       sanitize.text.function = identity )

# # analisis vertical activo
message( '\tTabla Análisis vertical del activo del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_activo.RData' ) ) 
aux <- copy( Anali_vert_act )
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_vertical_activo_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 14,
       sanitize.text.function = identity )

# cuentas por cobrar
message( '\tTabla Cuentas por cobrar del Fondo de Salud' )
load( file = paste0( parametros$RData_seg, 'IESS_SAL_cuentas_cobrar.RData' ) ) 
aux <- copy( ctas_x_cobrar )
aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual ]
aux[ , anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_cobrar_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # Subcuentas de cuentas por cobrar
# message( '\tTabla Subcuentas de cuentas por cobrar del Fondo de Salud' )
# #load( file = paste0( parametros$RData_seg, 'IESS_SAL_subs_cobrar.RData' ) ) 
# 
# aux <- copy( ctas_sub_x_cobrar )
# aux[ , anio := as.character( anio ) ]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_subcuentas_cobrar_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# 
# # PASIVO -----------------------------------------------------------------------------------------
message( '\tTabla Pasivo del Fondo de Salud' )
load( file = paste0( parametros$RData_seg, 'IESS_SAL_pasivo.RData' ) ) 
aux <- copy( Pasivo )
aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual ]
aux[ , anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pasivo_sal', '.tex' ), 
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # COMPONENTES DEL PASIVO -------------------------------------------------------------------------
message( '\tTabla Análisis de los componentes del pasivo del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_comp_pasivo.RData' ) ) 
aux <- copy( Comp_pasivo )
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_pasivo_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 12,
       sanitize.text.function = identity )

# # ANÁLISIS HORIZONTAL PASIVO ---------------------------------------------------------------------
message( '\tTabla Análisis horizontal del pasivo del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_pasivo.RData' ) ) 
aux <- copy( Anali_Horiz_Pasivo )
aux[ , A1 := 100 * A1 ]
aux[ , A2 := 100 * A2 ]
aux[ , A3 := 100 * A3 ]
aux[ , A4 := 100 * A4 ]
aux[ , A5 := 100 * A5 ]
aux[ , A6 := 100 * A6 ]
aux[ , A7 := 100 * A7 ]
aux[ , A8 := 100 * A8 ]
aux[ , A9 := 100 * A9 ]
aux[ , A10 := 100 * A10 ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 , 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_horiz_pasivo_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 6,
       sanitize.text.function = identity )

# # ANALISIS VERTICAL PASIVO -----------------------------------------------------------------------
# message( '\tTabla Análisis vertical del pasivo del Fondo de Salud' )
# #load( file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_pasivo.RData' ) ) 
# aux <- copy( Anali_Vert_Pasivo )
# aux[ , A1 := 100 * A1]
# aux[ , A2 := 100 * A2]
# aux[ , A3 := 100 * A3]
# aux[ , A4 := 100 * A4]
# aux[ , A5 := 100 * A5]
# aux[ , A6 := 100 * A6]
# aux[ , A7 := 100 * A7]
# aux[ , A8 := 100 * A8]
# aux[ , A9 := 100 * A9]
# aux_xtable <- xtable( aux, digits = c(  0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_vertical_pasivo_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = 1,
#        sanitize.text.function = identity )
# # CUENTAS POR PAGAR ------------------------------------------------------------------------------
# message( '\tTabla prestadores por pagar del Fondo de Salud al 31 de diciembre de cada año' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_cuentas_pagar.RData' ) ) 
# aux <- copy( Pres_Pagar )
# aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
# aux[ , anio := as.character( anio ) ]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_prestadores_pagar_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
#  
# # CUENTAS DEL PASIVO NO CORRIENTE ----------------------------------------------------------------
# message( '\tTabla pasivo no corriente del Fondo de Salud al 31 de diciembre de cada año' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_cuentas_pagar.RData' ) ) 
# aux <- copy( Pas_no_corr )
# aux[ , anio := as.character( anio ) ]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_pasivo_no_corriente_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# 
# PATRIMONIO----------------------------------------------------------------------------------------
message( '\tTabla Patrimonio del Fondo de Salud al 31 de diciembre de cada año' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_patrimonio.RData' ) )
aux <- copy( Patrimonio )
aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual ]
aux[ , anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# COMPONENTES DEL PATRIMONIO------------------------------------------------------------------------
message( '\tTabla Análisis de los componentes del patrimonio del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_comp_patrimonio.RData' ) ) 
aux <- copy( Com_Patrimonio )
# aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_patrimonio_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 6,
       sanitize.text.function = identity )

# # ANÁLISIS HORIZONTAL PATRIMONIO -----------------------------------------------------------------
message( '\tTabla Análisis horizontal del patrimonio del Fondo de Salud' )
load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_patrimonio.RData' ) )

aux <- copy( Anali_horiz_Patr )
aux[ , A1 := 100 * A1 ]
aux[ , A2 := 100 * A2 ]
aux[ , A3 := 100 * A3 ]
aux[ , A4 := 100 * A4 ]
aux[ , A5 := 100 * A5 ]
aux[ , A6 := 100 * A6 ]
aux[ , A7 := 100 * A7 ]
aux[ , A8 := 100 * A8 ]
aux[ , A9 := 100 * A9 ]
aux[ , A10 := 100 * A10 ]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_horiz_patrimonio_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 3,
       sanitize.text.function = identity )

# # ANÁLISIS VERTICAL PATRIMONIO -------------------------------------------------------------------
# message( '\tTabla Análisis vertical del patrimonio del Fondo del Seguro de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_patrimonio.RData' ) ) 
# aux <- copy( Anali_vert_patr )
# aux[ , A1 := 100 * A1]
# aux[ , A2 := 100 * A2]
# aux[ , A3 := 100 * A3]
# aux[ , A4 := 100 * A4]
# aux[ , A5 := 100 * A5]
# aux[ , A6 := 100 * A6]
# aux[ , A7 := 100 * A7]
# aux[ , A8 := 100 * A8]
# aux[ , A9 := 100 * A9]
# aux_xtable <- xtable( aux, digits = c(  0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_vertical_patrimonio_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = 1,
#        sanitize.text.function = identity )
# 
# # INGRESOS ---------------------------------------------------------------------------------------
message( '\tTabla Análisis de los Ingresos del Fondo del Seguro de Salud' )
load( file = paste0( parametros$RData_seg, 'IESS_SAL_ingresos.RData' ) )
aux <- copy(ingresos)
aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual ]
aux[ , anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # COMPONENTES DE LOS INGRESOS --------------------------------------------------------------------
message( '\tTabla Evolución de los componentes de los ingresos del Fondo de Salud.' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_comp_ingresoss.RData' ) ) 
aux <- copy( Comp_Ingresos.. )
# aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
# aux <- aux[ , 1:6]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 , 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_ingresoss1_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 12,
       sanitize.text.function = identity )

# aux <- copy( Comp_Ingresos.. )
# aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
# aux <- aux[ , c(1,7,8,9,10)]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2  ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_componentes_ingresoss2_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = 1,
#        sanitize.text.function = identity )
# 
# # nota 
# message( '\tTabla cuentas de intereses del fondo de salud a ser consideradas' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_ingresos_nota.RData' ) ) 
# aux <- copy( nota )
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_ingresos_nota_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# 
# # NOTA 1
# message( '\tTabla cuentas de intereses del fondo de salud a no ser consideradas' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_ingresos_nota_1.RData' ) ) 
# aux <- copy( nota_1 )
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_ingresos_nota1_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# 
# # ANALISIS HORIZONTAL ingresos -------------------------------------------------------------------
message( '\tTabla Análisis horizontal de los ingresos del Fondo de Salud.' )
load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_ingresos.RData' ) ) 

aux <- copy( Anali_horz_Ingresos )
aux[ , A1 := 100 * A1 ]
aux[ , A2 := 100 * A2 ]
aux[ , A3 := 100 * A3 ]
aux[ , A4 := 100 * A4 ]
aux[ , A5 := 100 * A5 ]
aux[ , A6 := 100 * A6 ]
aux[ , A7 := 100 * A7 ]
aux[ , A8 := 100 * A8 ]
aux[ , A9 := 100 * A9 ]
aux[ , A10 := 100 * A10 ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_horiz_ingresos_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 6,
       sanitize.text.function = identity )

# # ANALISIS VERTICAL INGRESOS ---------------------------------------------------------------------
# message( '\tTabla Análisis vertical de los Ingresos del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_ingresos.RData' ) ) 
# aux <- copy( Anali_vert_Ingresos )
# aux[ , A1 := 100 * A1]
# aux[ , A2 := 100 * A2]
# aux[ , A3 := 100 * A3]
# aux[ , A4 := 100 * A4]
# aux[ , A5 := 100 * A5]
# aux[ , A6 := 100 * A6]
# aux[ , A7 := 100 * A7]
# aux[ , A8 := 100 * A8]
# aux[ , A9 := 100 * A9]
# aux_xtable <- xtable( aux, digits = c(  0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_vertical_ingreso_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = 1,
#        sanitize.text.function = identity )
# 
# # APORTES ----------------------------------------------------------------------------------------
# message( '\tTabla Evolución de ingresos por aportes del Fondo de Salud.' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_ing_aportes.RData' ) ) 
# aux <- copy( Aportes )
# aux[ , a6 := 100 * a6]
# aux[ , anio := as.character( anio ) ]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_ing_aportes_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# 
# # Contribución del Estado
# message( '\tTabla Evolución histórica de la contribución del Estado' )
# #load( file = paste0( parametros$RData_seg, 'IESS_SAL_contrib_del_estado.RData' ) ) 
# 
# aux <- copy( contri_estado )
# aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
# aux[ , anio := as.character( anio ) ]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_contr_estado_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# # gastos
# message( '\tTabla Evolución histórica de los gastos' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_gasto.RData' ) )
aux <- copy(Egresos)
aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual ]
aux[ , anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_gastos_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # COMPONENTES DE LOS GASTOS ----------------------------------------------------------------------
message( '\tTabla Análisis del componente de la evolución de los gastos del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_comp_gastos.RData' ) ) 
aux <- copy( Comp_Egresos )
# aux <- aux[ , 1:6]
# aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_gastos1_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 8,
       sanitize.text.function = identity )
# 
# aux <- copy( Comp_Egresos )
# aux <- aux[ , c(1,7,8,9,10)]
# #aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2  ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_componentes_gastos2_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = 1,
#        sanitize.text.function = identity )
# 
# # ANALISIS HORIZONTAL GASTOS ---------------------------------------------------------------------
message( '\tTabla Análisis horizontal de la evolución de los gastos del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_gastos.RData' ) )
aux <- copy( Anali_horiz_Egresos )
aux[ , A1 := 100 * A1 ]
aux[ , A2 := 100 * A2 ]
aux[ , A3 := 100 * A3 ]
aux[ , A4 := 100 * A4 ]
aux[ , A5 := 100 * A5 ]
aux[ , A6 := 100 * A6 ]
aux[ , A7 := 100 * A7 ]
aux[ , A8 := 100 * A8 ]
aux[ , A9 := 100 * A9 ]
aux[ , A10 := 100 * A10 ]
aux_xtable <- xtable( aux, digits = c(  0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_horiz_gastos_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 4,
       sanitize.text.function = identity )

# # ANALISIS VERTICAL GASTO ------------------------------------------------------------------------
# message( '\tTabla Análisis vertical de la evolución de los gastos del Fondo de Salud' )
# #load( file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_gasto.RData' ) ) 
# 
# aux <- copy( Anali_Vert_Egresos )
# aux[ , A1 := 100 * A1]
# aux[ , A2 := 100 * A2]
# aux[ , A3 := 100 * A3]
# aux[ , A4 := 100 * A4]
# aux[ , A5 := 100 * A5]
# aux[ , A6 := 100 * A6]
# aux[ , A7 := 100 * A7]
# aux[ , A8 := 100 * A8]
# aux[ , A9 := 100 * A9]
# aux_xtable <- xtable( aux, digits = c(  0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_vertical_gasto_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = 1,
#        sanitize.text.function = identity )
# 
# # gastos prestacioanles
# message( '\tTabla Evolución de los egresos por pago de prestaciones Salud' )
# #load( file = paste0( parametros$RData_seg, 'IESS_SAL_gasto_prestac.RData' ) ) 
# 
# aux <- copy( Gast_Presta )
# aux[ , A10 := 100 * A10]
# aux[ , anio := as.character( anio ) ]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2,2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_gast_prest_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# 
# # OTROS gastos
# message( '\tTabla Evolución de Gastos de Administración del Fondo Salud' )
# #load( file = paste0( parametros$RData_seg, 'IESS_SAL_otros_gasto.RData' ) ) 
# 
# aux <- copy( Gast_Adminis )
# #aux[ , incremento_porcentual_anual := 100 * incremento_porcentual_anual]
# aux[ , anio := as.character( anio ) ]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_otros_gastos_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# 
# # OTROS gastos
# message( '\tTabla relación patrimonio gastos médicos' )
# aux <- copy( patri_gmedi_salud )
# aux[ , anio := as.character( anio ) ]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_patri_gmedi_salud_sal', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )

# # GASTOS PRESTACIONALES --------------------------------------------------------------------------
message( '\tTabla Análisis horizontal de la evolución de los gastos del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_gastos.RData' ) )
aux <- copy( Gast_Presta )
aux[ , A10 := 100 * A10]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2,2,2 ) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_horiz_Gast_Presta', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # Ingresos vs gastos
message( '\tTabla Análisis horizontal de la evolución de los gastos del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_gastos.RData' ) )
aux <- copy( Ing_vs_Gas )
aux$Anio <- as.character( aux$Anio )
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_Ing_vs_Gas', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # Rel_Pat_Gas
message( '\tTabla Análisis horizontal de la evolución de los gastos del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_gastos.RData' ) )
aux <- copy(Rel_Pat_Gas)
aux$Anio <- as.character( aux$Anio )
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_Rel_Pat_Gas', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # Aju_Bal_Gen
message( '\tTabla Análisis horizontal de la evolución de los gastos del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_gastos.RData' ) )
aux <- copy(Aju_Bal_Gen)
aux$Anio <- as.character( aux$Anio )
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_Aju_Bal_Gen', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# # Aju_del_Est
message( '\tTabla Análisis horizontal de la evolución de los gastos del Fondo de Salud' )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_gastos.RData' ) )
aux <- copy(Aju_del_Est)
aux$AÑO <- as.character( aux$AÑO )
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_Aju_del_Est', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

####################################################################################################
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

# 5.13 Egresos Prestacionales y Gastos de Administración -------------------------------------------
message( '\tLectura 5.13 Egresos Prestacionales y Gastos de Administración - Generando Tabla' )
load( file = paste0( parametros$RData_seg,'IESS_o5_13_T1_EgresosPrestacionales.RData' ) ) 
aux <- copy(as.data.table(o5_13_T1_EgresosPrestacionales) )

aux$Anio <- as.character(aux$Anio)
aux$Variacion_Anual <- aux$Variacion_Anual * 100
aux <- setnames( aux, 
                 c( 'Año', 'Egresos Prestacionales', 'Incremento Anual', 'Variación Anual(%)' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'tabla_o5_13_T1_EgresosPrestacionales', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# 5.13 Tabla2 Egresos Prestacionales y Gastos de Administración ------------------------------------
message( '\tLectura 5.13 Egresos Prestacionales y Gastos de Administración - Generando Tabla' )
load( file = paste0( parametros$RData_seg,'IESS_o5_13_T2_EgresosPrestacionales.RData' ) ) 
aux <- copy(as.data.table(o5_13_T2_EgresosPrestacionales) )

aux$Anio <- as.character(aux$Anio)
aux$Variacion_Anual <- aux$Variacion_Anual * 100
aux <- setnames( aux, 
                 c( 'Año', 'Gast_Admi', 'Incremento Anual', 'Variación Anual' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'tabla_o5_13_T2_EgresosPrestacionales', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# 5.13 Tabla3 Egresos Prestacionales y Gastos de Administración ------------------------------------
message( '\tLectura 5.13 Egresos Prestacionales y Gastos de Administración - Generando Tabla' )
load( file = paste0( parametros$RData_seg,'IESS_o5_13_T3_EgresosPrestacionales.RData' ) ) 
aux <- copy(as.data.table(o5_13_T3_EgresosPrestacionales) )

aux$Anio <- as.character(aux$Anio)
aux <- setnames( aux, c( 'Año', 'Egr_Prest', 'Gast_admi', 'Otros_gast', 'T_Egresos' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'tabla_o5_13_T3_EgresosPrestacionales', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# 5.13 Tabla4 Egresos Prestacionales y Gastos de Administración ------------------------------------
message( '\tLectura 5.13 Egresos Prestacionales y Gastos de Administración - Generando Tabla' )
load( file = paste0( parametros$RData_seg,'IESS_o5_13_T4_EgresosPrestacionales.RData' ) ) 
aux <- copy( as.data.table(o5_13_T4_EgresosPrestacionales ) )

aux$Anio <- as.character(aux$Anio)
aux[ ,2:5] <- aux[ ,2:5] * 100
aux <- setnames( aux, c( 'Año', 'Egr_Prest', 'Gast_admi', 'Otros_gast', 'T_Egresos' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'tabla_o5_13_T4_EgresosPrestacionales', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# 5.13 Tabla5 Egresos Prestacionales y Gastos de Administración ------------------------------------
message( '\tLectura 5.13 Egresos Prestacionales y Gastos de Administración - Generando Tabla' )
load( file = paste0( parametros$RData_seg,'IESS_o5_13_T5_EgresosPrestacionales.RData' ) ) 
aux <- copy( as.data.table(o5_13_T5_EgresosPrestacionales ) )

aux$Anio <- as.character(aux$Anio)
aux[ , 2:5 ] <- aux[ , 2:5 ] * 100
aux <- setnames( aux, c( 'Año', 'Egr_Prest', 'Gast_admi', 'Otros_gast', 'T_Egresos' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'tabla_o5_13_T5_EgresosPrestacionales', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# 5.14 Ingresos por aportes versus egresos prestacionales ------------------------------------------
message( '\tLectura 5.14 Ingresos por aportes versus egresos prestacionales - Generando Tabla' )
load( file = paste0( parametros$RData_seg,'IESS_o5_14_IngresosAportesVSEgresos.RData' ) ) 
aux <- copy( as.data.table(o5_14_IngresosAportesVSEgresos ) )

aux$Anio <- as.character(aux$Anio)
aux[ , 6:7 ] <- aux[ , 6:7 ] * 100
aux <- setnames( aux, c( 'Año', 'A_afil', 'Egr_Prest', '60_Egre', 'Resultado',
                         'Util_Anual', 'Val_Acumul', 'Acu_Reserva' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 , 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'tabla_o5_14_IngresosAportesVSEgresos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# 5.15 Comparación de ingresos por aportes versus lo gastos de administración del Fondo del Seguro de Salud ----
message( '\tLectura 5.15 Comparación de ingresos por aportes versus lo gastos de administración del Fondo del Seguro de Salud - Generando Tabla' )
load( file = paste0( parametros$RData_seg,'IESS_o5_15_ComparacionIngresosVSGast.RData' ) ) 
aux <- copy( as.data.table(o5_15_ComparacionIngresosVSGast) )

aux$Anio <- as.character(aux$Anio)
aux[ , 5:6 ] <- aux[ , 5:6 ] * 100
aux <- setnames( aux, c( 'Año', 'Apor_Afil', 'Gast_Admi', 'Result', 'Util_Anual', 'Val_Acumul' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 , 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'tabla_o5_15_ComparacionIngresosVSGast', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# 5.16 Análisis de las Reservas Disponibles --------------------------------------------------------
message( '\tLectura 5.16 Análisis de las Reservas Disponibles - Generando Tabla' )
load( file = paste0( parametros$RData_seg, 'IESS_o5_16_AnalisisReservasDisponibl.RData' ) ) 
aux <- copy( as.data.table(o5_16_AnalisisReservasDisponibl ) )

aux$Anio <- as.character(aux$Anio)
aux <- setnames( aux, c( 'Año', 'Egr_Prest', 'Patrimonio', 'Relacion' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'tabla_o5_16_AnalisisReservasDisponibl', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

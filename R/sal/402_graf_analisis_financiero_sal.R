message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tGráfico Activo del Fondo de Salud al 31 de diciembre de cada año' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SAL_analisis_financiero.RData' ) )

# Para ciertos gráficos de la situación actual
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_situacion_actual.RData' ) ) 
# Para ciertos gráficos de la Presentación de Resultados
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_presentacion_resultados.RData' ) )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_activo.RData' ) )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_cuentas_cobrar.RData' ) )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_pasivo.RData' ) ) 
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_cuentas_pagar.RData' ) )
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_patrimonio.RData' ) ) 
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_ingresos.RData' ) ) 
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_ing_aportes.RData' ) ) 
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_gasto.RData' ) ) 
# load( file = paste0( parametros$RData_seg, 'IESS_SAL_gasto_prestac.RData' ) ) 
# 
# Gráficos Activo del Fondo de Salud ---------------------------------------------------------------
unidad <- 1e6
aux <- copy( activo[ , .( anio,activo) ] )
aux[ , activo:=activo/unidad]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 1000, 12800 )
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_activo_fondo_salud <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = activo, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = NULL, y = 'Millones' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_activo_fondo_salud,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_activo_fondo_sal',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico Cuentas por cobrar del Fondo de Salud al 31 de diciembre de cada año ---------------------
message( '\tGráfico Cuentas por cobrar del Fondo de Salud al 31 de diciembre de cada año..' )
aux <- copy( ctas_x_cobrar)
unidad <- 1e6
aux <- aux[ ,.( anio, `cuentas por cobrar` ) ]
aux[ ,`cuentas por cobrar`:=`cuentas por cobrar`/unidad]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 1000, 11250 )
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_cobrar_salud <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = `cuentas por cobrar`, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = NULL, y = 'Millones' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_cuentas_cobrar_salud,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_cuentas_cobrar_salud_sal', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico Pasivo del Fondo de Salud ----------------------------------------------------------------
message( '\tGráfico Pasivo del Fondo de Salud' )
aux <- copy( Pasivo )
aux <- aux[ , .( anio, Pasivo) ]
unidad <- 1e6
aux[ , Pasivo := Pasivo/unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 4000 )
y_brk <- seq( y_lim[1], y_lim[2], 500 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pasivo_salud <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Pasivo, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = NULL, y = 'Millones' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_pasivo_salud,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_pasivo_salud_sal', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico Incremento Porcentual Anual del Fondo de Salud -------------------------------------------
# message( '\tGráfico Incremento Porcentual Anual del Fondo de Salud ' )
# aux <- copy( Pasivo )
# aux <- aux[ ,.( anio, incremento_porcentual_anual) ]
# aux <- aux[ , incremento_porcentual_anual:=100*incremento_porcentual_anual]
# aux <- aux[2:9]
# 
# x_lim <- c( 2011, 2018 )
# x_brk <- 2011:2018
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( -30, 210 )
# y_brk <- seq( y_lim[1], y_lim[2], 20 )
# y_lbl <- paste0(formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"%" )
# 
# iess_incremento_porc_salud <- ggplot( data = aux ) + 
#   geom_line( aes( x = anio, 
#                   y = incremento_porcentual_anual, 
#                   color = parametros$iess_blue ), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   labs( x = NULL, y = 'Millones' ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( '', '' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )
# 
# ggsave( plot = iess_incremento_porc_salud , 
#         filename = paste0( parametros$resultado_graficos, 'iess_incremento_porc_salud_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico prestadores por pagar del Fondo del Seguro de Salud al 31 de diciembre de cada año ----
# message( '\tGráfico prestadores por pagar del Fondo del Seguro de Salud al 31 de diciembre de cada año. ' )
# unidad <- 1e6
# aux <- copy( Pres_Pagar )
# aux <- aux[ ,.( anio,`Prestadores por pagar`) ]
# aux$`prestadores por pagar` <- aux$`prestadores por pagar`/unidad
# x_lim <- c( 2010, 2018 )
# x_brk <- 2010:2018
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( 0, 4000 )
# y_brk <- seq( y_lim[1], y_lim[2], 500 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# iess_prestadores_pagar_salud <- ggplot( data = aux ) + 
#   geom_line( aes( x = anio, 
#                   y = `prestadores por pagar`, 
#                   color = parametros$iess_blue ), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   labs( x = NULL, y = 'Millones' ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( '', '' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )
# 
# ggsave( plot = iess_prestadores_pagar_salud , 
#         filename = paste0( parametros$resultado_graficos, 'iess_prestadores_pagar_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Gráfico Patrimonio del Fondo de Salud (En millones de dólares ) ----------------------------------
message( '\tGráfico Patrimonio del Fondo de Salud (En millones de dólares ).' )
unidad <- 1e6
aux <- copy( Patrimonio )
aux <- aux[ , .( anio,patrimonio) ]
aux$patrimonio <- aux$patrimonio/unidad
x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 8800 )
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_patrimonio_salud <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = patrimonio, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = NULL, y = 'Millones' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_patrimonio_salud,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_patrimonio_sal', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico Variaciones del patrimonio de Salud por el periodo 2010-2018. -------------------------
# message( '\tGráfico Variaciones del patrimonio de Salud por el periodo 2010-2018.' )
# unidad <- 1e6
# aux <- copy( Patrimonio. )
# aux <- aux[ ,.( anio, incremento_porcentual_anual) ]
# aux <- aux[ , incremento_porcentual_anual:=100*incremento_porcentual_anual]
# aux <- aux[2:9]
# 
# x_lim <- c( 2011, 2018 )
# x_brk <- 2011:2018
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( -20, 160 )
# y_brk <- seq( y_lim[1], y_lim[2], 20 )
# y_lbl <- paste0(formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"%" )
# 
# iess_incremento_patrimonio_salud <- ggplot( data = aux ) + 
#   geom_line( aes( x = anio, 
#                   y = incremento_porcentual_anual, 
#                   color = parametros$iess_blue ), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   labs( x = NULL, y = NULL ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( '', '' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )
# 
# ggsave( plot = iess_incremento_patrimonio_salud , 
#         filename = paste0( parametros$resultado_graficos, 'iess_incremento_patrimonio_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Gráfico Evolución de los ingresos del Seguro de Salud --------------------------------------------
message( '\tGráfico Evolución de los ingresos del Seguro de Salud.' )
unidad <- 1e6
aux <- copy( ingresos )
aux <- aux[ ,.( anio,ingresos ) ]
aux$ingresos <- aux$ingresos/unidad
x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 4000 )
y_brk <- seq( y_lim[1], y_lim[2], 500 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evol_ingr_salud <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = ingresos, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = NULL, y = 'Millones' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_evol_ingr_salud,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_evol_ingr_salud_sal', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico Evolución de los ingresos por aportes del Fondo del Salud. -------------------------------
# message( '\tGráfico Evolución de los ingresos por aportes del Fondo del Salud.' )
# unidad <- 1e6
# aux <- copy( Aportes )
# aux <- aux[ ,.( anio,a4) ]
# aux$a4 <- aux$a4/unidad
# x_lim <- c( 2010, 2018 )
# x_brk <- 2010:2018
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( 0, 3000 )
# y_brk <- seq( y_lim[1], y_lim[2], 500 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# iess_evol_ingr_aportes_salud <- ggplot( data = aux ) + 
#   geom_line( aes( x = anio, 
#                   y = a4, 
#                   color = parametros$iess_blue ), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   labs( x = NULL, y = 'Millones' ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( '', '' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )
# 
# ggsave( plot = iess_evol_ingr_aportes_salud , 
#         filename = paste0( parametros$resultado_graficos, 'iess_evol_ingr_aportes_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico Variaciones de ingresos por aportes Salud ------------------------------------------------
# message( '\tGráfico Variaciones de ingresos por aportes Salud. ' )
# unidad <- 1e6
# aux <- copy( Aportes )
# aux <- aux[ ,.( anio, a6) ]
# aux <- aux[ , a6:=100*a6]
# aux <- aux[2:9]
# 
# x_lim <- c( 2011, 2018 )
# x_brk <- 2011:2018
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( -10, 100 )
# y_brk <- seq( y_lim[1], y_lim[2], 10 )
# y_lbl <- paste0(formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"%" )
# 
# iess_incremento_ingr_aportes_salud <- ggplot( data = aux ) + 
#   geom_line( aes( x = anio, 
#                   y = a6, 
#                   color = parametros$iess_blue ), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   labs( x = NULL, y = NULL ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( '', '' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )
# 
# ggsave( plot = iess_incremento_ingr_aportes_salud , 
#         filename = paste0( parametros$resultado_graficos, 'iess_incremento_ingr_aportes_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico Evolución del Gasto del Fondo de Salud ---------------------------------------------------
message( '\tGráfico Evolución del Gasto del Fondo de Salud' )
unidad <- 1e6
aux <- copy( Egresos )
aux <- aux[ , .( anio,`contribucion_del_estado` ) ]
aux$`contribucion_del_estado` <- aux$`contribucion_del_estado`/unidad
x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 2000 )
y_brk <- seq( y_lim[1], y_lim[2], 500 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evol_gastos_salud <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = `contribucion_del_estado`, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = NULL, y = 'Millones' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_evol_gastos_salud,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_evol_gastos_sal', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico Variación del Gasto del Fondo de Salud.   ------------------------------------------------------------------------
# message( '\tGráfico Variación del Gasto del Fondo de Salud.  ' )
# unidad <- 1e6
# aux <- copy( Egresos )
# aux <- aux[ ,.( anio, `incremento_porcentual_anual`) ]
# aux <- aux[ , `incremento_porcentual_anual`:=100*`incremento_porcentual_anual`]
# aux <- aux[2:9]
# 
# x_lim <- c( 2011, 2018 )
# x_brk <- 2011:2018
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( -50, 100 )
# y_brk <- seq( y_lim[1], y_lim[2], 10 )
# y_lbl <- paste0(formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"%" )
# 
# iess_incremento_gastos_salud <- ggplot( data = aux ) + 
#   geom_line( aes( x = anio, 
#                   y = `incremento_porcentual_anual`, 
#                   color = parametros$iess_blue ), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   labs( x = NULL, y = NULL ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( '', '' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )
# 
# ggsave( plot = iess_incremento_gastos_salud , 
#         filename = paste0( parametros$resultado_graficos, 'iess_incremento_gastos_salud_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 
# # Gráfico Incremento porcentual de los egresos por pago de prestaciones Salud --------------------
# message( '\tGráfico Incremento porcentual de los egresos por pago de prestaciones Salud.' )
# unidad <- 1e6
# aux <- copy( Gast_Presta )
# aux <- aux[ ,.( anio, A10 ) ]
# aux <- aux[ , A10:=100*A10]
# aux <- aux[2:9]
# 
# x_lim <- c( 2011, 2018 )
# x_brk <- 2011:2018
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( -60, 100 )
# y_brk <- seq( y_lim[1], y_lim[2], 10 )
# y_lbl <- paste0(formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"%" )
# 
# iess_incremento_egreso_pres_salud <- ggplot( data = aux ) + 
#   geom_line( aes( x = anio, 
#                   y = A10, 
#                   color = parametros$iess_blue ), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   labs( x = NULL, y = NULL ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( '', '' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )
# 
# ggsave( plot = iess_incremento_egreso_pres_salud , 
#         filename = paste0( parametros$resultado_graficos, 'iess_incremento_egreso_pres_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##################### Para gráficas de la situación actual #####################################
# unidad <- 1e6
# aux <- copy(evol_inver_salud)
# aux <- aux[is.na( sectorPF ),  sectorPF:=0]
# aux <- aux[is.na( sectorpubl ),  sectorpubl:=0]
# 
# tipo <- factor(c( 1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4
#                   ,1,2,3,4, 1,2,3,4) ,labels=c( "No Financiero"
#                                     ,"Público"
#                                     ,"Préstamos", "Caja" ) )
# AN <- factor(c( 1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4, 5,5,5,5
#                 ,6,6,6,6 ,7,7,7,7, 8,8,8,8),labels=c( "2011","2012","2013"
#                                         ,"2014","2015","2016","2017","2018" ) )
# valor <- c(  aux[1]$sectorPF,aux[1]$sectorpubl,aux[1]$prestamos, aux[1]$caja
#             ,aux[2]$sectorPF,aux[2]$sectorpubl,aux[2]$prestamos, aux[2]$caja
#             ,aux[3]$sectorPF,aux[3]$sectorpubl,aux[3]$prestamos, aux[3]$caja
#             ,aux[4]$sectorPF,aux[4]$sectorpubl,aux[4]$prestamos, aux[4]$caja
#             ,aux[5]$sectorPF,aux[5]$sectorpubl,aux[5]$prestamos, aux[5]$caja
#             ,aux[6]$sectorPF,aux[6]$sectorpubl,aux[6]$prestamos, aux[6]$caja
#             ,aux[7]$sectorPF,aux[7]$sectorpubl,aux[7]$prestamos, aux[7]$caja
#             ,aux[8]$sectorPF,aux[8]$sectorpubl,aux[8]$prestamos, aux[8]$caja )/unidad
# df <- data.frame(tipo, AN, valor)
# max( sum( aux[1,2:5] ), sum( aux[2,2:5] ),sum( aux[3,2:5] )
#      ,sum( aux[4,2:5] ),sum( aux[5,2:5] ),sum( aux[6,2:5] )
#      ,sum( aux[7,2:5] ), sum( aux[8,2:5] ) )/unidad # Esto para ver el limite en y
# y_lim <- c( 0, 800 )
# y_brk <- seq( y_lim[1], y_lim[2], 100 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# iess_evol_inver_salud <- ggplot(data=df, aes( x=AN, y=valor, fill=tipo) ) + 
#   geom_bar(stat="identity" ) +
#   scale_fill_manual(values=c(parametros$iess_green, parametros$iess_blue,"gray", "black" ) ) +
#   labs( x = NULL, y = "Millones" ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   theme_bw() +
#   plt_theme +
#   guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0 ) ) +
#   theme( legend.position="bottom",legend.box.spacing = unit(0.75, "cm" ) )
# 
# ggsave( plot = iess_evol_inver_salud, 
#         filename = paste0( parametros$resultado_graficos, 'iess_evol_inver_salud_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 
# #Evolución de las inversiones del SGSIF en préstamos y su rendimiento
# message( '\tGráfico Evolución de las inversiones del SGSIF en préstamos y su rendimiento' )
# unidad <- 1e6
# aux <- copy( evol_inver_pres_salud )
# aux[ , anio := as.character( anio) ]
# aux <- aux[is.na( prestamoQ ), prestamoQ:=0]
# aux <- aux[is.na( prestamoP ), prestamoP :=0]
# 
# tipo <- factor(c( 1,2, 1,2, 1,2, 1,2, 1,2, 1,2
#                   ,1,2, 1,2) ,labels=c( "Quirografario"
#                                                ,"Prendario" ) )
# AN <- factor(c( 1,1, 2,2, 3,3, 4,4, 5,5
#                 ,6,6, 7,7, 8,8),labels=c( "2011","2012","2013"
#                                         ,"2014","2015","2016","2017","2018" ) )
# valor <- c(  aux[1]$prestamoQ,aux[1]$prestamoP
#             ,aux[2]$prestamoQ,aux[2]$prestamoP
#             ,aux[3]$prestamoQ,aux[3]$prestamoP
#             ,aux[4]$prestamoQ,aux[4]$prestamoP
#             ,aux[5]$prestamoQ,aux[5]$prestamoP
#             ,aux[6]$prestamoQ,aux[6]$prestamoP
#             ,aux[7]$prestamoQ,aux[7]$prestamoP
#             ,aux[8]$prestamoQ,aux[8]$prestamoP)/unidad
# df <- data.frame(tipo, AN, valor)
# max( sum( aux[1,2:3] ), sum( aux[2,2:3] ),sum( aux[3,2:3] )
#      ,sum( aux[4,2:3] ),sum( aux[5,2:3] ),sum( aux[6,2:3] )
#      ,sum( aux[7,2:3] ), sum( aux[8,2:3] ) )/unidad # Esto para ver el limite en y
# y_lim <- c( 0, 400 )
# y_brk <- seq( y_lim[1], y_lim[2], 50 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# iess_evol_inver_pres_salud <- ggplot(data=df, aes( x=AN, y=valor, fill=tipo) ) + 
#   geom_bar(stat="identity" ) +
#   scale_fill_manual(values=c(parametros$iess_green, parametros$iess_blue,"gray", "black" ) ) +
#   labs( x = NULL, y = "Millones" ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   theme_bw() +
#   plt_theme +
#   guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0 ) ) +
#   theme( legend.position="bottom",legend.box.spacing = unit(0.75, "cm" ) )
# 
# ggsave( plot = iess_evol_inver_pres_salud, 
#         filename = paste0( parametros$resultado_graficos, 'iess_evol_inver_pres_salud_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 
# ##################### Para gráficas de la Presentación de Resultados #####################################
# message( '\tGráfico  Crecimiento de la población pensionista por viudedad' )
# unidad <- 1e6
# aux <- copy( primas_aporte_sal )
# aux <- aux[is.na(cd1), cd1:=0]
# aux <- aux[is.na(cd2), cd2:=0]
# aux <- aux[ ,anio:=as.character(substr(format( as.Date( anio),"%Y" ),1,4) ) ]
# aux1<- as.data.frame( aux)
# aux2<-data.frame(f=seq( 1, 11, 1 ), fecha=c( aux1[ ,1] ),cd1=c( aux1[ ,2] )*100,
#                  cd2=c( aux1[ ,3] )*100 )
# 
# x_lim <- c( 1, 11 )
# x_brk <- seq( x_lim[1], x_lim[2], 1 )
# x_lbl <- c( aux2[ ,2] )
# 
# y_lim <- c( 1, 15)
# y_brk <- seq( y_lim[1], y_lim[2], 1 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# iess_primas_aporte_sal <- ggplot( data = aux2 ) + 
#   geom_line( aes( x = f,  y = cd1, color = parametros$iess_blue), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   geom_line( aes( x = f, y = cd2, color = parametros$iess_green ), 
#              size = graf_line_size,
#              lineend = "round" ) +
#   labs( x = NULL, y = NULL ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( 'C.D.501', 'C.D.261' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = paste0( y_lbl,"%" ), limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 90, hjust = 1 ) ) +
#   theme( legend.box.spacing=unit(0.70,"cm" ) ) +
#   guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0 ) ) +
#   theme( legend.position="bottom" ) + theme( legend.title=element_blank() )
# 
# ggsave( plot = iess_primas_aporte_sal, 
#         filename = paste0( parametros$resultado_graficos, 'iess_primas_aporte_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 
# ##################### Para gráficas de los anexos de los menores de 18 años #####################################
# message( '\tGráfico Prima media nivelada' )
# unidad <- 1e6
# prima_media <- c(rep(0.01165,17) )
# prima_natural <- c( 0.008459697, 0.008798085, 0.009150009, 0.009516009, 0.009896649
#                    ,0.010292515, 0.010704216, 0.011132384, 0.01157768, 0.012040787
#                    ,0.012522419, 0.013023315, 0.013544248, 0.014086018, 0.014649459
#                    ,0.015235437, 0.015844854)
# aux<-data.frame(f=seq( 1, 17, 1 ),pm=prima_media*100,
#                  pn=prima_natural*100 )
# 
# x_lim <- c( 1, 17 )
# x_brk <- seq( x_lim[1], x_lim[2], 1 )
# x_lbl <- c( aux[ ,1] )
# 
# y_lim <- c( 0, 2)
# y_brk <- seq( y_lim[1], y_lim[2], 0.5 )
# y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# iess_primas_media_nivelada_sal <- ggplot( data = aux ) + 
#   geom_line( aes( x = f,  y = pm, color = parametros$iess_blue), 
#              size = graf_line_size,
#              lineend = "round" ) + 
#   geom_line( aes( x = f, y = pn, color = parametros$iess_green ), 
#              size = graf_line_size,
#              lineend = "round" ) +
#   labs( x = NULL, y = NULL ) +
#   scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
#                       labels = c( 'Prima media nivelada', 'Prima Natural' ) ) +
#   scale_y_continuous( breaks = y_brk, labels = paste0( y_lbl,"%" ), limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
#   theme_bw() +
#   plt_theme +
#   theme( axis.text.x = element_text( angle = 0, hjust = 0.5 ) ) +
#   guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0 ) ) +
#   theme( legend.position="bottom" ) + theme( legend.title=element_blank() ) +
#   theme( legend.box.spacing = unit(0.75, "cm" ) )
# 
# ggsave( plot = iess_primas_media_nivelada_sal, 
#         filename = paste0( parametros$resultado_graficos, 'iess_primas_media_nivelada_sal', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# # Gráfico gastos operacionales.   ------------------------------------------------------------------------
message( '\tGráfico gastos operacionales Salud.' )
unidad <- 1e6
aux <- copy( Gast_Presta )
# aux <- aux[ ,.( anio, A10 ) ]
# aux <- aux[ , A10:=100*A10]
# aux <- aux[2:9]
# 
x_lim <- c( 2010, 2020 )
x_brk <- 2011:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
aux$A9 <- as.numeric( aux$A9)/unidad
# 
y_lim <- c( 550, 2000 )
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
iess_Gast_Presta<-ggplot( data = aux ) + 
  geom_line( aes( x = Anio,
                  y = A9,
                  color = parametros$iess_blue ),
             linewidth = graf_line_size,
             lineend = "round" ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme + theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_Gast_Presta,
        filename = paste0( parametros$resultado_graficos,
                           'iess_Gast_Presta',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# # Gráfico relación patrimonio gasto --------------------------------------------------------------
message( '\tGráfico gastos operacionales Salud' )
#unidad <- 1e6
aux <- copy( Rel_Pat_Gas )
# aux <- aux[ ,.( anio, A10 ) ]
# aux <- aux[ , A10:=100*A10]
# aux <- aux[2:9]
# 
x_lim <- c( 2010, 2020 )
x_brk <- 2011:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
#aux$A9 <- as.numeric( aux$A9)/unidad
# 
y_lim <- c(0.50, 9)
y_brk <- seq( y_lim[1], y_lim[2], 0.8 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
iess_Rel_Pat_Gas<-ggplot( data = aux ) + 
  geom_line( aes( x = Anio,
                  y = A3,
                  color = parametros$iess_blue ),
             linewidth = graf_line_size,
             lineend = "round" ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_Rel_Pat_Gas,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_Rel_Pat_Gas', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Gráfico componentes pasivo ----------------------------------------------------------------------
message( '\tGráfico componentes pasivo.' )

table <- data.frame( valor = c(47.05,27.59,25.34,27.97,13.05,11.95),
                     tipos = c( "Otros pasivos corrientes","Atención médica a jubilados",
                                "Atención med. enf. catastróficas","Otros pasivos no corrientes",
                                "Cuentas por pagar años anteriores","Prest. med. por pagar" ) )
table$tipos <- as.factor( table$tipos )
bp <- ggplot( table, aes( x = "", y = valor, fill = tipos ) ) +
  geom_bar( width = 1, stat = "identity" )

pie <- bp + coord_polar( "y", start = 0 ) + scale_fill_brewer( "Blues" ) + 
  theme( axis.text.x=element_blank() ) +
  # geom_text( aes( y = Amount/3 + c(0, cumsum( Amount)[-length( Amount) ] ), 
  #               label = percent( Amount/100 ) ), 
  #           size=5)
  geom_text( aes(label = percent( valor/100 ) ),
             position = position_stack( vjust = 0.5 ),
             color = "black",
             size = 3 ) +
  theme_bw() +   
  #plt_theme_legend +
  theme_void() +
  theme( legend.position = 'bottom', 
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.key.size = unit( 0.5, 'cm' ),
         text = element_text( size = 10 ),
         legend.spacing.y = unit( -0.9, 'cm' ) )

ggsave( plot = pie ,
        filename = paste0( parametros$resultado_graficos,
                           'iess_comp_pasivo', 
                           parametros$graf_ext ),
        width = graf_width+2, height = graf_height, units = graf_units, dpi = graf_dpi )

# png(file = paste0( parametros$resultado_graficos, "iess_comp_pasivo", 
#                    parametros$graf_ext),
#     width=6, 
#     height=5,
#     units     = "cm",
#     res       = 1200,
#     pointsize = 4)
# pieval<-c(47.05,27.59,25.34,27.97,13.05,11.95)
# table <- data.frame(num = c(47.05,27.59,25.34,27.97,13.05,11.95),
#                     countries = c( "Otros pasivos corrientes","Atención médica a jubilados",
#                                   "Atención med. enf. catastróficas","Otros pasivos no corrientes",
#                                   "Cuentas por pagar años anteriores","Prest. med. por pagar" ) )
# label <-paste(table[ ,1],"%",sep="" )
# #pie3D(table$num,radius = 0.7,labels = label,col=hcl.colors(length(pieval), "Spectral" ),border="white",labelcex = 0.8,height=0.08,theta=pi/6)
# #par(bg = "aliceblue" )
# pie1 <- pie3D(table$num,radius = 0.8, 
#             col = hcl.colors( length( pieval ),
#                               "Spectral" ), 
#             explode = 0.08, 
#             border = FALSE, bty = "n" )
# 
# pie3D.labels( pie1, labels = label,labelrad = 1.3, labelcex = 0.9 )
# par( xpd = TRUE)
# legend( "top",legend = table$countries, cex=0.9, yjust = 0.5, 
#        fill =  hcl.colors(length( pieval ),
#                           "Spectral" ), 
#        bty = "n",
#        ncol = 2 )
# 
# dev.off()

## Gráfico componentes pasivo ----------------------------------------------------------------------
message( '\tRepresentatividad de los componentes del patrimonio.' )
table <- data.frame( valor = c( 0.8549, 0.1422, 0.0028 ),
                     tipos = c( "Fondos capitalizados"
                                ,"Resultados"
                                , "Otros Patrimonios" ) )
table$tipos <- as.factor( table$tipos )

bp <- ggplot( table, aes( x = "", y = valor, fill = tipos ) ) +
  geom_bar( width = 1, stat = "identity" )

pie <- bp + coord_polar( "y", start = 0 ) + scale_fill_brewer( "Blues" ) + 
  theme( axis.text.x=element_blank() ) +
  geom_text( aes(label = percent( valor , accuracy = 0.01 ) ),
             position = position_stack( vjust = 0.5 ),
             color = "black",
             size = 3 ) +
  theme_bw() +   
  theme_void() +
  theme( legend.position = 'bottom', 
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.key.size = unit( 0.5, 'cm' ),
         text = element_text( size = 10 ),
         legend.spacing.y = unit( -0.9, 'cm' ) )

ggsave( plot = pie ,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_comp_patrimonio',
                           parametros$graf_ext ),
        width = graf_width+2, height = graf_height, units = graf_units, dpi = graf_dpi )

# png(file = paste0( parametros$resultado_graficos, "iess_comp_patrimonio", parametros$graf_ext),width=6, height=5,
#     units     = "cm",
#     res       = 1200,
#     pointsize = 4)
# pieval<-c(85.49,14.22,0.28)
# table <- data.frame(num = c(85.49,14.22,0.28),
#                     countries = c( "Fondos capitalizados","Resultados",
#                                   "Otros Patrimonios" ) )
# label <-paste(table[ ,1],"%",sep="" )
# #pie3D(table$num,radius = 0.7,labels = label,col=hcl.colors(length(pieval), "Spectral" ),border="white",labelcex = 0.8,height=0.08,theta=pi/6)
# #par(bg = "aliceblue" )
# pie1<-pie3D(table$num,radius = 0.8,col=hcl.colors(length(pieval), "Spectral" ),explode = 0.08,border=FALSE,theta=pi/6)
# pie3D.labels(pie1,labels = label,labelrad=1.3,labelcex = 0.9)
# par( xpd = TRUE)
# legend( "top",legend = table$countries, cex=0.9, yjust=0.2, xjust = -0.1,  fill =  hcl.colors(length(pieval), "Spectral" ),bty="n",ncol=1)
# dev.off()

## Representatividad de los componentes del ingreso-------------------------------------------------------------------------------------------------------------------------
message( '\tRepresentatividad de los componentes del patrimonio.' )
table <- data.frame( valor = c(29,2.10,23,0.59,85,0.43) ,
                     tipos = c( "Otros Ingresos",
                                "Ap.jubilados y pensionistas",
                                "Ap.personales",
                                "Contribución del Estado",
                                "Aportes Patronales",
                                "Ingresos Financieros" ) )

table$tipos <- as.factor( table$tipos )
##

bp <- ggplot( table, aes( x = "", y = valor, fill = tipos ) ) +
  geom_bar( width = 1, stat = "identity" )

pie <- bp + coord_polar( "y", start = 0 ) + scale_fill_brewer( "Blues" ) + 
  theme( axis.text.x = element_blank() ) +
  # geom_text( aes( y = Amount/3 + c(0, cumsum( Amount)[-length( Amount) ] ), 
  #               label = percent( Amount/100 ) ), 
  #           size=5)
  geom_text( aes(label = percent( valor/100 ) ),
             position = position_stack( vjust = 0.5 ),
             color = "black",
             size = 3 ) +
  theme_bw() +   
  #plt_theme_legend +
  theme_void() +
  theme( legend.position = 'bottom', 
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.key.size = unit( 0.5, 'cm' ),
         text = element_text( size = 10 ),
         legend.spacing.y = unit( -0.9, 'cm' ) )

ggsave( plot = pie ,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_comp_ingreso', 
                           parametros$graf_ext ),
        width = graf_width + 2, height = graf_height + 1 , units = graf_units, dpi = graf_dpi )

# png(file = paste0( parametros$resultado_graficos, "iess_comp_ingreso", parametros$graf_ext),width=6, height=5,
#     units     = "cm",
#     res       = 1200,
#     pointsize = 4)
# #par(mar = c(0, 0, 0, 0 ) )
# pieval<-c(29,2.10,23,0.59,85,0.43)
# table <- data.frame(num = c(29,2.10,23,0.59,85,0.43) ,
#                     countries = c( "Otros Ingresos","Ap.jubilados y pensionistas","Ap.personales","Contribución del Estado","Aportes Patronales","Ingresos Financieros" ) )
# label <-paste(table[ ,1],"%",sep="" )
# #pie3D(table$num,radius = 0.7,labels = label,col=hcl.colors(length(pieval), "Spectral" ),border="white",labelcex = 0.8,height=0.08,theta=pi/6)
# #par(bg = "aliceblue" )
# pie1<-pie3D(table$num,radius = 0.8,col=hcl.colors(length(pieval), "Spectral" ),explode = 0.1,border=FALSE,theta=pi/7)
# pie3D.labels(pie1,labels = label,labelrad=1.3,labelcex = 0.9)
# legend( "top",legend = table$countries, cex=0.8, yjust=0.2, xjust = -0.1,  fill =  hcl.colors(length(pieval), "Spectral" ),bty="n",ncol=2)
# dev.off()

## Representatividad de los componentes del egreso-------------------------------------------------------------------------------------------------------------------------
message( '\tRepresentatividad de los componentes del egreso.' )
table <- data.frame( valor = c(0.57,5.35,6.42,87.66),
                     tipos = c( "Otros egresos \n operacionales directos",
                                "Egresos prestacionales \n por subsidios",
                                "Egresos prestacionales \n por atención médica",
                                "Otros Egresos" ) )
table$tipos <- as.factor( table$tipos )
##

bp <- ggplot( table, aes( x = "", y = valor, fill = tipos ) ) +
  geom_bar( width = 1, stat = "identity" )

pie <- bp + coord_polar( "y", start = 0 ) + scale_fill_brewer( "Blues" ) + 
  theme( axis.text.x=element_blank() ) +
  geom_text( aes(label = percent( valor/100 ) ),
             position = position_stack( vjust = 0.5 ),
             color = "black",
             size = 4 ) +
  theme_bw() +   
  theme_void() +
  theme( legend.position = 'bottom', 
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.key.size = unit( 0.5, 'cm' ),
         text = element_text( size = 10 ),
         legend.spacing.y = unit( -0.9, 'cm' ) )

ggsave( plot = pie ,
        filename = paste0( parametros$resultado_graficos,
                           'iess_comp_egresos',
                           parametros$graf_ext ),
        width = graf_width + 6, height = graf_height + 2, units = graf_units, dpi = graf_dpi )

# png(file = paste0( parametros$resultado_graficos, "iess_comp_egresos", parametros$graf_ext),width=6, height=5,
#     units     = "cm",
#     res       = 1200,
#     pointsize = 4)
# #par(mar = c(0, 0, 0, 0 ) )
# pieval<-c(0.57,5.35,6.42,87.66)
# table <- data.frame(num = c(0.57,5.35,6.42,87.66),
#                     countries = c( "Otros egresos operacionales directos","Egresos prestacionales por subsidios","Egresos prestacionales por atención médica","Otros Egresos" ) )
# label <-paste(table[ ,1],"%",sep="" )
# #pie3D(table$num,radius = 0.7,labels = label,col=hcl.colors(length(pieval), "Spectral" ),border="white",labelcex = 0.8,height=0.08,theta=pi/6)
# #par(bg = "aliceblue" )
# pie1<-pie3D(table$num,radius = 0.8,col=hcl.colors(length(pieval), "Spectral" ),explode = 0.1,border=FALSE,theta=pi/6)
# pie3D.labels(pie1,labels = label,labelrad=1.1,labelcex = 0.6)
# par( xpd = TRUE)
# legend( "top",legend = table$countries, cex=0.8, yjust=0.2, xjust = -0.1,  fill =  hcl.colors(length(pieval), "Spectral" ),bty="n",ncol=1)
# dev.off()

## Represetatividad de representatividad de los componentes del activo  ----------------------------------------------------------------------------------------------------------------------
#message( '\tRepresentatividad de los de los componentes del activo' )

#png(file = paste0( parametros$resultado_graficos, "iess_comp_activo", parametros$graf_ext),width=15, height=10, units = "cm",
#    pointsize = 1,res = 1200 )

my_data <- data.frame(
  stringsAsFactors = F,
  Section = c( "Deuda de Gobierno", 
               "Propiedad Planta \n y Equipo", 
               "Otras Cuentas por cobrar",
               "Fondos Disponibles", "Anticipo" ),
  Detail = letters[22:26],
  Amount = c(0.3109, 0.0792, 0.0933, 0.1036,0.4131)
)
v<- c(0.3109, 0.4131, 0.0933, 0.0907,0.1036)
v1<-c(0.0907,0.0127,0.0001)
a=c(0.0907,0.0127,0.001)
ae=a / sum( a ) * 2
d=c( "Inversiones","Fondos Disponibles","Otros Activos" )
db<-data.frame( a, ae, d )

my_data_aug <- my_data %>%
  mutate( arc_start = cumsum( lag( Amount, default = 0 ) ) * 2*pi-2 ,
          arc_end   = cumsum( Amount ) * 2*pi-2 ,
          x_pos = 0 + cos( arc_start - pi/2 ),
          y_pos = 1 - sin( arc_start - pi/2 ),
          mid_angle = 0.5*( arc_start + arc_end ) )

my_data_detail <- my_data_aug %>%
  mutate( amount_scaled = Amount / sum( Amount) * 2 )

my_data_lines <- my_data_aug %>% slice( 4, 5 )

iess_comp_activo <- ggplot( my_data_aug ) +
  geom_arc_bar( aes( x0 = 0, y0 = 1,  
                     r0 = 0, r  = 1,
                     fill = Section,
                     start = arc_start,
                     end = arc_end), color = NA ) +
  geom_text( aes( x = 0.8 * sin( mid_angle ) - 0.01, y = 0.8 * cos( mid_angle ) + 0.9, label = percent( Amount) ) ) +
  geom_tile(data = db,
            aes( x = 2, 
                 y = cumsum( ae ) - ae/2,
                 height = ae, fill = d) ) +
  geom_text( data = db,aes( x = 2, y = c(1,1.9,2.09), label = percent( a ) ) ) +
  annotate( "segment", x = my_data_lines[ 1:2, "x_pos" ],
            y = my_data_lines[ 1:2, "y_pos" ], xend = 1.5,
            yend = c(2,0 ) ) + coord_equal() + theme_void() +
  scale_x_continuous( name = "", breaks = NULL, labels = NULL ) +
  scale_y_continuous( name = "", breaks = NULL, labels = NULL ) +
  theme_bw() + plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = "horizontal",
         legend.key.size = unit( 0.5, 'cm' ),
         text = element_text( size = 10 ),
         legend.spacing.y = unit( -0.9, 'cm' ) )

# Corrección del gráfico
my_data <- data.frame(
  stringsAsFactors = F,
  Section = c( "Deuda de Gobierno"
               , "Propiedad Planta \n y Equipo"
               , "Otras Cuentas por cobrar"
               , "Anticipo"
               , "Inversiones"
               , "Fondos disponibles"
               , "Otros activos" ),
  #Detail = letters[22:26],
  Amount = c(0.3109, 0.0792, 0.0933,0.4131, 0.0907, 0.0127, 0.0001 )
)
my_data$Section <- as.factor( my_data$Section )

bp <- ggplot( my_data, aes( x = "", y = Amount, fill = Section ) ) +
  geom_bar( width = 1, stat = "identity" )
iess_comp_activo <- bp + 
  coord_polar( "y", start = 0 ) + scale_fill_brewer( "Blues" ) + 
  theme( axis.text.x = element_blank() ) +
  geom_text( aes(label = percent( Amount) ),
             position=position_stack( vjust = 0.5 ),
             color = 'black',# "#a86e16",
             size = 3 ) +
  theme_bw() +   
  #plt_theme_legend +
  theme_void() +
  theme( legend.position = 'bottom', 
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.key.size = unit( 0.5, 'cm' ),
         text = element_text( size = 10 ),
         legend.spacing.y = unit( -0.9, 'cm' ) )
# # ---
# aux1 <- copy( my_data )
# aux1 <- as.data.table( aux1 )
# aux1[ , Por:= Amount ]
# aux1[ , Amount:=NULL]
# 
# aux1 <- data.frame( aux1)
# 
# aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
#   mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
#           start_angle = lag(end_angle, default = 0 ),   
#           mid_angle = 0.5*(start_angle + end_angle) ) %>% 
#   mutate( aux1,
#           hjust = ifelse(mid_angle>pi, 1, 0 ),
#           vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1) )
# 
# rpie = 1 
# rlabel = 0.6 * rpie
# rlabel2= 1.05 * rpie
# aux1 <- data.table( aux1)
# iess_pastel_afi_cotiz_riesgo_mascul <- ggplot( aux1) + 
#   geom_arc_bar( aes( x0 = 0, y0 = 0, r0 = 0, r = rpie,
#                    start = start_angle, end = end_angle, fill = Section ),
#                colour='white' ) +
#   geom_label( data = aux1[ 1, 1:8 ],
#               aes( x = rlabel * sin( mid_angle ) - 0.05, y = rlabel * cos( mid_angle ) + 0.2,
#                   label = paste0( formatC( round( Por*100, 2 ), decimal.mark = ',' ), '%' ),
#               ), family = tipo_letra, size=3, 
#               fill='white', color = "black" ) +
#   geom_label( data=aux1[ 2, 1:8 ],
#               aes( x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#                   label = paste0(formatC(round( Por*100,2), decimal.mark = ',' ), '%' ),
#               ), family = tipo_letra, size=3, 
#               fill='white', color = "black" ) +
#   geom_label( data=aux1[ 3, 1:8 ],
#               aes( x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#                   label = paste0(formatC(round( Por*100,2), decimal.mark = ',' ), '%' ),
#               ), family = tipo_letra, size=3, 
#               fill='white', color = "black" ) +
#   geom_label( data=aux1[ 4, 1:8 ],
#               aes( x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#                   label = paste0(formatC(round( Por*100,2), decimal.mark = ',' ), '%' ),
#               ), family = tipo_letra, size=3, 
#               fill='white', color = "black" ) +
#   geom_label( data=aux1[ 5, 1:8 ],
#               aes( x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#                   label = paste0(formatC(round( Por*100,2), decimal.mark = ',' ), '%' ),
#               ), family = tipo_letra, size=3, 
#               fill='white', color = "black" ) +
#   geom_label( data=aux1[ 6, 1:8 ],
#               aes( x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#                   label = paste0(formatC(round( Por*100,2), decimal.mark = ',' ), '%' ),
#               ), family = tipo_letra, size=3, 
#               fill='white', color = "black" ) +
#   geom_label( data=aux1[ 7, 1:8 ],
#               aes( x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#                   label = paste0(formatC(round( Por*100,2), decimal.mark = ',' ), '%' ),
#               ), family = tipo_letra, size=3, 
#               fill='white', color = "red" ) + ######
#   # geom_label(data=aux1[riesgos%notin%c('Cumple requisitos para vejez en 1 año o menos', 'Cumple requisitos para vejez en 5 años' ) ],
#   #            aes( x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#   #                label = paste0(formatC(round( Por*100,2), decimal.mark = ',' ), '%' ),
#   #                hjust = hjust, vjust = vjust), family = tipo_letra, size=3,
#   #            fill='white', color = "black" ) +
# coord_fixed() + scale_fill_brewer(palette="Blues" ) +
#   scale_x_continuous(limits = c(-1, 1.0 ), name = "", breaks = NULL, labels = NULL ) +
#   scale_y_continuous(limits = c(-1, 1.1), name = "", breaks = NULL, labels = NULL ) +
#   # theme_bw() +   
#   # plt_theme_legend +
#   # theme( legend.position = 'bottom', legend.direction = "vertical",
#   #       legend.key.size = unit(0.3, 'cm' ),
#   #       text = element_text( size = 9),
#   #       legend.spacing.y = unit( -0.9, 'cm' ) )
# theme_bw() +   
#   #plt_theme_legend +
#   theme_void() +
#   theme( legend.position = 'bottom', 
#         legend.direction = "horizontal",
#         legend.title = element_blank(),
#         legend.key.size = unit( 0.5, 'cm' ),
#         text = element_text( size = 10 ),
#         legend.spacing.y = unit( -0.9, 'cm' ) )

ggsave( plot = iess_comp_activo,
        filename = paste0( parametros$resultado_graficos,
                           'iess_comp_activo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# dev.off()

### Representatividad de los  gastos operacionales 

#png(file = paste0( parametros$resultado_graficos, "iess_gtos_operacionales", parametros$graf_ext) )

Egresos <- c( "Otros gastos \n en afiliados y \n jubilados"
              ,"Otros gastos \n directos"
              ,"Gastos directos \n de personal"
              , "Servicios \n prestacionales u \n provinciales "
              ,"Compensación \n gastos médicos"
              ,"Conv.\n interinstitucionales"
              , "Gastos prestacionales \n por atención médica"
              ,"Gastos \n prestacionales \n por subsidios" )
valor <- c(0.04,1.93,2.99,0.43,0.02,0.001,88.45,6.14)

rgo <- data.frame( Egresos, valor, tipo = Egresos )

iess_gtos_operacionales <- ggplot( data = rgo, aes( x = Egresos, y = valor, fill = tipo ) ) + 
  geom_bar( stat = "identity", position = "dodge" ) + 
  scale_fill_brewer() +
  theme_bw() +
  theme_void() +
  plt_theme_legend +
  geom_text( aes(label = paste0( valor, "%" ) ), vjust = -0.50, hjust = -0.25,
             position = position_dodge( width = 0.9 ), size = 2, 
             color = "black" ) +
  scale_y_discrete( name = "", breaks = NULL, labels = Egresos ) +
  theme( legend.position = 'none' #'bottom'
         , legend.direction = "horizontal"
         ,legend.key.size = unit( 0.2, 'cm' )
         ,text = element_text( size = 6 )
         ,legend.spacing.y = unit( -0.7, 'cm' ) )

# iess_gtos_operacionales <- ggplot( data = rgo, aes( x = pgo, y = go, color = go ) ) +
#   geom_bar(stat="identity",position = 'dodge' ) +
#   scale_fill_viridis_c() +
#   geom_text( aes(label = paste0(pgo,"%" ) ), vjust=-0.50,hjust=-0.25,
#             position = position_dodge(width=0.9), size = 2,color="black" ) +
#   theme_bw() +
#   plt_theme_legend +
#   scale_x_continuous( name = "", breaks = NULL, labels = NULL ) +
#   scale_y_discrete( name = "", breaks = NULL, labels = go) +
#   theme( legend.position = 'bottom', legend.direction = "horizontal"
#         ,legend.key.size = unit(0.2, 'cm' ),text = element_text( size = 10 )
#         ,legend.spacing.y = unit(-0.7, 'cm' ) )

ggsave( plot = iess_gtos_operacionales ,
        filename = paste0( parametros$resultado_graficos,
                           'iess_gtos_operacionales', 
                           parametros$graf_ext ),
        width = graf_width + 2, height = graf_height, units = graf_units, dpi = graf_dpi )

# dev.off()

# 5.13 Tabla1 Egresos Prestacionales y Gastos de Administración ------------------------------------
message( '\tLectura 5.13 T1 Egresos Prestacionales y Gastos de Administración - Generando gráfico' )
load( file = paste0(parametros$RData_seg, 'IESS_o5_13_T1_EgresosPrestacionales.RData' ) ) # carga el Rdata
#------------------------------------------------------------------.
aux <- copy( as.data.table(o5_13_T1_EgresosPrestacionales ) )                                                # Data temporal
#aux <- separate( aux,Anio,c( "Anio","P" ),sep =" " )                                # separo los caracteres de colum Anio
#aux <- aux[ ,-"P"]                                                               # Elimino columna P: de caracteres
#aux$Anio <- as.numeric( aux$Anio)                                                # Colum Anio como numérico 

a <- aux[ , 1 ]
b1 <- min( a )         # inicio del intervalo x
b2 <- max( a )         # fin del intervalo x
a <- aux[ , 2 ]
b3 <- min( a )         # inicio del intervalo y
b4 <-max( a )          # fin del intervalo y
b5 <- (b4-b3)/10

x_lim <- c(b1,b2)                                                               # limites del eje X 
x_brk <- seq( x_lim[1 ], x_lim[2],1)                                               # genera la grilla del eje x
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )                                   #?

y_lim <- c(500000000, 1900000000 )                                                               # limites del eje X 
y_brk <- seq( y_lim[1], y_lim[2], b5 )                                              # genera la grilla del eje x
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )                                   #?

o5_13_T1_EgresosPrestacionales_Fig <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio,                                                     # Etiqueta columna x RData - aux
                  y = aux$Egresos_Prestacionales,                                        # Etiqueta columna Y RData - aux
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año',                                                              # Nombre del eje X - en el gráfico
        y = 'Egresos Prestacionales (Millones)' ) +                                                           # Nombre del eje y - en el gráfico
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1 ) )

# o5_13_T1_EgresosPrestacionales_Fig                                                                  # genera la figura

ggsave( plot = o5_13_T1_EgresosPrestacionales_Fig,                                                      # guarda la figura
        filename = paste0( parametros$resultado_graficos, 
                           'o5_13_T1_EgresosPrestacionales_Fig',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 5.13 Tabla2 Egresos Prestacionales y Gastos de Administración ------------------------------------
message( '\tLectura 5.13 T2 Egresos Prestacionales y Gastos de Administración - Generando gráfico' )
load( file = paste0(parametros$RData_seg, 'IESS_o5_13_T2_EgresosPrestacionales.RData' ) ) # carga el Rdata

aux <- copy( as.data.table(o5_13_T2_EgresosPrestacionales ) )                                                # Data temporal

a <- aux[ , 1 ]
b1 <- min( a )         # inicio del intervalo x
b2 <- max( a )         # fin del intervalo x
a <- aux[ , 2 ]
b3 <- min( a )         # inicio del intervalo y
b4 <-max( a )          # fin del intervalo y
b5 <- (b4-b3)/10

x_lim <- c(b1,b2)                                                               # limites del eje X 
x_brk <- seq( x_lim[1], x_lim[2], 1 )                                               # genera la grilla del eje x
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )                                   #?

y_lim <- c(b3,b4)                                                               # limites del eje X 
y_brk <- seq( y_lim[1], y_lim[2], b5 )                                              # genera la grilla del eje x
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )                                   #?

o5_13_T2_EgresosPrestacionales_Fig <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio,                                                     # Etiqueta columna x RData - aux
                  y = aux$Gastos_Administracion,                                        # Etiqueta columna Y RData - aux
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año',                                                              # Nombre del eje X - en el gráfico
        y = 'Gastos de Administración (Millones)' ) +                                                           # Nombre del eje y - en el gráfico
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

o5_13_T2_EgresosPrestacionales_Fig                                                                      # genera la figura

ggsave( plot = o5_13_T2_EgresosPrestacionales_Fig  ,                                                      # guarda la figura
        filename = paste0( parametros$resultado_graficos, 
                           'o5_13_T2_EgresosPrestacionales_Fig', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 5.13 Tabla5 Egresos Prestacionales y Gastos de Administración ------------------------------------
message( '\tLectura 5.13 T5 Egresos Prestacionales y Gastos de Administración - Generando gráfico' )
load( file = paste0( parametros$RData_seg, 'IESS_o5_13_T5_EgresosPrestacionales.RData' ) ) # carga el Rdata

aux <- copy( as.data.table(o5_13_T5_EgresosPrestacionales ) )                                                # Data temporal
Media_Egr_Prest <- mean( aux$Egresos_Prestacionales )
Media_Gast_Admi <- mean( aux$Gastos_Administracion )
Media_Otro_Gast <- mean( aux$Otros_Gastos )

aux1 <- data.frame( categorias = c( "Egresos Prestacionales"
                                    , "Gastos de Administración(contribución)"
                                    , "Otros Gastos" ),
                    porcentaje = c(Media_Egr_Prest,Media_Gast_Admi,Media_Otro_Gast) )
table <- data.frame( tipos = c( "Egresos Prestacionales"
                                , "Gastos de Administración(contribución)"
                                , "Otros Gastos" ),
                     valor = c(Media_Egr_Prest,Media_Gast_Admi,Media_Otro_Gast) )

#
bp <- ggplot( table, aes( x = "", y = valor, fill = tipos ) ) +
  geom_bar( width = 1, stat = "identity" )

o5_13_T5_EgresosPrestacionales_Fig <- bp + coord_polar( "y", start = 0 ) + scale_fill_brewer( "Blues" ) + 
  theme( axis.text.x=element_blank() ) +
  geom_text( aes(label = percent( valor, accuracy = 0.01 ) ),
             position = position_stack( vjust = 0.5 ),
             color = "black",
             size = 3 ) +
  theme_bw() +   
  theme_void() +
  theme( legend.position = 'bottom', 
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.key.size = unit( 0.5, 'cm' ),
         text = element_text( size = 10 ),
         legend.spacing.y = unit( -0.9, 'cm' ) )

ggsave( plot = o5_13_T5_EgresosPrestacionales_Fig,
        filename = paste0( parametros$resultado_graficos, 
                           'o5_13_T5_EgresosPrestacionales_Fig', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
# message( paste( rep('-', 100 ), collapse = '' ) )
# rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] ) #borra todas las variables
# gc()
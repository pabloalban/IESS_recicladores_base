message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tGraficando sensibilidad de la tasa actuarial' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
graf_width <- 18
graf_height <- 12

load( file = parametros$sal_rdata_ana_sen )

esc_sens_lst <- c( 'sensibilidad_1', 'sensibilidad_2', 'sensibilidad_3' )

# --------------------------------------------------------------------------------------------------
message( '\tGraficando variación del déficit/superávit tras la modificación de la tasa actuarial' )

load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_analisis_', esc_sens_lst[ 1 ], '.RData' ) )
load( paste0( parametros$sal_rdata_icomp_conf_esc, esc_sens$escenario, '.RData' ) )

aux <- balance_anual_sens[ sens == 1, list( t, sim = as.factor( sim ), V ) ]

cols_fun <- colorRampPalette( c( 'red', parametros$iess_green, 'gold', parametros$iess_blue ) )
cols_graf <- cols_fun( length( unique( aux$sim ) ) )

x_lim <- c( min( aux$t ), max( aux$t ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk , digits = 0, decimal.mark = ',', big.mark = '.', format = 'f' )

y_lim <- c( min( aux$V ), max( aux$V ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk / 1e6, digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' )

sim_lab <- c( esc$apo_act$i_a[1], esc_sens$var_tas )
sim_lab <- paste0( '$i^{', 1:length( sim_lab ), '}_a = ', formatC( 
  100 * sim_lab, 
  digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' ), '\\%$' )
sim_lab <- unname( TeX( sim_lab ) )

plt_sens <- ggplot( data = aux ) +
  geom_hline( yintercept = 0, colour = 'darkgreen' ) +
  geom_line( aes( x = t, y = V, colour = sim ), linewidth = graf_line_size ) +
  xlab( 'Tiempo' ) +
  ylab( 'Millones de USD' ) +
  scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
  scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
  scale_colour_manual( "",  
                       breaks = unique( aux$sim ),
                       labels = sim_lab,
                       values = cols_graf ) +
  plt_theme_legend

ggsave( plot = plt_sens, 
        filename = paste0( parametros$resultado_graficos, 'iess_bal_ana_sen_1', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( '\tGraficando variación del déficit/superavit tras la modificación del crecimiento de salarios' )

load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_analisis_', esc_sens_lst[ 2 ], '.RData' ) )
load( paste0( parametros$sal_rdata_icomp_conf_esc, esc_sens$escenario, '.RData' ) )

aux <- balance_anual_sens[ sens == 2, list( t, sim = as.factor( sim ), V ) ]

cols_fun <- colorRampPalette( c( 'red', parametros$iess_green, 'gold', parametros$iess_blue ) )
cols_graf <- cols_fun( length( unique( aux$sim ) ) )

x_lim <- c( min( aux$t ), max( aux$t ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk , digits = 0, decimal.mark = ',', big.mark = '.', format = 'f' )

y_lim <- c( min( aux$V ), max( aux$V ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk / 1e6, digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' )

sim_lab <- c( 1, esc_sens$cam_tas )
sim_lab <- paste0( '$i^{', 1:length( sim_lab ), '}_r = ', formatC( 
  c( 1, esc_sens$cam_tas ), 
  digits = 2, decimal.mark = '.', big.mark = ',', format = 'f' ), '\\, i_r$' )
sim_lab <- unname( TeX( sim_lab ) )

plt_sens <- ggplot( data = aux ) +
  geom_hline( yintercept = 0, colour = 'darkgreen' ) +
  geom_line( aes( x = t, y = V, colour = sim ), linewidth = graf_line_size ) +
  xlab( 'Tiempo' ) +
  ylab( 'Millones de USD' ) +
  scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
  scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
  scale_colour_manual( "",  
                       breaks = unique( aux$sim ),
                       labels = sim_lab,
                       values = cols_graf ) +
  plt_theme_legend

ggsave( plot = plt_sens, 
        filename = paste0( parametros$resultado_graficos, 'iess_bal_ana_sen_2', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( '\tGraficando variación del déficit/superavit tras la modificación del porcentaje de aportes de activos' )

load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_analisis_', esc_sens_lst[ 3 ], '.RData' ) )
load( paste0( parametros$sal_rdata_icomp_conf_esc, esc_sens$escenario, '.RData' ) )

aux <- balance_anual_sens[ sens == 3, list( t, sim = as.factor( sim ), V ) ]

cols_fun <- colorRampPalette( c( 'red', parametros$iess_green, 'gold', parametros$iess_blue ) )
cols_graf <- cols_fun( length( unique( aux$sim ) ) )

x_lim <- c( min( aux$t ), max( aux$t ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk , digits = 0, decimal.mark = ',', big.mark = '.', format = 'f' )

y_lim <- c( min( aux$V ), max( aux$V ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk / 1e6, digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' )

sim_lab <- c( esc$apo_act$por_apo_2[ 1 ], esc_sens$por_apo_2 )
sim_lab <- paste0( '$\\pi^{2,', 1:length( sim_lab ), '} = ', formatC( 
  100 * sim_lab, 
  digits = 3, decimal.mark = ',', big.mark = '.', format = 'f' ), '\\%$' )
sim_lab <- unname( TeX( sim_lab ) )

plt_sens <- ggplot( data = aux ) +
  geom_hline( yintercept = 0, colour = 'darkgreen' ) +
  geom_line( aes( x = t, y = V, colour = sim ), linewidth = graf_line_size ) +
  xlab( 'Tiempo' ) +
  ylab( 'Millones de USD' ) +
  scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
  scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
  scale_colour_manual( "",  
                       breaks = unique( aux$sim ),
                       labels = sim_lab,
                       values = cols_graf ) +
  plt_theme_legend

ggsave( plot = plt_sens, 
        filename = paste0( parametros$resultado_graficos, 'iess_bal_ana_sen_3', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

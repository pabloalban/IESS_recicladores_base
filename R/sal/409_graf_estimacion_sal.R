message( paste( rep( '-', 100 ), collapse = '' ) )

load( file = paste0( parametros$RData_seg, 'IESS_SAL_estimacion.RData' ) )

source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Comportamiento de lambda -------------------------------------------------------------------------
class_u_c <- as.character( cut( c( 0, 5, seq( 20, 60, 20 ) ), 
                                breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                                include.lowest = TRUE, right = FALSE, 
                                ordered_result = FALSE ) )
class_u_c <- c( paste( 'F', class_u_c, 'C' ), paste( 'M', class_u_c, 'C' ) )


class_u_e <- as.character( cut( c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ) ), 
                                breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                                include.lowest = TRUE, right = FALSE, 
                                ordered_result = FALSE ) )
class_u_e <- c( paste( 'F', class_u_e, 'E' ), paste( 'M', class_u_e, 'E' ) )


aux_c <- copy( ben_est_fre[ enf == 'C' ] )
aux_c[ , g := factor( paste( sexo, u, enf ), levels = class_u_c, ordered = TRUE ) ]

aux_e <- copy( ben_est_fre[ enf == 'E' ] )
aux_e[ , g := factor( paste( sexo, u, enf ), levels = class_u_e, ordered = TRUE ) ]

# gráfico frecuencia enfermedades catastróficas ----------------------------------------------------
plt_lambda_C <- ggplot() + 
  geom_step( data = aux_c, aes( x = L, y = lambda, colour = g ), 
             size = 0.5, color = 'blue' ) +
  # scale_x_continuous( limits = c( 0, 40 ), breaks = seq( 0, 40, 5 ) ) +
  scale_x_continuous( limits = c( 0, 6 ), breaks = seq( 0, 6, 1 ) ) +
  xlab( 'tiempo' ) +
  ylab( 'lambda' ) +
  facet_wrap( ~ g, nrow = 2, ncol = 5, # scales = 'free_y',
              labeller = labeller( dif = function( x ) paste0( 'grupo ', x ) ) ) +
  iess_timeseries_theme

ggsave( plot = plt_lambda_C, 
        filename = paste0( parametros$resultado_graficos, 'iess_lambda_catas', parametros$graf_ext ),
        width = 24, height = 12, dpi = 200, units = 'cm' )

# gráfico frecuencia enfermedades no catastróficas -------------------------------------------------
plt_lambda_M_E <- ggplot() + 
  geom_step( data = aux_e[ sexo == 'M' ], aes( x = L, y = lambda, colour = g ), size = 0.5, color = 'blue' ) +
  scale_x_continuous( limits = c( 0, 6 ), breaks = seq( 0, 6, 1 ) ) +
  # scale_y_continuous( limits = c( 0, 1 ), breaks = seq( 0, 1, 0.2 ) ) +
  xlab( 'tiempo' ) +
  ylab( 'lambda' ) +
  facet_wrap( ~ g, nrow = 2, ncol = 8, # scales = 'free_y',
              labeller = labeller( dif = function( x ) paste0( 'grupo ', x ) ) ) +
  iess_timeseries_theme

ggsave( plot = plt_lambda_M_E, 
        filename = paste0( parametros$resultado_graficos, 'iess_lambda_nocatas_m', parametros$graf_ext ),
        width = 24, height = 12, dpi = 200, units = 'cm' )

plt_lambda_F_E <- ggplot() + 
  geom_step( data = aux_e[ sexo == 'F' ], aes( x = L, y = lambda, colour = g ), size = 0.5, color = 'blue' ) +
  scale_x_continuous( limits = c( 0, 6 ), breaks = seq( 0, 6, 1 ) ) +
  # scale_y_continuous( limits = c( 0, 1 ), breaks = seq( 0, 1, 0.2 ) ) +
  xlab( 'tiempo' ) +
  ylab( 'lambda' ) +
  facet_wrap( ~ g, nrow = 2, ncol = 8, # scales = 'free_y',
              labeller = labeller( dif = function( x ) paste0( 'grupo ', x ) ) ) +
  iess_timeseries_theme

ggsave( plot = plt_lambda_F_E, 
        filename = paste0( parametros$resultado_graficos, 'iess_lambda_nocatas_f', parametros$graf_ext ),
        width = 24, height = 12, dpi = 200, units = 'cm' )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

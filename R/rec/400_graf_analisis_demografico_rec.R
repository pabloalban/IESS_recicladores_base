message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGraficando demografía del SGRT' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_REC_tablas_demografia.RData' ) )
load( paste0( parametros$RData, 'MIESS_censo_recicladores_ajustado.RData' ) )

# Pirámide por edad y sexo -------------------------------------------------------------------------

message( '\tGraficando población de recicladores base por edad y sexo' )

aux <- edad_sexo_int %>%
  mutate( porcentaje = lx_int / sum( lx_int, na.rm = TRUE ) ) %>% 
  mutate( porcentaje = if_else( g == 'mujer',
                         -porcentaje,
                         porcentaje ) ) %>%
  arrange( g, x )

salto_y <- 0.005
brks_y <- seq( -0.04, 0.04, salto_y )
lbls_y <- paste0( formatC( 100 * abs( brks_y ), digits = 1, format = 'f', big.mark = '.', decimal.mark = ',' ), '%' )
salto_x <- 10
brks_x <- seq( 15, 100, salto_x )
lbls_x <- paste0( as.character( brks_x ) )

rec_pir_porc_edad_sexo <- ggplot( aux, aes( x = x, y = porcentaje, fill = g ) ) +
  xlab( 'Edad' ) +
  #ylab( 'Porcentaje por grupo' ) +
  geom_bar( data = aux %>% filter( g == 'mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( g == 'hombre' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 15, 90 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Hombres', 'Mujeres' ) )

ggsave( plot = rec_pir_porc_edad_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_porc_edad_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Pirámide por instrucción y sexo ------------------------------------------------------------------

message( '\tGraficando población de recicladores base por instrucción y sexo' )

aux <- pir_instr_sexo %>%
  mutate( porcentaje = ifelse( sexo_reciclador == 'mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, ( instruccion ) )

salto_x <- 5
brks_y <- seq( -25, 25, salto_x )
lbls_y <- paste0( as.character( c( seq( 25, 0, -salto_x ), seq( salto_x, 25, salto_x ) ) ), '%')

rec_pir_instr_sexo <- ggplot( aux, aes( x = instruccion, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Instruccion' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'hombre' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Hombres', 'Mujeres' ) )

ggsave( plot = rec_pir_instr_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_instr_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Pirámide por provincia y sexo --------------------------------------------------------------------

message( '\tGraficando población de recicladores base por provincia y sexo' )
aux <- pir_prov_sexo %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, provincia )

salto_x <- 5
brks_y <- seq( -55, 55, salto_x )
lbls_y <- paste0( as.character( c( seq( 55, 0, -salto_x ), seq( salto_x, 55, salto_x ) ) ), '%')

rec_pir_prov_sexo <- ggplot( aux, aes( x = provincia, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Provincia' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'hombre' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Hombres', 'Mujeres' ) )

ggsave( plot = rec_pir_prov_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_prov_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Pirámide de ingreso promedio por edad y sexo -----------------------------------------------------

message( '\tGraficando ingreso promedio por instrucción y sexo' )
aux <- pir_edad_sal_prom %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'mujer',
                                -promedio,
                                promedio ) ) %>%
  arrange( sexo_reciclador, edad_mies )

salto_y <- 50
brks_y <- seq( -250, 500, salto_y )
lbls_y <- paste0( paste0( '$', as.character( c( seq( -brks_y[1], 0, -salto_y ), seq( salto_y, brks_y[ length( brks_y ) ], salto_y ) ) ) ) )

rec_pir_edad_sal_prom <- ggplot( aux, aes( x = edad_mies, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Instruccion' ) +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'hombre' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Hombres', 'Mujeres' ) )

ggsave( plot = rec_pir_edad_sal_prom, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_edad_sal_prom',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pirámide de ingreso de reciclaje según edad y sexo-------------------------------------------------

message( '\tGraficando ingreso ingreso de reciclaje según edad y sexo' )
aux <- pir_edad_sal_reciclaje %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, edad_mies )

salto_x <- 5
brks_y <- seq( -45, 45, salto_x )
lbls_y <- paste0( as.character( c( seq( 45, 0, -salto_x ), seq( salto_x, 45, salto_x ) ) ), '%')

rec_pir_edad_sal_reciclaje <- ggplot( aux, aes( x = edad_mies, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Instruccion' ) +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'hombre' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Hombres', 'Mujeres' ) )

ggsave( plot = rec_pir_edad_sal_reciclaje, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_edad_sal_reciclaje',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pirámide de ingreso total según edad y sexo--------------------------------------------------------

message( '\tGraficando ingreso ingreso total según edad y sexo' )
aux <- pir_edad_sal_total %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, edad_mies )

salto_x <- 5
brks_y <- seq( -45, 45, salto_x )
lbls_y <- paste0( as.character( c( seq( 45, 0, -salto_x ), seq( salto_x, 45, salto_x ) ) ), '%')

rec_pir_edad_sal_total <- ggplot( aux, aes( x = edad_mies, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Instruccion' ) +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'hombre' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Hombres', 'Mujeres' ) )

ggsave( plot = rec_pir_edad_sal_total, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_edad_sal_total',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pirámide de afiliados por sexo---------------------------------------------------------------------

message( '\tGraficando población de afiliados por sexo' )
aux <- pir_afiliados_sexo %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, caracteristica_social_afiliado )

salto_x <- 10
brks_y <- seq( -100, 100, salto_x )
lbls_y <- paste0( as.character( c( seq( 100, 0, -salto_x ), seq( salto_x, 100, salto_x ) ) ), '%')

rec_pir_afiliados_sexo <- ggplot( aux, aes( x = caracteristica_social_afiliado, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'caracteristica social' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'hombre' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Hombres', 'Mujeres' ) )

ggsave( plot = rec_pir_afiliados_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_afiliados_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pirámide de afiliados antiguos por sexo------------------------------------------------------------

message( '\tGraficando población de afiliados antiguos por sexo' )
aux <- pir_afiliados_antiguos_sexo %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, caracteristica_social_estuvo_afiliado )

salto_x <- 10
brks_y <- seq( -100, 100, salto_x )
lbls_y <- paste0( as.character( c( seq( 100, 0, -salto_x ), seq( salto_x, 100, salto_x ) ) ), '%')

rec_pir_afiliados_antiguos_sexo <- ggplot( aux, aes( x = caracteristica_social_estuvo_afiliado, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Caracteristica Social' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'hombre' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Hombres', 'Mujeres' ) )

ggsave( plot = rec_pir_afiliados_antiguos_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_afiliados_antiguos_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

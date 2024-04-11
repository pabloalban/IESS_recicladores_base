message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGraficando demografía del SGRT' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rec/200_estadisticas_descriptivas_rec.R', encoding = 'UTF-8', echo = FALSE)

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_REC_tablas_demografia.RData' ) )

# Pirámide por edad y sexo ----------------------------------------------------

message( '\tGraficando población de recicladores base por edad y sexo' )

aux <- pir_porc_edad_sexo %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'Mujer',
                         -porcentaje,
                         porcentaje ) ) %>%
  arrange( sexo_reciclador, edad_mies )

salto_x <- 15
brks_y <- seq( -75, 75, salto_x )
lbls_y <- paste0( as.character( c( seq( 75, 0, -salto_x ), seq( salto_x, 75, salto_x ) ) ), '%')

rec_pir_porc_edad_sexo <- ggplot( aux, aes( x = edad_mies, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Rangos de Edad' ) +
  ylab( 'Porcentaje por grupo' ) +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Hombre' ), stat = 'identity', colour = 'white') +
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

ggsave( plot = rec_pir_porc_edad_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_rec_pir_porc_edad_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Pirámide por instrucción y sexo ----------------------------------------------------

message( '\tGraficando población de recicladores base por instrucción y sexo' )
aux <- pir_instr_sexo %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'Mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, instruccion )

salto_x <- 15
brks_y <- seq( -60, 60, salto_x )
lbls_y <- paste0( as.character( c( seq( 60, 0, -salto_x ), seq( salto_x, 60, salto_x ) ) ), '%')

rec_pir_instr_sexo <- ggplot( aux, aes( x = instruccion, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Instruccion' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Hombre' ), stat = 'identity', colour = 'white') +
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


# Pirámide de ingreso promedio por edad y sexo ----------------------------------------------------

message( '\tGraficando ingreso promedio por instrucción y sexo' )
aux <- pir_edad_sal_prom %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'Mujer',
                                -promedio,
                                promedio ) ) %>%
  arrange( sexo_reciclador, edad_mies )

salto_x <- 15
brks_y <- seq( -440, 440, salto_x )
lbls_y <- paste0( as.character( c( seq( 440, 0, -salto_x ), seq( salto_x, 440, salto_x ) ) ), '$')

rec_pir_edad_sal_prom <- ggplot( aux, aes( x = edad_mies, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Instruccion' ) +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Hombre' ), stat = 'identity', colour = 'white') +
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


#Pirámide de ingreso de reciclaje según edad y sexo------------------------------------------------
message( '\tGraficando ingreso ingreso de reciclaje según edad y sexo' )
aux <- pir_edad_sal_reciclaje %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'Mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, edad_mies )

salto_x <- 15
brks_y <- seq( -80, 80, salto_x )
lbls_y <- paste0( as.character( c( seq( 80, 0, -salto_x ), seq( salto_x, 80, salto_x ) ) ), '%')

rec_pir_edad_sal_reciclaje <- ggplot( aux, aes( x = edad_mies, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Instruccion' ) +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Hombre' ), stat = 'identity', colour = 'white') +
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


#Pirámide de ingreso total según edad y sexo-------------------------------------------------------

message( '\tGraficando ingreso ingreso total según edad y sexo' )
aux <- pir_edad_sal_total %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'Mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, edad_mies )

salto_x <- 15
brks_y <- seq( -80, 80, salto_x )
lbls_y <- paste0( as.character( c( seq( 80, 0, -salto_x ), seq( salto_x, 80, salto_x ) ) ), '%')

rec_pir_edad_sal_total <- ggplot( aux, aes( x = edad_mies, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Instruccion' ) +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Hombre' ), stat = 'identity', colour = 'white') +
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


#Pirámide de afiliados por sexo--------------------------------------------------------------------

message( '\tGraficando población de afiliados por sexo' )
aux <- pir_afiliados_sexo %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'Mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, caracteristica_social_afiliado )

salto_x <- 10
brks_y <- seq( -100, 100, salto_x )
lbls_y <- paste0( as.character( c( seq( 100, 0, -salto_x ), seq( salto_x, 100, salto_x ) ) ), '%')

rec_pir_afiliados_sexo <- ggplot( aux, aes( x = caracteristica_social_afiliado, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'caracteristica social' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Hombre' ), stat = 'identity', colour = 'white') +
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

#Pirámide de afiliados antiguos por sexo-----------------------------------------------------------

message( '\tGraficando población de afiliados por sexo' )
aux <- pir_afiliados_antiguos_sexo %>%
  mutate( porcentaje = if_else( sexo_reciclador == 'Mujer',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo_reciclador, caracteristica_social_estuvo_afiliado )

salto_x <- 10
brks_y <- seq( -100, 100, salto_x )
lbls_y <- paste0( as.character( c( seq( 100, 0, -salto_x ), seq( salto_x, 100, salto_x ) ) ), '%')

rec_pir_afiliados_antiguos_sexo <- ggplot( aux, aes( x = caracteristica_social_estuvo_afiliado, y = porcentaje, fill = sexo_reciclador ) ) +
  xlab( 'Caracteristica Social' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Mujer' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo_reciclador == 'Hombre' ), stat = 'identity', colour = 'white') +
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
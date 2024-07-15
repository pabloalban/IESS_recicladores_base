message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IVM/', 'IESS_IVM_salidas_demograficos.RData' ) )

# --------------------------------------------------------------------------------------------------
message( '\tTasa de cobertura sobre la fuerza laboral (PEA), ambos sexos' )
aux <- copy( tasa_cob[ , list( year, tcv ) ] )
aux$year <- as.numeric(aux$year)

x_lim <- c( 2021, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 3 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0.3, 0.6 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), '%' )

iess_tcv <- ggplot( data = aux ) +
  geom_line( aes(x = year,
                 y = tcv,
                 color = parametros$iess_green),
             size = graf_line_size ) +
  labs( x = '', y = '' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ),
                      labels = c( 'Activos/Fuerza Laboral Empleada, M+F', '' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ), legend.spacing.y = unit(-1.0, 'cm') )

ggsave( plot = iess_tcv,
        filename = paste0( parametros$resultado_graficos, 'iess_tcv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( '\tEvolución de la carga pensional' )
aux <- copy( tasa_cob )
aux$year <- as.numeric(aux$year)

# Completo datos históricos tomados del BI para afiliados, para pensionistas se
# estima de 2016 a 2012 con tasa de decrecimiento
file <- paste0( parametros$Data, 'IVM/', 'DatosHistoricosDemografia.xlsx' ) # No debería leerse aquí
aux_afi <- read_excel( file, sheet='hisact'
                       ,col_names = FALSE
                       ,guess_max = 24000
                       ,skip = 7 )
setnames( aux_afi, c( 'year', 'Femeninoa', 'Masculinoa' ) )

aux_pen <- read_excel( file, sheet='hispen'
                       ,col_names = FALSE
                       ,guess_max = 24000
                       ,skip = 1 )
setnames( aux_pen, c('year', 'Femeninop', 'Masculinop' ) )
aux_ap <- merge( aux_afi, aux_pen, all.x = TRUE, by = 'year')
aux_ap <- as.data.table( aux_ap )
aux_ap[ , cap := (Femeninoa+Masculinoa)/(Femeninop+Masculinop) ]
aux_ap[ , cap_female := (Femeninoa)/(Femeninop) ]
aux_ap[ , cap_male := (Masculinoa)/(Masculinop) ]
aux_ap <- aux_ap[ , list( year, cap, cap_female, cap_male ) ]
aux_ap$year <- as.numeric(aux_ap$year)
#
aux <- aux[ , list( year, cap, cap_female, cap_male ) ]
aux <- rbind( aux_ap, aux )
aux$year <- as.numeric(aux$year)

x_lim <- c( 2012, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 3 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0, 13 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_carga_pen <- ggplot( data = aux ) + 
  geom_line( aes(x = year,
                 y = cap
                 , color = 'red'
  ),
  size = graf_line_size ) +
  geom_line( aes(x = year,
                 y = cap_male
                 , color = parametros$iess_blue
  ),
  size = graf_line_size ) +
  geom_line( aes(x = year,
                 y = cap_female
                 , color = parametros$iess_green
  ),
  size = graf_line_size ) +
  labs( x = '', y = '' ) +
  scale_color_manual( values =  c( parametros$iess_blue , parametros$iess_green, 'red' ), 
                      labels = c( 'Activos/Pensionistas, M', 'Activos/Pensionistas, F', 'Activos/Pensionistas, F+M' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text(  angle = 90, vjust = 0.5 ), legend.spacing.y = unit(-1.0, 'cm') )

ggsave( plot = iess_carga_pen,
        filename = paste0( parametros$resultado_graficos, 'iess_carga_pen', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( '\tCotizantes activos e inactivos por sexo' )
aux <- copy( act )
aux$Anio <- as.numeric(aux$Anio)

file <- paste0( parametros$Data, 'IVM/', 'DatosHistoricosDemografia.xlsx' ) # No debería leerse aquí
aux_afi <- as.data.table( read_excel( file, sheet = 'hisact'
                                      ,col_names = FALSE
                                      ,guess_max = 24000
                                      ,skip = 7 ) )
setnames( aux_afi, c( 'year', 'Femeninoa', 'Masculinoa' ) )

aux_ina <- as.data.table( read_excel( file, sheet='hisInact'
                                      ,col_names = FALSE
                                      ,guess_max = 24000
                                      ,skip = 1 ) )
setnames( aux_ina, c( 'year','Masculino_ina', 'Femenino_ina' ) )

aux_afi <- aux_afi[ year != '2020' ]
aux_ina <- aux_ina[ year != '2020' ]

afi_aux <- data.table( Anio = aux_afi$year, Valor = aux_afi$Femeninoa
                       , tipo = 'Activ_female' )
afi_aux <- rbind( afi_aux,  data.table( Anio = aux_afi$year, Valor = aux_afi$Masculinoa
                                        , tipo = 'Activ_male' ) )
afi_aux <- rbind( afi_aux, data.table( Anio = aux_afi$year, Valor = aux_afi$Masculinoa + aux_afi$Femeninoa
                                       , tipo = 'Afi_f_m' ) )

afi_aux <- rbind( afi_aux, data.table( Anio = aux_ina$year, Valor = aux_ina$Femenino_ina
                                       , tipo = 'Inact_f' ) )

afi_aux <- rbind( afi_aux, data.table( Anio = aux_ina$year, Valor = aux_ina$Masculino_ina
                                       , tipo = 'Inact_m' ) )

afi_aux <- rbind( afi_aux, data.table( Anio = aux_ina$year, Valor = aux_ina$Masculino_ina + aux_ina$Femenino_ina
                                       , tipo = 'Inac_f_m' ) )

afi_aux <- rbind( afi_aux, data.table( Anio = aux_ina$year, Valor = aux_ina$Femenino_ina + aux_afi$Femeninoa
                                       , tipo = 'Afi_f' ) )

afi_aux <- rbind( afi_aux, data.table( Anio = aux_ina$year, Valor = aux_ina$Masculino_ina + aux_afi$Masculinoa
                                       , tipo = 'Afi_m' ) )

afi_aux <- rbind( afi_aux, data.table( Anio = aux_ina$year, Valor = aux_ina$Masculino_ina + aux_ina$Femenino_ina + aux_afi$Masculinoa + aux_afi$Femeninoa
                                       , tipo = 'Afi_f_m' ) )
afi_aux$Anio <- as.numeric( afi_aux$Anio )
aux <- rbind( afi_aux,  aux )

x_lim <- c( 2012, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0, max( aux[ tipo == 'Afi_f' | tipo == 'Afi_m' ]$Valor ) )
y_lim2 <- c( 0, ceiling(max(aux[Anio==2060]$Valor)/1000000 ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_brk2 <- seq( y_lim2[1], y_lim2[2], length.out = 7 )
y_lbl <- formatC( y_brk2, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_act_inac_f <- ggplot( aux[ tipo == 'Activ_female' | tipo == 'Inact_f' ],
                           aes( x = Anio, y = Valor, fill = tipo ) ) +
  geom_bar( position = 'dodge', stat = 'identity' ) +
  xlab( '' ) +
  ylab( '' ) +
  geom_line( data = aux[ tipo == 'Afi_f' ], aes( x = Anio,
                                                 y = Valor, 
                                                 group = 1, 
                                                 linetype = 'Afiliados, F' ), 
             inherit.aes = FALSE ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue )
                     , labels = c( 'Activos, F', 'Inactivos, F' ) ) +
  theme( axis.text.x = element_text(  angle = 90, vjust = 0.5 ), legend.spacing.y = unit(-1.0, 'cm') )

ggsave( plot = iess_act_inac_f,
        filename = paste0( parametros$resultado_graficos, 'iess_act_inac_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
iess_act_inac_m <- ggplot( aux[ tipo == 'Activ_male' | tipo == 'Inact_m' ]
                           , aes( x = Anio, y = Valor, fill = tipo ) ) +
  geom_bar( position = 'dodge', stat = 'identity' ) +
  xlab( '' ) +
  ylab( '' ) +
  geom_line( data = aux[ tipo == 'Afi_m' ], aes( x = Anio,
                                                 y = Valor, 
                                                 group = 1, 
                                                 linetype = 'Afiliados, m' ),
             inherit.aes = FALSE ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue )
                     , labels = c( 'Activos, M', 'Inactivos, M' ) ) +
  theme( axis.text.x = element_text(  angle = 90, vjust = 0.5 ), legend.spacing.y = unit(-1.0, 'cm') )

ggsave( plot = iess_act_inac_m,
        filename = paste0( parametros$resultado_graficos, 'iess_act_inac_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( '\tProyectado cotizantes activos e inactivos' )
iess_afi_m_f <- ggplot( aux[ tipo == 'Acti_f_m' | tipo == 'Inac_f_m' ]
                        , aes( x = Anio, y = Valor, fill = tipo ) ) +
  geom_bar( position = 'dodge', stat = 'identity' ) +
  xlab( '' ) +
  ylab( '' ) +
  geom_line( data = aux[ tipo == 'Afi_f_m' ], aes( x = Anio,
                                                   y = Valor, 
                                                   group = 1, 
                                                   linetype = 'Afilaidos, m+f' ), 
             inherit.aes = FALSE ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue )
                     , labels = c( 'Activos, M+F', 'Inactivos, M+F' ) ) +
  theme( axis.text.x = element_text(  angle = 90, hjust = 1 ) )

ggsave( plot = iess_afi_m_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_afi_m_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( '\tProyectado Fuerza de trabajo, cotizantes activos e inactivos' )
aux <- copy( tasa_cob[ , list( year, tcv, activos_f_m, fuerza_la_f_m ) ] )
aux$year <- as.numeric(aux$year)

file <- paste0( parametros$Data, 'IVM/', 'DatosHistoricosDemografia.xlsx' ) # No debería leerse aquí
flt <- read_excel( file, sheet = 'hisFL'
                   ,col_names = FALSE
                   ,guess_max = 24000
                   ,skip = 1 )
setnames( flt, c('year', 'FL' ) )
flt <- as.data.table( flt )

aux_afi <- read_excel( file, sheet='hisact'
                       ,col_names = FALSE
                       ,guess_max = 24000
                       ,skip = 7 )
setnames( aux_afi, c('year', 'Femeninoa', 'Masculinoa' ) )
aux_afi <- as.data.table( aux_afi )

flt[ , tcv := ( aux_afi$Femeninoa + aux_afi$Masculinoa ) / FL ]
flt[ , activos_f_m :=  aux_afi$Femeninoa + aux_afi$Masculinoa ]
flt[ , fuerza_la_f_m := FL ]
flt <-  flt[ , list( year, tcv, activos_f_m, fuerza_la_f_m ) ]
aux <- rbind( flt, aux )

aux2 <- aux[, list( Anio = year, Valor = activos_f_m, tipo = 'activos_f_m' ) ]
aux3 <- aux[, list( Anio = year, Valor = fuerza_la_f_m, tipo = 'fuerza_la_f_m' ) ]
aux4 <- aux[, list( Anio = year, Valor = tcv, tipo = 'tcv' ) ]
aux <- rbind( aux2, aux3, aux4 )

scl = 1e7 # escala de miles de millones
hmts = 2 # homotecia

x_lim <- c( 2012, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0, 14e6 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 14 )
y_lim2 <- c( 0, ceiling(max(aux[Anio==2060]$Valor)/1000000 ) )
y_brk2 <- seq( y_lim2[1], y_lim2[2], length.out = 14 )
y_lbl <- formatC( y_brk2, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_fuerzat_m_f <- ggplot( aux[ tipo == 'activos_f_m' | tipo == 'fuerza_la_f_m' ]
                            , aes( x = Anio, y = Valor, fill = tipo ) ) +
  geom_bar( position = 'dodge', stat = 'identity' ) +
  xlab( '' ) +
  ylab( '' ) +
  geom_line( data = aux[ tipo == 'tcv' ]
             , aes( x = Anio
                    , y = hmts * scl * Valor
                    , group = 1
                    , linetype = 'Activos/Fuerza Laboral, M+F' )
             
             , inherit.aes = FALSE
             , size = 1
  ) +
  scale_linetype_manual( NULL, values = 1 ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim
                      , sec.axis = sec_axis( ~./( hmts * scl )
                                             , name = ''
                                             , labels = function( b ) { paste0( b * 100, '%' ) } ) ) +
  plt_theme_legend +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue )
                     , labels = c( 'Activos, M+F', 'Fuerza Laboral, M+F' ) ) +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ), 
         legend.spacing.y = unit(-1.0, 'cm') )

ggsave( plot = iess_fuerzat_m_f,
        filename = paste0( parametros$resultado_graficos, 'iess_fuerzat_m_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( '\tProyección: evolución del número de pensionistas de vejez' )
aux <- copy( tasa_cob )
aux$year <- as.numeric(aux$year)

aux <- aux[ , list( year, jv_female, jv_male, jv_f_m ) ]

file <- paste0( parametros$Data, 'IVM/OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/'
                , 'base_hoap__female_50_.csv' ) # No debería leerse aquí

aux_f <- fread( file, skip = 1 )
aux_f <- as.data.table( aux_f )
aux_f <- aux_f[, 2:3 ]

file <- paste0( parametros$Data, 'IVM/OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/'
                , 'base_hoap__male_50_.csv' ) # No debería leerse aquí
aux_m <- fread( file, skip = 1 )
aux_m <- as.data.table( aux_m )
aux_m <- aux_m[, 2:3 ]

auxt <- cbind( aux_f, aux_m[ , 2:2 ] )
auxt[ , jv_f_m := 0 ]
setnames(auxt, colnames(aux) )
auxt[ , jv_f_m := jv_female + jv_male ]
auxt <- auxt[ year > 2011 ]

aux <- rbind( auxt, aux )

x_lim <- c( 2012, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0, 3e3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen_jv <- ggplot( data = aux ) + 
  geom_area( aes(x = year,
                 y = jv_f_m/1000
                 , linetype = 'Vejez, m+f'
  )
  , inherit.aes = FALSE
  , fill = '#69b3a2'
  , alpha=0.8
  ) +
  geom_line( aes(x = year,
                 y = jv_female/1000
                 , color = parametros$iess_green
  )
  , size = graf_line_size ) +
  geom_line( aes(x = year,
                 y = jv_male/1000
                 , color = parametros$iess_blue
  )
  , size = graf_line_size ) +
  labs( x = '', y = '' ) +
  scale_color_manual( values =  c( parametros$iess_blue , parametros$iess_green ), 
                      labels = c( 'Vejez, M', 'Vejez, F' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text(  angle = 90, vjust = 0.5 ),
         legend.spacing.y = unit(-1.0, 'cm' ) )

ggsave( plot = iess_pen_jv,
        filename = paste0( parametros$resultado_graficos, 'iess_pen_jv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico pensionistas de invalidez ----------------------------------------------------------------
message( '\tGráfico pensionistas de invalidez' )
dis <- copy( pen_dis )

x_lim <- c( 2012, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 120000 ) / 1e3
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux <- rbind( dis[, c('anio', 'total') ], data.frame( anio = rev( dis$anio), total = rep(0,51 ) ) )

iess_pob_dis <- ggplot( data = dis ) + 
  geom_line( aes( x = anio,
                  y = hombres/1e3,
                  color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( aes( x = anio,
                  y = mujeres/1e3,
                  color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_polygon( data = aux, aes( x = anio, y = total/1e3, fill = '#69b3a2'), alpha = 0.5  ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Invalidez, M', 'Invalidez, F') ) +
  scale_fill_manual( values = c('#69b3a2'),
                     labels = c( 'Invalidez, M+F' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) ) +
  theme(legend.position = 'bottom', legend.spacing.y = unit(-0.5, 'cm' ) )   +
  guides( fill = guide_legend( title = NULL ) )+
  guides( color = guide_legend( title = NULL ) )

ggsave( plot = iess_pob_dis, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_dis', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico pensionistas de viudez -------------------------------------------------------------------
message( '\tGráfico pensionistas de viudez' )
viu <- copy(pen_viu)

x_lim <- c( 2012, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 200000)/1e3
y_brk <- seq( y_lim[1], y_lim[2], 25 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux <- rbind( viu[ , c('anio', 'total') ], data.frame( anio = rev( viu$anio),
                                                       total = rep(0, length(rev( viu$anio ) ) ) ) )

iess_pob_viu <- ggplot( data = viu ) + 
  geom_line( aes( x = anio,
                  y = hombres/1e3,
                  color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( aes( x = anio,
                  y = mujeres/1e3,
                  color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_polygon( data = aux, aes( x = anio, y = total/1e3, fill = '#69b3a2'), alpha = 0.5  ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Viudez, M', 'Viudez, F' ) ) +
  scale_fill_manual(   values = c( '#69b3a2' ),
                       labels = c( 'Viudez, M+F' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) ) +
  theme( legend.position = 'bottom', legend.spacing.y = unit(-0.5, 'cm' ) )   +
  guides( fill = guide_legend( title = NULL ) )+
  guides( color = guide_legend( title = NULL ) )

ggsave( plot = iess_pob_viu, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_viu', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico pensionistas de orfandad -----------------------------------------------------------------
message( '\tGráfico pensionistas de orfandad' )
orf <- copy(pen_orf)

x_lim <- c( 2012, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50000)/1e3
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux <- rbind( orf[ , c('anio', 'total') ], data.frame( anio = rev( orf$anio),
                                                       total = rep(0, length(rev( orf$anio ) ) ) ))

iess_pob_orf <- ggplot( data = orf ) + 
  geom_line( aes( x = anio,
                  y = hombres/1e3,
                  color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( aes( x = anio,
                  y = mujeres/1e3,
                  color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_polygon( data = aux, aes( x = anio, y = total/1e3, fill = '#69b3a2'), alpha = 0.5  ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Orfandad, M', 'Orfandad, F') ) +
  scale_fill_manual(   values = c( '#69b3a2' ),
                       labels = c( 'Orfandad, M+F' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ), legend.spacing.y = unit(-0.5, 'cm') ) +
  theme( legend.position = 'bottom' )   +
  guides( fill = guide_legend( title = NULL ) )+
  guides( color = guide_legend( title = NULL ) )

ggsave( plot = iess_pob_orf, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_orf', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico nuevos pensionistas por tipo -------------------------------------------------------------
message( '\tGráfico nuevos pensionistas por tipo' )
his <- copy( his_pen)

# Vejez --------------------------------------------------------------------------------------------
message( '\tVejez' )
proy_vej <- copy( proy_vej )

# Invalidez-Discapacidad ---------------------------------------------------------------------------
proy_inv <- copy( proy_inv)

# Viudedad -----------------------------------------------------------------------------------------
message( '\tViudedad' )
proy_viu <- copy( proy_viu )

# Orfandad-----------------------------------------------------------------------------------------
message( '\tOrfandad' )
proy_orf <- copy( proy_orf )

pensi <- rbind( his, proy_vej, proy_inv, proy_viu, proy_orf )
pensi[ , tipo := toupper(tipo) ]
pensi[ , valor := as.numeric(valor) ]
setorder( pensi, anio, sexo, tipo )
pensi_total_tipo <- pensi[ , list(total= sum(valor, na.rm=T ) ), by = list(anio,tipo) ]
pensi_total <- pensi[ , list( total= sum(valor, na.rm=T ) ), by = list(anio) ]

x_lim <- seq( 2012, 2060, 1 )
x_brk <- seq( 2012, 2060, 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 250000 )/1e3
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_nuevos_pensi <- ggplot() +
  geom_bar( pensi_total_tipo, mapping = aes(x = anio, y = total/1e3, fill= tipo ), 
            position = 'dodge', stat = 'identity', size = 0.1) +
  geom_point( data = pensi_total, mapping = aes( x = anio, y = total/1e3, color = parametros$iess_blue ),
              size=3, shape=95) +
  labs( x = NULL, y = NULL ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_discrete( breaks = x_brk, labels = x_lbl) +
  scale_fill_manual( values = c( 'INVALIDEZ' = 'black',
                                 'ORFANDAD' = 'purple',
                                 'VEJEZ' = parametros$iess_green,
                                 'VIUDEDAD'= 'red'),
                     label = c('Invalidez, M+F', 'Orfandad, M+F', 'Vejez, M+F', 'Viudez, M+F' ) )+
  scale_color_manual( values = c( parametros$iess_blue), 
                      label = c('Nuevos, M+F' ) ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) +
  theme( legend.key.size = unit( 0.5, 'cm' ) ) +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0, 
                               label.vjust = 0.5 ) )+
  guides( shape = guide_legend( title = NULL ) )     

ggsave( plot = iess_nuevos_pensi, 
        filename = paste0( parametros$resultado_graficos, 'iess_nuevos_pensi', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico tasa de cobertura ------------------------------------------------------------------------
message( '\tGráfico tasa de cobertura' )

proy_cob <- copy( proy_cob )
proy_cob[, valor:=as.numeric( valor) ]
prom_cob <- proy_cob[ , list( prom =  mean(valor, na.rm=T ) ), by = list(anio) ]

x_lim <- c( 2021, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 100)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- paste0( formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%' )

iess_pob_cob <- ggplot(  ) + 
  geom_line( data = proy_cob[sexo=='H'],
             aes( x = anio,
                  y = valor  * 100,
                  color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( data = proy_cob[sexo=='M'],
             aes( x = anio,
                  y = valor * 100,
                  color =  parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( data = prom_cob,
             aes( x = anio,
                  y = prom * 100,
                  color = 'red' ),
             size = graf_line_size,
             lineend = 'round' ) +
  xlab( NULL ) +
  ylab( NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green, 'red' ),
                      labels = c( 'Pensionistas/Población, 60+, M', 'Pensionistas/Población, 60+, F',
                                  'Pensionistas/Población, 60+, M+F' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim,sec.axis = dup_axis() ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) +
  theme(legend.text = element_text( size = 7 ) ) +
  guides( color = guide_legend( title = NULL ) )

ggsave( plot = iess_pob_cob, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_cob', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico tasa de reparto --------------------------------------------------------------------------
message( '\tGráfico tasa de reparto' )

# Gráfico pensionistas de invalidez ----------------------------------------------------------------
message( '\tGráfico entradas el sistema' )
entradas <- copy( entradas_sis )
entradas[ , h_por:=as.numeric(h_por)  * 100 ]
entradas[ , m_por:=as.numeric(m_por) * 100 ]
entradas[ , edad:=as.numeric(edad) ]
entradas <- entradas[ !is.na(edad) ]

x_lim <- c( 15, 70 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 6 )
y_brk <- seq( y_lim[1], y_lim[2], 1 )
y_lbl <- paste0( formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), '%' )

iess_entr_sis <- ggplot(  ) + 
  geom_line( data = entradas, aes( x = edad,
                                   y =  h_por ,
                                   color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( data = entradas, aes( x = edad,
                                   y = m_por ,
                                   color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Masculino', 'Femenino') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +  
  theme( axis.text.x = element_text( angle = 0, vjust = 0.5 ),
         legend.title = element_blank() ) 

ggsave( plot = iess_entr_sis, 
        filename = paste0( parametros$resultado_graficos, 'iess_entr_sis', parametros$graf_ext ),
        width = graf_width + 4, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico salarios ---------------------------------------------------------------------------------
message( '\tGráfico salarios ' )
sal <- copy( salarios )

x_lim <- c( 15, 70 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1000)
y_brk <- seq( y_lim[1], y_lim[2], 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_salarios <- ggplot(  ) + 
  geom_line( data = sal, aes( x = edad_h,
                              y =  male ,
                              color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( data = sal, aes( x = edad_m,
                              y = female ,
                              color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  labs( x = NULL, y = 'USD' ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Masculino', 'Femenino') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  theme( axis.text.x = element_text( angle = 0, vjust = 0.5 ),
         legend.title = element_blank() )

ggsave( plot = iess_salarios, 
        filename = paste0( parametros$resultado_graficos, 'iess_salarios', parametros$graf_ext ),
        width = graf_width + 5, height = graf_height, units = graf_units, dpi = graf_dpi )

# # Gráfico proporción de afiliados activos e inactivos 3. 1 ---------------------------------------
message( '\tGráfico proporcion de afiliados activos e inactivos ' ) 
aux1 <- copy( prop_afi_act_inac)
colnames(aux1) = c('Des','Descripcion','Total', 'Por')
aux1<-aux1[,-c(1,3) ]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2= 1.05 * rpie
iess_act_inact <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = Descripcion),
               color='white') +
  geom_text( aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle), 
                 label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%' ) ),
             hjust = 0.5, vjust = 0.5, family = tipo_letra ) +
  geom_text( aes(x = rlabel2*sin(mid_angle), y = rlabel2*cos(mid_angle), 
                 label = Descripcion,
                 hjust = hjust, vjust = vjust), size = 4, family = tipo_letra ) +
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1.5, 1.5), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1), name = '', breaks = NULL, labels = NULL) +
  theme_void() + theme(legend.position='none')

ggsave( plot = iess_act_inact,
        filename = paste0( parametros$resultado_graficos, 'iess_act_inact', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Distribución de cotizantes activos por edad, sexo masculino --------------------------------------
message( '\tGráfico barras cotizantes activos por edad, sexo masculino' )
cotizantes <- copy( cotizantes )

x_lim <- c(-0.5, 44)
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <-formatC( seq( x_lim[1]+.5, x_lim[2], 5 ), digits = 0, format = 'f')

y_lim<- c(0,max(cotizantes$num_cot)/1000)
y_brk<- seq( x_lim[1], x_lim[2], 50 )
y_lbl<-formatC(y_brk, digits = 0, format = 'f')

iess_cotizantes_activ_masculi_1 <- ggplot(cotizantes, aes(x=cot, y=num_cot/1000, fill = ' ' ) ) +
  xlab( 'Años de cotizaciones' ) +
  ylab( 'Número de cotizantes (miles)' ) +
  geom_bar( data = cotizantes, stat = 'identity', colour = 'white', size = 0.1) +
  theme_tufte()+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue)+
  theme( text = element_text(  color = 'black' ),  
         legend.text = element_text(  size = rel( 0.6), colour = 'black', face = 'plain', family = tipo_letra  ) )

ggsave( plot = iess_cotizantes_activ_masculi_1,
        filename = paste0( parametros$resultado_graficos, 'iess_cotizantes_activ_masculi_1', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Unificado de cotizantes vs anios aportados -------------------------------------------------------
cotizantes_h <- copy( cotizantes )
cotizantes_m <- copy( activ_cotiz_fem )

cotizantes_h <- cotizantes_h[ , list( edad, cot, num_cot = num_cot/1000, tipo = 'Hombres' ) ]
cotizantes_m <- cotizantes_m[ , list( edad, cot, num_cot = num_cot/1000, tipo = 'Mujeres' ) ]
cotizantes_t <- rbind( cotizantes_h, cotizantes_m )

iess_cotizantes_activ_unificado <- ggplot( cotizantes_t, aes( x = cot, y = num_cot, fill = tipo) ) +
  xlab( 'Años de cotizaciones' ) +
  ylab( 'Número de cotizantes  (miles) ' ) +
  geom_bar( data = cotizantes_t, stat = 'identity', colour = 'white', size = 0.1,
            position = position_dodge() ) +
  theme_tufte()+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  scale_fill_manual(  values = c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Hombres', 'Mujeres') )+
  theme( text = element_text(  color = 'black' ),  
         legend.text = element_text(  size = rel( 0.6), colour = 'black', 
                                      face = 'plain', family = tipo_letra ),
         legend.position = 'bottom' ,
         legend.title = element_blank( ) )

ggsave( plot = iess_cotizantes_activ_unificado,
        filename = paste0( parametros$resultado_graficos, 'iess_cotizantes_activ_unificado',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# BARRAS HORIZONTALES ------------------------------------------------------------------------------
message( '\tGráfico barras horizontales cotizantes activos por edad, sexo masculino' )

unidad<-1e3
cotizantes[, num := num/ unidad ]

y_lim <- c( 15, 70.5 )
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_cotizantes_mascul_2 <- ggplot(cotizantes, aes(x=num, y=edad, fill = ' ' ) ) +
  xlab( 'Afiliados (miles)' ) +
  ylab( 'Edad' ) +
  geom_bar( data = cotizantes, stat = 'identity', colour='white',size=0.01) +
  theme_tufte()+
  scale_y_discrete(breaks = y_brk, labels = y_lbl) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue)+
  theme( text = element_text(  color = 'black' ),  
         legend.text = element_text(  size = rel( 0.6), colour = 'black', face = 'plain', 
                                      family = tipo_letra  ) )

ggsave( plot = iess_cotizantes_mascul_2 ,
        filename = paste0( parametros$resultado_graficos, 'iess_cotizantes_mascul_2',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# PASTEL 3.2 M -------------------------------------------------------------------------------------
message( '\tGráfico pastel activos por edad, sexo masculino' )
aux1 <- copy( pastel_afi_cotiz_riesgo_mascul[1:4,9:11])
aux1 <- aux1[ , -2 ]
aux1[ , Por := porcentaje ]
aux1[ , porcentaje := NULL ]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2= 1.05 * rpie
aux1 <- data.table( aux1)
iess_pastel_afi_cotiz_riesgo_mascul <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = riesgos),
               colour='white') +
  geom_label(  data = aux1[riesgos=='Cumple requisitos para vejez en 5 años'],
               aes(x = rlabel*sin(mid_angle)-0.05, y = rlabel*cos(mid_angle)+0.2,
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
               ), family = tipo_letra, size=3, 
               fill='white', color = 'black') +
  geom_label(  data = aux1[riesgos=='Cumple requisitos para vejez en 1 año o menos'],
               aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
               ), family = tipo_letra, size=3, 
               fill='white', color = 'black') +
  geom_label( data = aux1[riesgos%nin%c('Cumple requisitos para vejez en 1 año o menos', 
                                        'Cumple requisitos para vejez en 5 años') ],
              aes( x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
                   hjust = hjust, vjust = vjust), family = tipo_letra, size=3,
              fill='white', color = 'black') +
  
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'vertical',
         legend.key.size = unit(0.3,'cm'),
         text = element_text( size = 9 ),
         legend.spacing.y = unit(-0.9,'cm' ) )

ggsave( plot = iess_pastel_afi_cotiz_riesgo_mascul ,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afi_cotiz_riesgo_mascul',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# BURBUJAS -----------------------------------------------------------------------------------------
message( '\tGráfico burbujas cotizantes activos por edad, sexo masculino' )

aux1 <- copy(afi_cotiz_riesgo_mascul[,2:7])
aux1 <- aux1[,-3]
aux1 <- aux1[,-4]
df2 <- aux1
df2 <- df2[ edad<=70 & impos_años <=45] # Corrección para OIT

x_lim <- c(0,45)
x_brk <- seq( x_lim[1], x_lim[2], 5)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c(-70,-15)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_afi_cotiz_riesgo_mascul <- ggplot(df2, aes(x =impos_años , y = -edad) ) +
  geom_point(aes(size = n_afiliados, colour = riesgo3),alpha=0.5) +
  scale_size(range = c(1,15 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[1],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[2],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[3],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[4] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Número de cotizaciones', y = 'Edad' ) +
  guides(color = guide_legend( override.aes = list(size = 5) ) ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'vertical', text = element_text( size = 9 ) ) +
  theme( axis.text.x = element_text( angle = 0, vjust =1) ) 

ggsave( plot = iess_afi_cotiz_riesgo_mascul,
        filename = paste0( parametros$resultado_graficos, 'iess_afi_cotiz_riesgo_mascul', 
                           parametros$graf_ext ),
        width = 15, height = 18, units = graf_units, dpi = graf_dpi )

# Distribución de cotizantes activos por edad, sexo femenino ---------------------------------------
message( '\tGráfico barras cotizantes activos por edad, sexo femenino' )
activ_cotiz_fem <- copy( activ_cotiz_fem )

x_lim <- c( -0.5,44)
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( seq( x_lim[1]+.5, x_lim[2], 5 ), digits = 0, format = 'f')

y_lim <- c(0,max(activ_cotiz_fem$num_cot )/1000)
y_brk <- seq( x_lim[1], x_lim[2], 5 )
y_lbl<-formatC( y_brk, digits = 0, format = 'f')

iess_activ_cotiz_fem_1 <- ggplot(activ_cotiz_fem, aes(x=cot, y=num_cot/1000, fill = ' ' ) ) +
  xlab( 'Años de cotizaciones' ) +
  ylab( 'Número de cotizantes  (miles) ' ) +
  geom_bar( data = activ_cotiz_fem, stat = 'identity', colour = 'white', size = 0.1) +
  theme_tufte()+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_green)+
  theme( text = element_text(  color = 'black' ),  
         legend.text = element_text(  size = rel( 0.6), colour = 'black', face = 'plain', 
                                      family = tipo_letra  ) )

ggsave( plot = iess_activ_cotiz_fem_1 ,
        filename = paste0( parametros$resultado_graficos, 'iess_activ_cotiz_fem_1',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# BARRAS HORIZONTALES ------------------------------------------------------------------------------
message( '\tGráfico barras horizontales cotizantes activos por edad, sexo femenino' )
unidad <- 1e3
activ_cotiz_fem[, num := num/ unidad ]

y_lim <- c( 15, 70.5 )
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_activ_cotiz_fem_2 <- ggplot( activ_cotiz_fem, aes( x = num, y = edad, fill = ' ' ) ) +
  xlab( 'Afiliados (miles)' ) +
  ylab( 'Edad' ) +
  geom_bar( data = activ_cotiz_fem, stat = 'identity', colour = 'white', size = 0.01 ) +
  theme_tufte()+
  scale_y_discrete(breaks = y_brk, labels = y_lbl) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_green)+
  theme( text = element_text(  color = 'black' ),  
         legend.text = element_text(  size = rel( 0.8), colour = 'black', face = 'plain',
                                      family = tipo_letra  ) )

ggsave( plot = iess_activ_cotiz_fem_2 ,
        filename = paste0( parametros$resultado_graficos, 'iess_activ_cotiz_fem_2',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Unificado de activos por edad, sexo y número -----------------------------------------------------
cotizantes_h <- copy( cotizantes )
cotizantes_m <- copy( activ_cotiz_fem )
unidad<-1e3

cotizantes_h <- cotizantes_h[ , list( edad, num = - num, num_cot = num_cot/1000, tipo = 'Hombres' ) ]
cotizantes_m <- cotizantes_m[ , list( edad, num = num, num_cot = num_cot/1000, tipo = 'Mujeres' ) ]
cotizantes_t <- rbind( cotizantes_h, cotizantes_m )
cotizantes_t <- cotizantes_t[ !is.na( edad) ]

y_lim <- c( 15, 70.5 )
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

salto_x <- 10

brks_y <- seq( -70, 50, salto_x )
lbls_y <- paste0(abs(brks_y ) )

iess_activ_cotiz_sexo_unificado <- ggplot( cotizantes_t, aes( x = num, y = edad, fill = tipo) ) +
  xlab( 'Afiliados (miles)' ) +
  ylab( 'Edad' ) +
  geom_bar( data = cotizantes_t[ tipo == 'Mujeres' ], stat = 'identity', colour = 'white', size = 0.1 ) +
  geom_bar( data = cotizantes_t[ tipo == 'Hombres' ], stat = 'identity', colour = 'white', size = 0.1 ) +
  theme_tufte()+
  scale_y_discrete(breaks = y_brk, labels = y_lbl) +
  scale_x_continuous( breaks = brks_y, labels = lbls_y) +
  scale_fill_manual(  values = c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Hombres', 'Mujeres') )+
  theme(text = element_text(  color = 'black' ),  
        legend.text = element_text(  size = rel( 0.6), colour = 'black', 
                                     face = 'plain', family = tipo_letra ),
        legend.position = 'bottom' ,
        legend.title = element_blank( ) )

ggsave( plot = iess_activ_cotiz_sexo_unificado,
        filename = paste0( parametros$resultado_graficos, 'iess_activ_cotiz_sexo_unificado', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# PASTEL -------------------------------------------------------------------------------------------
message( '\tGráfico pastel cotizantes activos por edad, sexo femenino' )
aux1 <- copy( pastel_distri_activ_cotiz_riesgo_femenin[2:5,10:12])
colnames(aux1) = c('riesgos', 'nada', 'porcentaje')
aux1<-aux1[,-2]
aux1[ , Por:=porcentaje]
aux1[ , porcentaje:=NULL]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2 = 1.05 * rpie
aux1 <- data.table( aux1)
iess_pastel_afi_cotiz_riesgo_femen <- ggplot(aux1) + 
  geom_arc_bar( aes( x0 = 0, y0 = 0, r0 = 0, r = rpie,
                     start = start_angle, end = end_angle, fill = riesgos ),
                colour='white' ) +
  geom_label(  data = aux1[riesgos=='Cumple requisitos para vejez en 5 años'],
               aes(x = rlabel*sin(mid_angle)-0.05, y = rlabel*cos(mid_angle)+0.2,
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
               ), family = tipo_letra, size=3, 
               fill='white', color = 'black') +
  geom_label(  data = aux1[riesgos=='Cumple requisitos para vejez en 1 año o menos'],
               aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
               ), family = tipo_letra, size=3, 
               fill='white', color = 'black') +
  geom_label( data = aux1[riesgos%nin%c('Cumple requisitos para vejez en 1 año o menos', 
                                        'Cumple requisitos para vejez en 5 años') ],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
                  hjust = hjust, vjust = vjust), family = tipo_letra, size=3,
              fill='white', color = 'black') +
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL) +
  plt_theme_legend +
  theme(legend.position = 'bottom', legend.direction = 'vertical',
        legend.key.size = unit(0.3,'cm'),
        text = element_text( size=9 ),
        legend.spacing.y = unit(-0.9,'cm' ) )

ggsave( plot = iess_pastel_afi_cotiz_riesgo_femen ,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afi_cotiz_riesgo_femen', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# BURBUJAS -----------------------------------------------------------------------------------------
message( '\tGráfico burbujas cotizantes activos por edad, sexo femenino' )
aux1 <- copy( distri_activ_cotiz_riesgo_femenin[,2:7] )
aux1 <- aux1[,-3]
aux1 <- aux1[,-4]
df2 <- aux1
df2 <- df2[ edad<=70 & impos_años <=45] #Corrección para OIT

x_lim <- c(0,45)
x_brk <- seq( x_lim[1], x_lim[2], 5)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c(-70,-15)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_afi_cotiz_riesgo_femenin <- ggplot(df2, aes(x =impos_años , y = -edad) ) +
  geom_point(aes(size = n_afiliados, colour = riesgo3),alpha=0.5) +
  scale_size(range = c(1,15 ) ) +
  scale_color_manual(values=c( 'Requisitos no cumplidos' = esc_cl[1],
                               'Requisitos cumplidos para invalidez y muerte' = esc_cl[2],
                               'Cumple requisitos para vejez en 5 años' = esc_cl[3],
                               'Cumple requisitos para vejez en 1 año o menos' = esc_cl[4])
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Número de cotizaciones', y = 'Edad' ) +
  guides(color = guide_legend( override.aes = list(size = 5) ) ) +
  guides( size = F) +
  plt_theme_legend +
  theme(legend.position = 'bottom', legend.direction = 'vertical', text = element_text( size=9 ) ) +
  theme( axis.text.x = element_text(  angle = 0 , vjust =1) )

ggsave( plot = iess_afi_cotiz_riesgo_femenin,
        filename = paste0( parametros$resultado_graficos, 'iess_afi_cotiz_riesgo_femenin',
                           parametros$graf_ext ),
        width = 15, height = 18, units = graf_units, dpi = graf_dpi )

# # Distribución de cotizantes inactivos por edad, sexo masculino ----------------------------------
message( '\tGráfico barras cotizantes inactivos por edad, sexo masculino' )

inac_cotiz_masc <- copy( inac_cotiz_masc )

x_lim <- c(-0.5,20)
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC(seq( x_lim[1]+0.5, x_lim[2], 2 ), digits = 0, format = 'f')

y_lim <- c( -0.5,max(inac_cotiz_masc$num_cot)/1000)
y_brk <- seq( y_lim[1], y_lim[2], 100)
y_lbl <- formatC(y_brk, digits = 0, format = 'f')

iess_inac_cotiz_masc_1 <- ggplot(inac_cotiz_masc, aes(x=cot, y=num_cot/1000, fill = ' ' ) ) +
  xlab( 'Años de cotizaciones' ) +
  ylab( 'Número de cotizantes  (miles)' ) +
  geom_bar( data = inac_cotiz_masc, stat = 'identity', colour = 'white', size = 0.1) +
  theme_tufte()+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue)+
  theme(text = element_text(  color = 'black' ),  
        legend.text = element_text(  size = rel( 0.8), colour = 'black', face = 'plain', family = tipo_letra  ) )

ggsave( plot = iess_inac_cotiz_masc_1,
        filename = paste0( parametros$resultado_graficos, 'iess_inac_cotiz_masc_1', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# BARRAS HORIZONTALES ------------------------------------------------------------------------------
message( '\tGráfico barras horizontales cotizantes inactivos por edad, sexo masculino' )

unidad<-1e3
inac_cotiz_masc[, num:= num / unidad ]

y_lim <- c( 15, 80 )
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_inac_cotiz_masc_2 <-ggplot(inac_cotiz_masc, aes(x=num, y=edad, fill= ' '  ) ) +
  xlab( 'Afiliados (miles)' ) +
  ylab( 'Edad' ) +
  geom_bar( data = inac_cotiz_masc, stat = 'identity', colour='white',size=0.01) +
  theme_tufte()+
  scale_y_discrete(breaks = y_brk, labels = y_lbl) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue)+
  theme(text = element_text(  color = 'black' ),  
        legend.text = element_text(  size = rel( 0.8), colour = 'black', face = 'plain', 
                                     family = tipo_letra  ) )

ggsave( plot = iess_inac_cotiz_masc_2,
        filename = paste0( parametros$resultado_graficos, 'iess_inac_cotiz_masc_2', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# PASTEL -------------------------------------------------------------------------------------------
message( '\tGráfico pastel cotizantes inactivos por edad, sexo masculino' )

aux1<-copy( pastel_distri_inact_cotiz_riesgo_mascul[2:5,10:12])
colnames(aux1) = c('riesgos', 'nada', 'porcentaje')
aux1<-aux1[,-2]

aux1[ , Por:=porcentaje]
aux1[ , porcentaje:=NULL]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2= 1.05 * rpie
aux1 <- data.table( aux1)
iess_pastel_afi_inac_cotiz_riesgo_mascul <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = riesgos),
               colour='white') +
  geom_label(  data = aux1[riesgos=='Cumple requisitos para vejez en 5 años'],
               aes(x = rlabel*sin(mid_angle)-0.05, y = rlabel*cos(mid_angle)+0.2,
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
               ), family = tipo_letra, size=3, 
               fill='white', color = 'black') +
  geom_label(  data = aux1[riesgos=='Cumple requisitos para vejez en 1 año o menos'],
               aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
               ), family = tipo_letra, size=3, 
               fill='white', color = 'black') +
  geom_label( data = aux1[ riesgos%nin%c('Cumple requisitos para vejez en 1 año o menos',
                                         'Cumple requisitos para vejez en 5 años') ],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
                  hjust = hjust, vjust = vjust), family = tipo_letra, size=3,
              fill='white', color = 'black') +
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL) +
  plt_theme_legend +
  theme(legend.position = 'bottom', legend.direction = 'vertical',
        legend.key.size = unit(0.3,'cm'),
        text = element_text( size=9 ),
        legend.spacing.y = unit(-0.9,'cm' ) )

ggsave( plot = iess_pastel_afi_inac_cotiz_riesgo_mascul ,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afi_inac_cotiz_riesgo_mascul', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# BURBUJAS -----------------------------------------------------------------------------------------
message( '\tGráfico burbujas cotizantes inactivos por edad, sexo masculino' )

aux1<-copy(distri_inact_cotiz_riesgo_mascul[,2:7])
aux1 <- aux1[,-3]
aux1 <- aux1[,-4]
df2 <- aux1
df2 <- df2[ edad<=70 & impos_años <=45] #Corrección para OIT

#x_lim <- c(0,70)
x_lim <- c(0,45)
x_brk <- seq( x_lim[1], x_lim[2], 5)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

#y_lim <- c( -125, -15 )
y_lim <- c(-70,-15)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_afi_cotiz_inac_riesgo_mascul <- ggplot(df2, aes(x =impos_años , y = -edad) ) +
  geom_point(aes(size = n_afiliados, colour = riesgo3),alpha=0.5) +
  scale_size(range = c(1,15 ) ) +
  scale_color_manual(values=c( 'Requisitos no cumplidos' = esc_cl[1],
                               'Requisitos cumplidos para invalidez y muerte' = esc_cl[2],
                               'Cumple requisitos para vejez en 5 años' = esc_cl[3],
                               'Cumple requisitos para vejez en 1 año o menos' = esc_cl[4])
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Número de cotizaciones', y = 'Edad' ) +
  guides(color = guide_legend( override.aes = list(size = 5) ) ) +
  guides( size = F ) +
  plt_theme_legend +
  theme(legend.position = 'bottom', legend.direction = 'vertical', text = element_text( size = 9 ) ) +
  theme( axis.text.x = element_text( angle = 0, vjust =1) )


ggsave( plot = iess_afi_cotiz_inac_riesgo_mascul,
        filename = paste0( parametros$resultado_graficos, 'iess_afi_cotiz_inac_riesgo_mascul',
                           parametros$graf_ext ),
        width = 15, height = 18, units = graf_units, dpi = graf_dpi )

# Distribución de cotizantes inactivos por edad, sexo femenino -------------------------------------
message( '\tGráfico barras cotizantes inactivos por edad, sexo femenino' )
inac_cotiz_fem <- copy( inac_cotiz_fem )

x_lim <- c( -0.5,20)
x_brk <- seq( x_lim[1]+0.5, x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f')

y_lim <- c( -0.5,max(inac_cotiz_fem$num_cot)/1000)
y_brk <- seq( y_lim[1]+0.5, y_lim[2],100)
y_lbl <- formatC( y_brk, digits = 0, format = 'f')

iess_inac_cotiz_fem_1 <- ggplot(inac_cotiz_fem, aes(x=cot, y=num_cot/1000, fill = ' ' ) ) +
  xlab( 'Años de cotizaciones' ) +
  ylab( 'Número de cotizantes  (miles)' ) +
  geom_bar( data = inac_cotiz_fem, stat = 'identity', colour = 'white', size = 0.1) +
  theme_tufte()+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim )+
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim )+
  plt_theme +
  scale_fill_manual( values = parametros$iess_green)+
  theme(text = element_text(  color = 'black' ),  
        legend.text = element_text(  size = rel( 0.8), colour = 'black', face = 'plain', family = tipo_letra  ) )

ggsave( plot = iess_inac_cotiz_fem_1,
        filename = paste0( parametros$resultado_graficos, 'iess_inac_cotiz_fem_1', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Unificado inactivos cotizantes vs anios cotizados ------------------------------------------------
cotizantes_h <- copy( inac_cotiz_masc )
cotizantes_m <- copy( inac_cotiz_fem  )

cotizantes_h <- cotizantes_h[ , list( edad, cot, num_cot = num_cot/1000, tipo = 'Hombres' ) ]
cotizantes_m <- cotizantes_m[ , list( edad, cot, num_cot = num_cot/1000, tipo = 'Mujeres' ) ]
cotizantes_t <- rbind( cotizantes_h, cotizantes_m )

iess_inac_cotiz_anios_unificado <- ggplot( cotizantes_t, aes( x = cot, y = num_cot, fill = tipo) ) +
  xlab( 'Años de cotizaciones' ) +
  ylab( 'Número de cotizantes  (miles) ' ) +
  geom_bar( data = cotizantes_t, stat = 'identity', colour = 'white', size = 0.1,
            position = position_dodge() ) +
  theme_tufte()+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  scale_fill_manual(  values = c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Hombres', 'Mujeres') )+
  theme( text = element_text(  color = 'black' ),  
         legend.text = element_text(  size = rel( 0.6), colour = 'black', 
                                      face = 'plain', family = tipo_letra ),
         legend.position = 'bottom' ,
         legend.title = element_blank( ) )

ggsave( plot = iess_inac_cotiz_anios_unificado,
        filename = paste0( parametros$resultado_graficos, 'iess_inac_cotiz_anios_unificado',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# BARRAS HORIZONTALES ------------------------------------------------------------------------------
message( '\tGráfico barras horizontales cotizantes inactivos por edad, sexo femenino' )
inac_cotiz_fem[, num := num / unidad ]

y_lim <- c( 15, 70 )
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_inac_cotiz_fem_2 <- ggplot(inac_cotiz_fem, aes(x=num, y=edad, fill = ' ' ) ) +
  xlab( 'Afiliados (miles)' ) +
  ylab( 'Edad' ) +
  geom_bar( data = inac_cotiz_fem, stat = 'identity', colour='white',size=0.01) +
  theme_tufte()+
  scale_y_discrete(breaks = y_brk, labels = y_lbl) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_green)+
  theme( text = element_text(  color = 'black' ),  
         legend.text = element_text(  size = rel( 0.8), colour = 'black', face = 'plain', family = tipo_letra  ) )

ggsave( plot = iess_inac_cotiz_fem_2,
        filename = paste0( parametros$resultado_graficos, 'iess_inac_cotiz_fem_2',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Unificado inactivos por edad, sexo y numero ------------------------------------------------------
cotizantes_h <- copy( inac_cotiz_masc )
cotizantes_m <- copy( inac_cotiz_fem )
unidad <- 1e3

cotizantes_h <- cotizantes_h[ , list( edad, num = - num, num_cot = num_cot/1000, tipo = 'Hombres' ) ]
cotizantes_m <- cotizantes_m[ , list( edad, num = num, num_cot = num_cot/1000, tipo = 'Mujeres' ) ]
cotizantes_t <- rbind( cotizantes_h, cotizantes_m )
cotizantes_t <- cotizantes_t[ !is.na( edad) ]

y_lim <- c( 15, 70.5 )
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

salto_x <- 10

brks_y <- seq( -80, 50, salto_x )
lbls_y <- paste0(abs(brks_y ) )

iess_inacti_sexo_unificado <- ggplot( cotizantes_t, aes( x = num, y = edad, fill = tipo) ) +
  xlab( 'Afiliados (miles)' ) +
  ylab( 'Edad' ) +
  geom_bar( data = cotizantes_t[ tipo == 'Mujeres' ], stat = 'identity', colour = 'white', size = 0.1 ) +
  geom_bar( data = cotizantes_t[ tipo == 'Hombres' ], stat = 'identity', colour = 'white', size = 0.1 ) +
  theme_tufte()+
  scale_y_discrete(breaks = y_brk, labels = y_lbl) +
  scale_x_continuous( breaks = brks_y, labels = lbls_y) +
  theme_bw() +
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                     labels = c( 'Hombres', 'Mujeres') )+
  theme( text = element_text(  color = 'black' ),  
         legend.text = element_text(  size = rel( 0.6), colour = 'black', 
                                      face = 'plain', family = tipo_letra ),
         legend.position = 'bottom' ,
         legend.title = element_blank( ) )

ggsave( plot = iess_inacti_sexo_unificado,
        filename = paste0( parametros$resultado_graficos, 'iess_inacti_sexo_unificado', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# PASTEL -------------------------------------------------------------------------------------------
message( '\tGráfico pastel cotizantes inactivos por edad, sexo femenino' )
aux1 <- copy( pastel_distri_inact_cotiz_riesgo_femen[3:6,9:11] )
colnames(aux1) = c('riesgos', 'nada', 'porcentaje')
aux1<-aux1[,-2]

aux1[ , Por:=porcentaje ]
aux1[ , porcentaje:=NULL ]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2 = 1.05 * rpie
aux1 <- data.table( aux1)
iess_pastel_distri_inact_cotiz_riesgo_femen <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = riesgos),
               colour='white') +
  geom_label(  data = aux1[riesgos=='Cumple requisitos para vejez en 5 años'],
               aes(x = rlabel*sin(mid_angle)-0.05, y = rlabel*cos(mid_angle)+0.2,
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
               ), family = tipo_letra, size=3, 
               fill='white', color = 'black') +
  geom_label(  data = aux1[riesgos=='Cumple requisitos para vejez en 1 año o menos'],
               aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                   label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
               ), family = tipo_letra, size=3, 
               fill='white', color = 'black') +
  geom_label( data = aux1[riesgos%nin%c( 'Cumple requisitos para vejez en 1 año o menos', 
                                         'Cumple requisitos para vejez en 5 años' ) ],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
                  hjust = hjust, vjust = vjust), family = tipo_letra, size=3,
              fill='white', color = 'black') +
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'vertical',
         legend.key.size = unit(0.3,'cm'),
         text = element_text( size=9 ),
         legend.spacing.y = unit(-0.9,'cm' ) )

ggsave( plot = iess_pastel_distri_inact_cotiz_riesgo_femen ,
        filename = paste0( parametros$resultado_graficos,
                           'iess_pastel_distri_inact_cotiz_riesgo_femen',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# BURBUJAS -----------------------------------------------------------------------------------------
message( '\tGráfico burbujas cotizantes inactivos por edad, sexo femenino' )
aux1 <- copy(distri_inact_cotiz_riesgo_femen[,2:7])
aux1 <- aux1[,-3]
aux1 <- aux1[,-4]
df2 <- aux1
df2 <- df2[ edad<=70 & impos_años <=45] #Corrección para OIT

x_lim <- c(0,45)
x_brk <- seq( x_lim[1], x_lim[2], 5)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c(-70,-15)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_distri_inact_cotiz_riesgo_femen <- ggplot(df2, aes(x =impos_años , y = -edad) ) +
  geom_point(aes(size = n_afiliados, colour = riesgo3),alpha=0.5) +
  scale_size(range = c(1,15 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[1],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[2],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[3],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[4] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Número de cotizaciones', y = 'Edad' ) +
  guides( color = guide_legend( override.aes = list(size = 5) ) ) +
  guides( size = F ) +
  plt_theme_legend +
  theme(legend.position = 'bottom', legend.direction = 'vertical',text = element_text( size=9 ) ) +
  theme( axis.text.x = element_text(  angle = 0 , vjust =1) )

ggsave( plot = iess_distri_inact_cotiz_riesgo_femen,
        filename = paste0( parametros$resultado_graficos, 'iess_distri_inact_cotiz_riesgo_femen',
                           parametros$graf_ext ),
        width = 15, height = 18, units = graf_units, dpi = graf_dpi )

# Número de pensionistas por tipo de pensión y sexo 2020 3.8 ---------------------------------------
# Ambos sexos---------------------
message( '\tNúmero de pensionistas por tipo de pensión y sexo 2020 (Ambos sexos) ' )
aux1 <- copy( tipo_pensi_sexo[1:4,-c(1:5) ] )
# Femenino-------------------------
message( '\tNúmero de pensionistas por tipo de pensión y sexo 2020 (Femenino)')
aux_2 <- copy( tipo_pensi_sexo[13:16,-c(1:5) ] )
# Masculino--------------------
message( '\tNúmero de pensionistas por tipo de pensión y sexo 2020 (Masculino)')
aux_3 <- copy( tipo_pensi_sexo[20:23,-c(1:5) ] )

# ambos sexos
aux1<-copy( tipo_pensi_sexo[1:4,-c(1:5) ] )
aux1[ , Por:=porcentaje]
aux1[ , porcentaje:=NULL]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2 = 1.05 * rpie
aux1 <- data.table( aux1)
iess_tipo_pension_ambsex <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = tipo_pens),
               colour='white') +
  geom_label( 
    aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
        label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
    ), family = tipo_letra, size=3, 
    fill='white', color = 'black') +
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit(0.7,'cm'),
         text = element_text( size = 10 ),
         legend.spacing.y = unit(-0.9,'cm' ) )

ggsave( plot = iess_tipo_pension_ambsex ,
        filename = paste0( parametros$resultado_graficos, 'iess_tipo_pension_ambsex',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# femenino
aux1 <- copy( tipo_pensi_sexo[13:16,-c(1:5) ] )

aux1[ , Por:=porcentaje]
aux1[ , porcentaje:=NULL]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2 = 1.05 * rpie
aux1 <- data.table( aux1)
iess_tipo_pension_fem <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = tipo_pens),
               colour='white') +
  geom_label( data = aux1[tipo_pens=='Orfandad'],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle)+0.25,
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3, 
              fill='white', color = 'black') +
  geom_label( data = aux1[tipo_pens=='Viudez'],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle)+0.08,
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3, 
              fill='white', color = 'black') +
  geom_label( data = aux1[tipo_pens%nin%c('Orfandad','Viudez') ],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3, 
              fill='white', color = 'black')+
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL ) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL ) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit(0.7,'cm'),
         text = element_text( size = 10 ),
         legend.spacing.y = unit(-0.9,'cm' ) )

ggsave( plot = iess_tipo_pension_fem ,
        filename = paste0( parametros$resultado_graficos, 'iess_tipo_pension_fem', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# masculino
aux1 <- copy( tipo_pensi_sexo[20:23,-c(1:5) ] )
aux1[ , Por:=porcentaje]
aux1[ , porcentaje:=NULL]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2 = 1.05 * rpie
aux1 <- data.table( aux1)
iess_tipo_pension_masc <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = tipo_pens),
               colour='white') +
  geom_label( 
    aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
        label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
    ), family = tipo_letra, size=3, 
    fill='white', color = 'black')+
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit(0.7,'cm'),
         text = element_text( size = 10 ),
         legend.spacing.y = unit(-0.9,'cm' ) )

ggsave( plot = iess_tipo_pension_masc ,
        filename = paste0( parametros$resultado_graficos, 'iess_tipo_pension_masc',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Número de pensionistas por tipo de pensión, edad y sexo, 2020 ------------------------------------
# Masculino
aux5 = copy(pensio_edad_sexo_h[-1,])
aux5$num_vejz1 <- as.numeric(aux5$num_vejz)
aux5$num_inval2 <- as.numeric(aux5$num_inval)
aux5$num_viud3 <- as.numeric(aux5$num_viud)
aux5$num_orfan4<- as.numeric(aux5$num_orfan)
aux5$edad5<- as.numeric(aux5$edad)

base_h = aux5 [,6:10]
base_h$sexo <-  'Masculino'

# Femenino
aux6 = copy(pensio_edad_sexo_m[-1,])
aux6$num_vejz1 <- as.numeric(aux6$num_vejz)
aux6$num_inval2 <- as.numeric(aux6$num_inval)
aux6$num_viud3 <- as.numeric(aux6$num_viud)
aux6$num_orfan4 <- as.numeric(aux6$num_orfan)
aux6$edad5 <- as.numeric(aux6$edad)

base_m = aux6[,6:10]
base_m$sexo = 'Femenino'

# Gráfico completo ---------------------------------------------------------------------------------
base =rbind(base_h, base_m)

base <- base %>%
  gather(-c(sexo, edad5), key = variable, value = valor)
base$variable = factor(base$variable,
                       levels = unique(base$variable),
                       labels = c( 'Vejez', 'Invalidez', 'Viudez', 'Orfandad' ) )

baseM = filter( base, sexo == 'Masculino' )
baseF = filter( base, sexo == 'Femenino' )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim <- c( 0, 15000 )
y_brk <- seq( y_lim[1], y_lim[2], 3000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_pension_sex_M <- ggplot( baseM, aes(x = edad5, y = valor, fill = variable) ) +
  geom_bar(stat = 'identity', width = 1) +
  scale_fill_manual( values = c( 'Vejez' = esc_cl[1],
                                 'Invalidez' = esc_cl[2],
                                 'Viudez' = esc_cl[3],
                                 'Orfandad' = esc_cl[4] ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) + 
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  labs( x = NULL, y = NULL, fill = NULL )+ ggtitle('Masculino') +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 0, hjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) +
  theme( legend.key.size = unit( 0.5, 'cm' ) ) +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit(0.5,'cm'),
         text = element_text( size = 10 ),
         legend.spacing.y = unit(-0.9,'cm' ) ) +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0, 
                               label.vjust = 0.5 ) )+
  guides( shape = guide_legend( title = NULL ) ) 

esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_pension_sex_F <- ggplot( baseF, aes(x = edad5, y = valor, fill = variable) ) +
  geom_bar(stat = 'identity', width = 1) +
  scale_fill_manual( values=c( 'Vejez' = esc_cl[1],
                               'Invalidez' = esc_cl[2],
                               'Viudez' = esc_cl[3],
                               'Orfandad' = esc_cl[4]) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) + 
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  labs(x = NULL, y = NULL, fill = NULL )+ ggtitle('Femenino') + 
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 0, hjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) +
  theme( legend.key.size = unit( 0.5, 'cm' ) ) +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit(0.5,'cm'),
         text = element_text( size = 10 ),
         legend.spacing.y = unit(-0.9,'cm' ) ) +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0,
                               label.vjust = 0.5 ) )+
  guides( shape = guide_legend( title = NULL ) ) 

# Uniendo ------------------------------------------------------------------------------------------
iess_pension_sex <- marrangeGrob( list( iess_pension_sex_M,iess_pension_sex_F),
                                  nrow = 2, ncol = 1, top = '' )
ggsave( plot = iess_pension_sex ,
        filename = paste0( parametros$resultado_graficos, 'iess_pension_sex', parametros$graf_ext ),
        width = 15, height = 18, units = graf_units, dpi = graf_dpi )

# Gastos en beneficios por tipo de pensión y sexo, 2020 --------------------------------------------
message( '\tNúmero de pensionistas por tipo de pensión y sexo 2020 (Ambos sexos) ' )

# Ambos sexos
aux1 <- copy( benef_pensi_sexo[1:4,-c(1:5) ] )
aux1[ , Por := porcentaje ]
aux1[ , porcentaje := NULL ]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2= 1.05 * rpie
aux1 <- data.table( aux1)
iess_benef_pensi_sexo_ambsex <- ggplot(aux1) + 
  geom_arc_bar( aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                    start = start_angle, end = end_angle, fill = tipo_pens),
                colour='white') +
  geom_label( data = aux1[tipo_pens=='Orfandad'],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle)+0.15,
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3,
              fill='white', color = 'black') +
  geom_label( data = aux1[tipo_pens%nin%c('Orfandad') ],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3, 
              fill='white', color = 'black')+
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit(0.7,'cm'),
         text = element_text( size = 10 ),
         legend.spacing.y = unit(-0.9,'cm' ) )

ggsave( plot = iess_benef_pensi_sexo_ambsex ,
        filename = paste0( parametros$resultado_graficos, 'iess_benef_pensi_sexo_ambsex', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# femenino
aux1 <- copy( benef_pensi_sexo[13:16,-c(1:5) ] )
aux1[ , Por:=porcentaje]
aux1[ , porcentaje:=NULL]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2 = 1.05 * rpie
aux1 <- data.table( aux1)
iess_benef_pensi_sexo_femen <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = tipo_pens),
               colour='white') +
  geom_label( data = aux1[tipo_pens=='Orfandad'],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle)+0.3,
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3,
              fill = 'white', color = 'black') +
  geom_label( data = aux1[tipo_pens=='Viudez'],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle)+0.125,
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3,
              fill='white', color = 'black') +
  geom_label( data = aux1[tipo_pens%nin%c('Orfandad', 'Viudez') ],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3, 
              fill = 'white', color = 'black') +
  coord_fixed() + scale_fill_brewer( palette = 'Blues' ) +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit(0.7,'cm'),
         text = element_text( size = 10 ),
         legend.spacing.y = unit( -0.9,'cm' ) )

ggsave( plot = iess_benef_pensi_sexo_femen ,
        filename = paste0( parametros$resultado_graficos, 'iess_benef_pensi_sexo_femen', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# masculino
aux1 <- copy( benef_pensi_sexo[20:23,-c(1:5) ] )
aux1[ , Por:=porcentaje]
aux1[ , porcentaje:=NULL]

aux1 <- data.frame(aux1)

aux1 <- aux1 %>% mutate(Por_total = 1) %>% 
  mutate( end_angle = 2*pi*cumsum(Por)/Por_total,      
          start_angle = lag(end_angle, default = 0),   
          mid_angle = 0.5*(start_angle + end_angle ) ) %>% 
  mutate( aux1,
          hjust = ifelse(mid_angle>pi, 1, 0),
          vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1 ) )

rpie = 1 
rlabel = 0.6 * rpie
rlabel2 = 1.05 * rpie
aux1 <- data.table( aux1)
iess_benef_pensi_sexo_mascul <- ggplot(aux1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = tipo_pens),
               colour='white') +
  geom_label( data = aux1[tipo_pens=='Orfandad'],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle)+0.2,
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3,
              fill='white', color = 'black') +
  geom_label( data = aux1[tipo_pens=='Viudez'],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle)+0.125,
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3,
              fill='white', color = 'black') +
  geom_label( data = aux1[tipo_pens%nin%c('Orfandad', 'Viudez') ],
              aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
                  label = paste0(formatC(round( Por * 100,2), decimal.mark = ','), '%'),
              ), family = tipo_letra, size=3, 
              fill='white', color = 'black') +
  coord_fixed() + scale_fill_brewer(palette='Blues') +
  scale_x_continuous( limits = c(-1, 1.0), name = '', breaks = NULL, labels = NULL ) +
  scale_y_continuous( limits = c(-1, 1.1), name = '', breaks = NULL, labels = NULL ) +
  plt_theme_legend +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit(0.7,'cm'),
         text = element_text( size = 10 ),
         legend.spacing.y  = unit( -0.9,'cm' ) )

ggsave( plot = iess_benef_pensi_sexo_mascul,
        filename = paste0( parametros$resultado_graficos, 'iess_benef_pensi_sexo_mascul',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Valores promedios de beneficios por tipo de pensión, sexo y edad vejez ---------------------------
df = copy(val_prom_benef_sex_1)
x_lim <- c( 40, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 2000 )
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_val_prom_benef_vejz <- ggplot(  ) +
  geom_line( data = df, aes( x = edad,
                             y =  mascu_vejz ,
                             color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( data = df, aes( x = edad,
                             y = femen_vejz ,
                             color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Masculino', 'Femenino' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) + ggtitle( 'Vejez' )

# Valores promedios de beneficios por tipo de pensión, sexo y edad invalidez -----------------------
val_prom_inv <- copy( val_prom_benef_sex_1 )

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1000)
y_brk <- seq( y_lim[1], y_lim[2], 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_val_prom_benef_inv <- ggplot(  ) +
  geom_line( data = val_prom_inv , aes( x = edad,
                                        y =  mascu_inv ,
                                        color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( data = val_prom_inv, aes( x = edad,
                                       y = femen_inv ,
                                       color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Masculino', 'Femenino' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) + ggtitle( 'Invalidez' )

# Valores promedios de beneficios por tipo de pensión, sexo y edad viudez --------------------------
val_prom_viud <- copy( val_prom_benef_sex_2 )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 600 )
y_brk <- seq( y_lim[1], y_lim[2], 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_val_prom_benef_viud <- ggplot(  ) +
  geom_line( data = val_prom_viud , aes( x = edad,
                                         y =  mascu_viud ,
                                         color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( data = val_prom_viud, aes( x = edad,
                                        y = femen_viud ,
                                        color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Masculino', 'Femenino') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) + ggtitle( 'Viudez' )

# Valores promedios de beneficios por tipo de pensión, sexo y edad orfandad ------------------------
val_prom_orfan <- copy( val_prom_benef_sex_2)

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 400 )
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_val_prom_benef_orfan <- ggplot(  ) +
  geom_line( data = val_prom_orfan , aes( x = edad,
                                          y =  mascu_orfan ,
                                          color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = 'round' ) +
  geom_line( data = val_prom_orfan, aes( x = edad,
                                         y = femen_orfan ,
                                         color = parametros$iess_green ),
             size = graf_line_size,
             lineend = 'round' ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ),
                      labels = c( 'Masculino', 'Femenino') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) + ggtitle('Orfandad')

# Uniendo ------------------------------------------------------------------------------------------
iess_val_prom_benef_vej_inv_viu_orf <- marrangeGrob(
  list( iess_val_prom_benef_vejz,
        iess_val_prom_benef_inv,
        iess_val_prom_benef_viud,
        iess_val_prom_benef_orfan ),  
  nrow = 2, ncol = 2, top = ' ')

ggsave( plot = iess_val_prom_benef_vej_inv_viu_orf ,
        filename = paste0( parametros$resultado_graficos, 'iess_val_prom_benef_vej_inv_viu_orf', 
                           parametros$graf_ext ),
        width = 15, height = 17, units = graf_units, dpi = graf_dpi )

# Valores promedio de beneficios por tipo de pensión y sexo, 2020 ----------------------------------
aux <- copy(val_prom_benef__tipo_sex)
aux1 = aux[2,1:4]
names(aux1) = c('Vejez', 'Invalidez', 'Vuidez', 'Orfandad')
aux2 = aux[2,6:9]
names(aux2) = c('Vejez', 'Invalidez', 'Vuidez', 'Orfandad')
aux3 = rbind(aux1, aux2)
aux3$sexo = c('Masculino','Femenino')

aux3 = aux3 %>% 
  gather(-c(sexo), key = variable , value = valor)
aux3$valor1 = as.numeric(aux3$valor)
aux3 = aux3 [,-3]
aux3$variable = factor(aux3$variable,
                       levels = unique(aux3$variable),
                       labels = c('Vejez', 'Invalidez', 'Viudez', 'Orfandad' ) )
aux3M <- filter(aux3, sexo == 'Masculino')
aux3F <- filter(aux3, sexo == 'Femenino')

aux5 = aux [4,1:4]
names(aux5) = c('Vejez', 'Invalidez', 'Vuidez', 'Orfandad')
aux6 = aux [4,6:9]
names(aux6) = c('Vejez', 'Invalidez', 'Vuidez', 'Orfandad')
SBUM = rbind(aux5, aux6)
SBUM$sexo = c('Masculino','Femenino')
SBUM = SBUM %>% 
  gather(-c(sexo), key = variable , value = valor)
SBUM$valor1 = as.numeric(SBUM$valor)
SBUM = SBUM [,-3]
SBUM$variable = factor(SBUM$variable,
                       levels = unique(SBUM$variable),
                       labels = c('Vejez', 'Invalidez', 'Viudez', 'Orfandad' ) )
SBUM_M <- filter(SBUM, sexo == 'Masculino')

SBUM_F <-  filter(SBUM, sexo == 'Femenino')

aux7 = aux [6, 1:4]
names(aux7) = c('Vejez', 'Invalidez', 'Vuidez', 'Orfandad')
aux8 = aux [6, 6:9]
names(aux8) = c('Vejez', 'Invalidez', 'Vuidez', 'Orfandad')
sal_prom = rbind(aux7, aux8)
sal_prom$sexo =c('Masculino','Femenino')
sal_prom = sal_prom %>% 
  gather(-c(sexo), key = variable , value = valor)
sal_prom$valor1 = as.numeric(sal_prom$valor)
sal_prom = sal_prom [,-3]
sal_prom$variable = factor(sal_prom$variable,
                           levels = unique(sal_prom$variable),
                           labels = c( 'Vejez', 'Invalidez', 'Viudez', 'Orfandad' ) )

sal_prom_M <- filter( sal_prom, sexo == 'Masculino' )
sal_prom_F <- filter( sal_prom, sexo == 'Femenino' )

iess_benef_prom_sex_M <- ggplot()+
  geom_bar( data = aux3M , aes(x = variable, y = valor1, fill = 'Beneficios promedio'), 
            stat = 'identity', width = 0.3)+
  geom_line( data = SBUM_M, aes(x = variable, y = valor1, group = 1,
                                linetype = 'SBUM'),
             inherit.aes = FALSE,
             size = 0.5 ) +
  geom_line( data = sal_prom_M, aes(x = variable, y = valor1, group = 1,
                                    linetype = 'Salario promedio'),
             inherit.aes = FALSE,
             size = 0.5 ) +
  scale_fill_manual( values =  c( parametros$iess_blue ) ) +
  scale_y_continuous(  n.breaks = 10 ) + 
  plt_theme_legend +
  labs( x = NULL, y = NULL ) +
  ggtitle('Masculino')+
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1 ) ) +
  theme( legend.position = 'bottom' ) +
  theme( legend.key.size = unit( 0.5, 'cm' ) ) +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0,
                               label.vjust = 0.5 ) )+
  guides( shape = guide_legend( title = NULL ) )

iess_benef_prom_sex_F <- ggplot()+
  geom_bar( data = aux3F , aes( x = variable, y = valor1, fill = 'Beneficios promedio' ),
            stat = 'identity', width = 0.3 )+
  geom_line( data = SBUM_F, aes( x = variable, y = valor1, group = 1, linetype = 'SBUM' ),
             inherit.aes = FALSE,
             size = 0.5 ) +
  geom_line( data = sal_prom_F, aes( x = variable, y = valor1, group = 1, linetype = 'Salario promedio' ),
             inherit.aes = FALSE,
             size = 0.5 ) +
  scale_fill_manual( values =  c( parametros$iess_green ) ) +
  scale_y_continuous(  n.breaks = 10 ) + 
  plt_theme_legend +
  labs( x = NULL, y = NULL ) +
  ggtitle('Femenino')+
  theme( axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1 ) ) +
  theme( legend.position = 'bottom' ) +
  theme( legend.key.size = unit( 0.5, 'cm' ) ) +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0, label.vjust = 0.5 ) )+
  guides( shape = guide_legend( title = NULL ) )

# Uniendo ------------------------------------------------------------------------------------------
iess_benef_prom_sex <- marrangeGrob( list(iess_benef_prom_sex_F, iess_benef_prom_sex_M), nrow = 2, 
                                     ncol = 1, top = ' ' )
ggsave( plot = iess_benef_prom_sex,
        filename = paste0( parametros$resultado_graficos, 'iess_benef_prom_sex', parametros$graf_ext ),
        width = 11.5, height = 16, units = graf_units, dpi = graf_dpi )

#---------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

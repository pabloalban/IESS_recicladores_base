message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura portafolio de inversiones' )
file_inversiones <- paste0( parametros$RData_seg, 'IESS_SAL_inversiones', '.RData' )
load( file = file_inversiones )

message( '\tGráficas del portafolio de inversiones' )
# Evolución histórica de la inversiones del Fondo del Seguro----------------------------------------
aux <- recurs_adm_biess %>% 
  mutate( periodo = ymd( paste0(ano, '/01/01') ) ) %>%
  filter( periodo > ymd('2011-12-31', tz = parametros$time_zone ) )

aux['intrumento']<-'Saldo valor nominal'
aux<-dplyr::select(aux,periodo,intrumento,inversiones,rendimiento_neto)

df_bar <- aux %>% dplyr::select(-rendimiento_neto)
df_line = aux %>% dplyr::select(periodo, rendimiento_neto)

scl = 100000000

y_lim <- c( 0, 1000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_inv_tot<- ggplot(data = df_bar, aes(x = periodo, y = inversiones, fill = intrumento) ) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line, aes(x = periodo, y = rendimiento_neto*scl*100-19180000, group = 1,
                                linetype = 'Rendimiento Neto'),
            inherit.aes = FALSE,
            size=1) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Inversiones (millones USD)', labels = y_lbl, breaks = y_brk, limits =c( -100000000, 900000000 ),
                     sec.axis = sec_axis(~./(scl*100)+0.001918, name = 'Rendimiento Neto',
                                         labels = function(b) { paste0(round(b * 100, 0), '%')},breaks =c(0,0.02,0.04,0.06,0.08))) + 
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue) )+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_inv_tot, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_tot', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolucion de los inv en creditos hipotecarios y quirografarios------------------------------------
setnames(creditos, c('ano', 'Quirografarios', 'Prendarios', 'Total', 'Rendimiento', 
                     'Rendimiento_Ponderado_Real', 'Plazo'))
aux<-as.data.table(creditos)
aux[ , ano := ymd( paste0(ano, '/01/01') ) ]
aux <- as.data.frame( aux )
aux<-dplyr::select(aux,ano,Quirografarios,Prendarios,Rendimiento)
df_bar = melt(aux, id.vars = 'ano', 
              measure.vars = c('Quirografarios', 'Prendarios', 'Rendimiento'),
              variable.name = 'prestamos', 
              value.name = 'monto') %>% filter(prestamos != 'Rendimiento')
df_line = aux %>% dplyr::select(ano, Rendimiento)

scl = 10000000 # escala en cientos de millones
hmts =  6 # homotecia
y_lim <- c( 0, 400000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_creditos_inv<- ggplot(data = df_bar, aes(x = ano, y = monto, fill = prestamos)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line, aes(x = ano, y = Rendimiento*scl*hmts*100-482000000, group = 1,
                                linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=1) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Inversiones (millones USD)', labels = y_lbl, breaks = y_brk,
                     sec.axis = sec_axis(~./(scl*hmts*100)+0.08033333, name = 'Rendimiento Promedio',
                                         labels = function(b) { paste0(round(b * 100, 2), '%')})) + 
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue) )+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_creditos_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_creditos_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en bonos del Estado ----------------------------------------------------------------
setnames( inv_instrumento, c('ano', 
                             'Instrumento', 
                             'Saldo', 
                             'Rendimiento_Ponderado',
                             'Rendimiento_Ponderado_Real',
                             'Plazo',
                             'Inflacion') )
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)

aux<-aux %>% 
  filter(Instrumento=='Bonos del Estado') %>% 
  dplyr::select(Periodo,Instrumento,Saldo,Rendimiento_Ponderado)
df_bar <- aux %>% dplyr::select(-Rendimiento_Ponderado)
df_line = aux %>% dplyr::select(Periodo, Rendimiento_Ponderado)

scl = 100000000  # escala en cientos de millones
hmts = 0.25 # homotecia

y_lim <- c( 0, 200000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_bonos_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = Saldo, fill = Instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = Rendimiento_Ponderado*hmts*scl*100, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=1) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk,
                     sec.axis = sec_axis(~./(scl*hmts*100), name = 'Rendimiento Promedio',
                                         labels = function(b) { paste0(round(b * 100, 2), '%')})) + 
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue) )+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))


ggsave( plot = iess_bonos_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_bonos_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en Obligaciones --------------------------------------------------------------------
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  filter(Instrumento=='Obligaciones') %>% 
  dplyr::select(Periodo,Instrumento,Saldo,Rendimiento_Ponderado)
df_bar <- aux %>% dplyr::select(-Rendimiento_Ponderado)
df_line = aux %>% dplyr::select(Periodo, Rendimiento_Ponderado)

scl = 100000  # escala de millones
hmts = 18 # homotecia

y_lim <- c( 0, 20000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_obligaciones_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = Saldo, fill = Instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = Rendimiento_Ponderado*hmts*scl*100, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=1) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk,
                     sec.axis = sec_axis(~./(scl*hmts*100), name = 'Rendimiento Promedio',
                                         labels = function(b) { paste0(round(b * 100, 2), '%')})) + 
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue) )+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_obligaciones_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_obligaciones_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en Titularizaciones ----------------------------------------------------------------
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  filter(Instrumento %in% c('Titularizaciones'))%>% 
  dplyr::select(Periodo,Instrumento,Saldo,Rendimiento_Ponderado)
aux$Instrumento<-'Titularizaciones'
df_bar <- aux %>% dplyr::select(-Rendimiento_Ponderado)
df_line = aux %>% dplyr::select(Periodo, Rendimiento_Ponderado)

scl = 1000000  # escala de millones
hmts = 5 # homotecia

y_lim <- c( 0, 50000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_titularizaciones_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = Saldo, fill = Instrumento)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = Rendimiento_Ponderado*hmts*scl*100, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=1) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk,limits = c(0,50000000),
                     sec.axis = sec_axis(~./(scl*hmts*100), name = 'Rendimiento Promedio',
                                         labels = function(b) { paste0(round(b * 100, 2), '%')},
                                         breaks=c(0,0.02,0.04,0.06,0.08,0.10))) + 
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue) )+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_titularizaciones_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_titularizaciones_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en sector público-- ----------------------------------------------------------------
aux<-as.data.table(sector_publico)
aux[ , Periodo := ymd( paste0(anio, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  mutate(Instrumento:='Inversiones Sector público')

df_bar <- aux %>% dplyr::select(-rendimiendo, -anio)
df_line = aux %>% dplyr::select(Periodo, rendimiendo)

scl = 1000000  # escala de millones
hmts = 40 # homotecia

y_lim <- c( 0, 400000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 8 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sector_publico_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = saldo, fill = Instrumento )) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = rendimiendo*hmts*scl*100, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=1) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk,limits = c(0,400000000),
                     sec.axis = sec_axis(~./(scl*hmts*100), name = 'Rendimiento Promedio',
                                         labels = function(b) { paste0(round(b * 100, 2), '%')},
                                         breaks=c(0,0.02,0.04,0.06,0.08,0.10))) + 
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue) )+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_sector_publico_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_sector_publico_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfica de la prima del aporte--------------------------------------------------------------------
unidad <- 1e6
aux<-as.data.table( primas_sal )
aux <- aux[is.na(cd1), cd1:=0]
aux <- aux[is.na(cd2), cd2:=0]
aux1<-as.data.frame(aux)
aux2<-data.frame(f=seq( 1, 11, 1 ),Fecha=c(aux1[,1]),cd1=c(aux1[,2])*100,
                 cd2=c(aux1[,3])*100 )

x_lim <- c( 1, 11 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux2[,2])

y_lim <- c( 0, 14 )
y_brk <- seq( y_lim[1], y_lim[2], 2 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_primas_aporte_sal <- ggplot( data = aux2 ) + 
  geom_line( aes( x = f,  y = cd1, color = parametros$iess_blue), 
             size = graf_line_size,
             lineend = "round" ) + 
  geom_line( aes( x = f, y = cd2, color = parametros$iess_green), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = NULL, y = NULL ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'C.D.501', 'C.D.261' ) ) +
  scale_y_continuous( breaks = y_brk, labels = paste0(y_lbl,"%"), limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )+
  theme(legend.box.spacing=unit(0.70,"cm"))+
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+ theme(legend.title=element_blank())

ggsave( plot = iess_primas_aporte_sal, 
        filename = paste0( parametros$resultado_graficos, 'iess_primas_aporte_sal', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Limpiar RAM --------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
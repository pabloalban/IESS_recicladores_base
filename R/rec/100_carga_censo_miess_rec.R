message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura censo del MIESS' )

#Cargando información financiera--------------------------------------------------------------------
file <-
  paste0( parametros$Data,
          'IESS_28-11-2023.xlsx' )

#Carga de recursos administrados por el BIESS-------------------------------------------------------
censo_miess_original <- read_excel( 
  file,
  sheet = 'Hoja1',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>% 
  mutate( edad_mies = factor( edad_mies, levels = c( 'De 7 a 11 años',
                                                     'De 12 a 17 años',
                                                     'De 18 a 29 años',
                                                     'De 30 a 64 años',
                                                     'Mayor a 65 años' ) ) )

censo_miess <- censo_miess_original %>% 
  distinct( iddatosreciclador, .keep_all = TRUE ) %>% 
  filter( ingresos_reciclaje < 5072 ) %>% 
  filter( ingreso_total < 5072 )

#Depuración de datos--------------------------------------------------------------------------------

censo_miess$instruccion <- gsub( 'Superior Universitario Técnico Tecnologico', 'Técnico', censo_miess$instruccion )
censo_miess[ which(censo_miess$instruccion == 'Superior Universitario (Universidades y Escuelas Politécnicas)'), ]$instruccion <- 'Universitario' 
censo_miess[ which(censo_miess$caracteristica_social_afiliado == 'Seguro Ministerio de la Salud Pública (MSP)'), ]$caracteristica_social_afiliado <- 'Seguro Ministerio de la Salud Pública'
censo_miess[ which(censo_miess$caracteristica_social_estuvo_afiliado == 'Seguro Ministerio de la Salud Pública (MSP)'), ]$caracteristica_social_estuvo_afiliado <- 'Seguro Ministerio de la Salud Pública'
censo_miess[ which(censo_miess$caracteristica_social_estuvo_afiliado == 'Aseguramiento Universal de la Salud (AUS)'), ]$caracteristica_social_estuvo_afiliado <- 'Aseguramiento Universal de la Salud'

#Arreglo de sexo------------------------------------------------------------------------------------
censo_miess <- censo_miess %>% 
  mutate( sexo_reciclador = tolower( sexo_reciclador ) )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando base de recicladores de MIESS ' )

save( censo_miess,
      censo_miess_original,
  file = paste0( 
    parametros$RData,
    'MIESS_censo_recicladores.RData'
  )
)

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )

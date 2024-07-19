message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura resultados estudios actuariales' )

#Cargando información financiera--------------------------------------------------------------------
file <-
  paste0( parametros$Data,
          'IESS_tasa_aportes.xlsx' )

#Carga de tasa de CD 515----------------------------------------------------------------------------
tasas_legal <- read_excel( 
  file,
  sheet = 'legal_tasas',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

#Aportes según CD 515-------------------------------------------------------------------------------
aportes_legal <- read_excel( 
  file,
  sheet = 'aportes_legal',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

#Pensiones mínimas----------------------------------------------------------------------------------
minimos_pen <- read_excel( 
  file,
  sheet = 'minimos',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

#Resultados valuaciones actuariales-----------------------------------------------------------------
resultados <- read_excel( 
  file,
  sheet = 'resultados',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando resultados actuariales' )

save( tasas_legal,
      aportes_legal,
      minimos_pen,
      resultados,
      file = paste0( 
        parametros$RData,
        'IESS_resultados_actuariales.RData'
      )
)

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )

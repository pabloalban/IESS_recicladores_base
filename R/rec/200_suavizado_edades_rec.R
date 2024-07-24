message( paste( rep('-', 100 ), collapse = '' ) )

#0.Cargando datos-----------------------------------------------------------------------------------
message( '\tCargando censo del MIESS' )
load( paste0( parametros$RData, 'MIESS_censo_recicladores.RData' ) )

x_ini <- 11
x_lim <- 85
age_grid <- c( x_ini:105 )
  
#1. Preparación de datos----------------------------------------------------------------------------

edad_sexo <- censo_miess %>% 
  group_by( sexo_reciclador,
            edad_mies ) %>% 
  mutate( lx = n( ) ) %>% 
  distinct( sexo_reciclador,
            edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( x_rango := edad_mies,
                 g := sexo_reciclador,
                 lx ) %>% 
  arrange( x_rango ) %>% 
  cbind( ., data.frame( x = c( 11,
                               11,
                               17,
                               17,
                               29,
                               29,
                               64,
                               64,                
                               x_lim,
                               x_lim ) ) ) %>% 
  group_by( g ) %>% 
  mutate( lx_acu = cumsum( lx ) ) %>% 
  ungroup( ) %>%
  dplyr::select( x, g, lx, lx_acu )
  
  
#2. Suavizado para hombres--------------------------------------------------------------------------

edad_sexo_h <- edad_sexo %>% 
  filter( g == 'hombre' )

aux <- edad_sexo_h %>% 
  dplyr::select( x, lx_acu  ) %>% 
  na.omit( . )  %>% 
  rbind( ., data.frame( x = c( 0:6 ),
                       lx_acu = c( rep( 0, 7 ) ) ) ) %>% 
  rbind( ., filter( ., x == x_lim ) ) 

aux[ nrow( aux ), 1 ] <- x_lim + 1

plot( aux$x, aux$lx_acu )


mod <- smooth.spline( aux$x,
                      aux$lx_acu, df = 13 ) 

pred <- data.frame( x = age_grid, 
                    lx_acu_int = predict( mod, age_grid, deriv = 0)[["y"]] ) %>%
  mutate( lx_acu_int = if_else( lx_acu_int < 0,
                              0,
                              lx_acu_int ) )

edad_sexo_h <- expand.grid( g = c( 'hombre' ),
                            x = age_grid ) %>% 
  full_join( ., edad_sexo_h, by = c( 'x', 'g' ) ) %>%
  full_join( ., pred, by = 'x' )

#Gráfico de Control
plot( edad_sexo_h$x, edad_sexo_h$lx_acu )
lines( edad_sexo_h$x, edad_sexo_h$lx_acu_int )

#3. Suavizado para mujeres--------------------------------------------------------------------------

edad_sexo_m <- edad_sexo %>% 
  filter( g == 'mujer' )

aux <- edad_sexo_m %>% 
  dplyr::select( x, lx_acu  ) %>% 
  na.omit( . ) %>%
  rbind( ., data.frame( x = c( 0:6 ),
                        lx_acu = rep( 0, 7 ) ) ) %>% 
  rbind( ., filter( ., x == x_lim ) ) 

aux[ nrow( aux ), 1 ] <- x_lim + 1

plot( aux$x, aux$lx_acu )


mod <- smooth.spline( aux$x,
                      aux$lx_acu, df = 12 ) 

pred <- data.frame( x = age_grid, 
                    lx_acu_int = predict( mod, age_grid, deriv = 0)[["y"]] ) %>%
  mutate( lx_acu_int = if_else( lx_acu_int < 0,
                                0,
                                lx_acu_int ) )

edad_sexo_m <- expand.grid( g = c( 'mujer' ),
                            x = age_grid )%>% 
  full_join( ., edad_sexo_m, by = c( 'x', 'g' ) ) %>%
  full_join( ., pred, by = 'x' ) 

#Gráfico de Control
plot( edad_sexo_m$x, edad_sexo_m$lx_acu )
lines( edad_sexo_m$x, edad_sexo_m$lx_acu_int )


#4. Concatenar bases--------------------------------------------------------------------------------

edad_sexo <- rbind( edad_sexo_h,
                    edad_sexo_m ) %>% 
  group_by( g ) %>% 
  mutate( lx_int = c( 7, diff( lx_acu_int, differences = 1 ) ) ) %>% 
  mutate( lx_int = if_else( lx_int < 0,
                            0,
                            lx_int ) )

plot( edad_sexo$x, edad_sexo$lx_int )

#5. Redondear y precisar frecuencias----------------------------------------------------------------

edad_sexo_int <- edad_sexo %>% 
  mutate( lx_int = round( lx_int, 0 ) ) %>%
  mutate( lx_int = if_else( g == 'hombre' & x == 11,
                            lx_int - 1 ,
                            lx_int ) ) %>%
  mutate( lx_int = if_else( g == 'hombre' & x == 17,
                            lx_int - 6 ,
                            lx_int ) ) %>%
  mutate( lx_int = if_else( g == 'hombre' & x == 29,
                            lx_int - 1 ,
                            lx_int ) ) %>%
  mutate( lx_int = if_else( g == 'hombre' & x == 64,
                            lx_int - 2 ,
                            lx_int ) ) %>%
  # mutate( lx_int = if_else( g == 'hombre' & x == 85,
  #                           lx_int + 1 ,
  #                           lx_int ) ) %>%
  mutate( lx_int = if_else( g == 'mujer' & x == 11,
                            lx_int - 4,
                            lx_int ) ) %>%
  mutate( lx_int = if_else( g == 'mujer' & x == 17,
                            lx_int - 3,
                            lx_int ) ) %>%
  # mutate( lx_int = if_else( g == 'mujer' & x == 17,
  #                           lx_int + 1,
  #                           lx_int ) ) %>%
  mutate( lx_int = if_else( g == 'mujer' & x == 64,
                            lx_int + 1,
                            lx_int ) ) %>%
  # mutate( lx_int = if_else( g == 'mujer' & x == 85,
  #                           lx_int + 1,
  #                           lx_int ) ) %>%
  group_by( g ) %>% 
  mutate( lx_acu_int = cumsum( lx_int ) ) %>% 
  ungroup( )

#Número de recicladores con otros ingresos----------------------------------------------------------
table( censo_miess$otro_ingreso )

table(censo_miess$tiene_discapacidad)
# Guarda resultados --------------------------------------------------------------------------------
message( '\tGuardando suavizamiento de edades' )

save( edad_sexo,
      edad_sexo_int,
      file = paste0( parametros$RData, 'MIESS_censo_recicladores_ajustado.RData' ) )

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )
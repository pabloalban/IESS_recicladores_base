message( paste( rep( "-", 100 ), collapse = "" ) )
#0. Cargando Rdatas---------------------------------------------------------------------------------
message( "\tCargando datos" )
load( paste0( parametros$RData, "MIESS_censo_recicladores.RData" ) )
load( paste0( parametros$RData, 'MIESS_censo_recicladores_ajustado.RData' ) )
message( "\tEstadísticas descriptivas" )

#1.Número de recicladores por edad y sexo-----------------------------------------------------------

cortes_edad <- c( 7, 14, 24, 64, 105 )

etiquetas_edad <-
  c( paste0( 
    "De ",
    formatC( 
      c( 7, 15, 25 ),
      digits = 0,
      format = 'f',
      big.mark = '.',
      decimal.mark = ','
    ),
    " a ",
    formatC( 
      c( 14, 24, 64 ),
      digits = 0,
      format = 'f',
      big.mark = '.',
      decimal.mark = ','
    ),
    ""
  ), "mayor a  65" )

edad_sexo <- edad_sexo_int %>% 
  mutate( rango_edad = cut( 
    x,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
  ) ) %>%
  group_by( g, rango_edad ) %>% 
  mutate( recicladores = sum( lx_int, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( rango_edad, g, .keep_all = TRUE ) %>% 
  mutate( porcentaje = round( 100 * recicladores / sum( recicladores, na.rm = TRUE ), 2 ) ) %>% 
  dplyr::select( g, rango_edad,  recicladores, porcentaje ) %>% 
  pivot_wider( names_from = g,
               values_from = c( recicladores, porcentaje ),
               names_prefix = "_",
               names_repair = "check_unique" ) %>%
  clean_names( ) %>% 
  mutate( total = recicladores_hombre + recicladores_mujer,
          total_porc = porcentaje_hombre + porcentaje_mujer,
          rango_edad = as.character( rango_edad ) ) %>% 
  rbind( ., c( ( "Total" ), as.integer( colSums( .[ , 2:ncol( . ) ], na.rm = TRUE ) ) ) ) %>%
  mutate_at( c( 4, 5, 7 ), as.numeric ) %>%
  mutate_at( c( 2, 3, 6 ), as.integer ) %>% 
  dplyr::select( rango_edad,
                 recicladores_hombre,
                 porcentaje_hombre,
                 recicladores_mujer,
                 porcentaje_mujer,
                 total,
                 total_porc )
  
#2. Ingresos promedios------------------------------------------------------------------------------
## 2.1. Ingreso promedio de reciclaje por edad y sexo-----------------------------------------------

edad_ingreso_rec <- censo_miess %>%
  group_by( edad_mies, sexo_reciclador ) %>%
  mutate( ingreso_promedio = mean( ingresos_reciclaje ) ) %>%
  ungroup( edad_mies, sexo_reciclador )%>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  group_by( edad_mies ) %>% 
  mutate( rec_total = mean( ingresos_reciclaje ) ) %>% 
  ungroup( ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingreso_promedio, rec_total ) %>% 
  pivot_wider( ., 
               names_from = sexo_reciclador,
               values_from = ingreso_promedio,
               names_prefix = "rec_" ) %>% 
  arrange( edad_mies ) %>% 
  dplyr::select( edad_mies, rec_mujer, rec_hombre, rec_total ) %>% 
  mutate( edad_mies = as.character( edad_mies ) ) %>% 
  rbind( ., c( 'Total',
               mean( filter( censo_miess, sexo_reciclador == 'mujer')$ingresos_reciclaje ),
               mean( filter( censo_miess, sexo_reciclador == 'hombre')$ingresos_reciclaje ),
               mean( censo_miess$ingresos_reciclaje ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric )
  

## 2.2. Ingreso promedio total por edad y sexo------------------------------------------------------

edad_ingreso_tot <- censo_miess %>%
  group_by( edad_mies, sexo_reciclador ) %>%
  mutate( ingreso_promedio = mean( ingreso_total ) ) %>%
  ungroup( edad_mies, sexo_reciclador )%>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  group_by( edad_mies ) %>% 
  mutate( tot_total = mean( ingreso_total ) ) %>% 
  ungroup( ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingreso_promedio, tot_total ) %>% 
  pivot_wider( ., 
               names_from = sexo_reciclador,
               values_from = ingreso_promedio,
               names_prefix = "tot_" ) %>% 
  arrange( edad_mies ) %>% 
  dplyr::select( edad_mies, tot_mujer, tot_hombre, tot_total ) %>% 
  mutate( edad_mies = as.character( edad_mies ) ) %>% 
  rbind( ., c( 'Total',
               mean( filter( censo_miess, sexo_reciclador == 'mujer')$ingreso_total ),
               mean( filter( censo_miess, sexo_reciclador == 'hombre')$ingreso_total ),
               mean( censo_miess$ingreso_total ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric )


## 2.3. Ingresos desagregado por edad y sexo--------------------------------------------------------

edad_sexo_ingreso <- left_join( edad_ingreso_rec, edad_ingreso_tot, by = 'edad_mies' ) %>% 
  mutate( otro_hombre = tot_hombre - rec_hombre,
          otro_mujer = tot_mujer - rec_mujer,
          otro_total = tot_total - rec_total ) %>% 
  dplyr::select( edad_mies,
                 rec_mujer,
                 otro_mujer,
                 tot_mujer,
                 rec_hombre,
                 otro_hombre,
                 tot_hombre,
                 rec_total,
                 otro_total,
                 tot_total )

## 2.4. Rango de ingresos totales por sexo y edad---------------------------------------------------

cortes_monto <- c( 0, 50, 90, 106.25, 212.5, 425, Inf )

etiquetas_monto <- c(paste0("(\\$", formatC(cortes_monto[ 1:5 ],
                                            digits = 2, format = 'f', big.mark = '.', decimal.mark = ','),
                            "-\\$", formatC(cortes_monto[ 2:6 ],
                                            digits = 2, format = 'f', big.mark = '.', decimal.mark = ','), "]"), 
                     "Mayor a 425" ) 

rang_sal_total <- censo_miess %>%
  mutate(  rango_monto = cut(  ingreso_total, 
                               breaks = cortes_monto,
                               labels = etiquetas_monto,
                               include.lowest = TRUE,
                               right = TRUE ) ) %>%
  group_by(  rango_monto, sexo_reciclador ) %>%
  mutate(  recicladores = n(  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo_reciclador, rango_monto, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo_reciclador,
                  rango_monto,
                  recicladores ) %>%
  arrange(  sexo_reciclador, rango_monto  ) %>%
  spread(   ., sexo_reciclador, value = c(  recicladores  ),  sep = "_"  )  %>%
  mutate_if(  is.numeric , replace_na, replace = 0 ) %>%
  mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
  mutate(  rango_monto = as.character(  rango_monto  )  ) %>%
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(  porc_mujer = 100 * sexo_reciclador_mujer / sexo_reciclador_mujer[7] , 
           porc_hombre = 100* sexo_reciclador_hombre / sexo_reciclador_hombre[7] , 
           porc_total = 100 * total / total[7] ) %>%
  dplyr::select(  rango_monto, 
                  sexo_reciclador_mujer, porc_mujer, 
                  sexo_reciclador_hombre, porc_hombre, 
                  total, porc_total ) %>%
  distinct(  ., rango_monto, .keep_all = TRUE  )

rang_sal_rec <- censo_miess %>%
  mutate(  rango_monto = cut(  ingresos_reciclaje, 
                               breaks = cortes_monto,
                               labels = etiquetas_monto,
                               include.lowest = TRUE,
                               right = TRUE ) ) %>%
  group_by(  rango_monto, sexo_reciclador ) %>%
  mutate(  recicladores = n(  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo_reciclador, rango_monto, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo_reciclador,
                  rango_monto,
                  recicladores ) %>%
  arrange(  sexo_reciclador, rango_monto  ) %>%
  spread(   ., sexo_reciclador, value = c(  recicladores  ),  sep = "_"  )  %>%
  mutate_if(  is.numeric , replace_na, replace = 0 ) %>%
  mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
  mutate(  rango_monto = as.character(  rango_monto  )  ) %>%
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(  porc_mujer = 100 * sexo_reciclador_mujer / total[7] , 
           porc_hombre = 100* sexo_reciclador_hombre / total[7] , 
           porc_total = 100 * total / total[7] ) %>%
  dplyr::select(  rango_monto, 
                  sexo_reciclador_mujer, porc_mujer, 
                  sexo_reciclador_hombre, porc_hombre, 
                  total, porc_total ) %>%
  distinct(  ., rango_monto, .keep_all = TRUE  )

#3. Número de recicladores por instrucción----------------------------------------------------------

instr_sexo <- censo_miess %>%
  group_by( sexo_reciclador, instruccion ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, instruccion ) %>% 
  distinct( sexo_reciclador, instruccion, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, instruccion, recicladores ) %>% 
  mutate( instruccion = factor( instruccion,  levels = c( 'Centro de alfabetización',
                                                          'Educación Básica',
                                                          'Educación Media/Bachillerato',
                                                          'Jardín de Infantes',
                                                          'Ninguno',
                                                          'Postgrado, Doctorado, PHD',
                                                          'Primaria',
                                                          'Secundaria',
                                                          'Técnico',
                                                          'Universitario') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = recicladores, values_fill = 0) %>% 
  arrange( instruccion ) %>% 
  mutate( total = mujer + hombre,
          instruccion = as.character( instruccion ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = mujer * 100 / total[10],
         porcentaje_hombres = hombre * 100 / total[10],
         porc = total * 100 / total[10]) %>%
  dplyr::select( instruccion, 
                 mujer, porcentaje_mujeres,
                 hombre, porcentaje_hombres, 
                 total, porc ) 

#4. Número de recicladores por provincia------------------------------------------------------------

prov_sexo <- censo_miess %>%
  group_by( sexo_reciclador, provincia ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, provincia ) %>% 
  distinct( sexo_reciclador, provincia, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, provincia, recicladores ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = recicladores, values_fill = 0) %>%
  arrange( provincia ) %>% 
  mutate( total = mujer + hombre,
          provincia = as.character( provincia ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = mujer * 100 / total[25],
         porcentaje_hombres = hombre * 100 / total[25],
         porc = total * 100 / total[25]) %>%
  dplyr::select( provincia, 
                 mujer, porcentaje_mujeres,
                 hombre, porcentaje_hombres, 
                 total, porc)



#5. Número por Acceso a la Seguridad Social---------------------------------------------------------

afiliados_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_afiliado ) %>% 
  mutate( afiliados = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, afiliados ) %>% 
  mutate( caracteristica_social_afiliado = factor( caracteristica_social_afiliado,
                                                   levels = c( 'IESS, Seguro General',
                                                               'IESS, Seguro Voluntario',
                                                               'Ninguno',
                                                               'Seguro Campesino',
                                                               'Seguro de salud privado con hospitalización',
                                                               'Seguro de salud privado sin hospitalización',
                                                               'Seguro del ISSFA ó ISSPOL',
                                                               'Seguro Ministerio de la Salud Pública',
                                                               'Seguro Municipales y de Consejos Provinciales') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = afiliados, values_fill = 0) %>% 
  arrange( caracteristica_social_afiliado ) %>% 
  mutate( total = mujer + hombre,
          caracteristica_social_afiliado = as.character( caracteristica_social_afiliado ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = mujer * 100 / total[10],
         porcentaje_hombres = hombre * 100 / total[10],
         porc = total * 100 / total[10]) %>%
  dplyr::select( caracteristica_social_afiliado,
                 mujer, porcentaje_mujeres, 
                 hombre, porcentaje_hombres,
                 total, porc)

##5.1. Número de personas que estuvieron afiliadas---------------------------------------------------

afiliados_antiguos_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  mutate( afiliados = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_estuvo_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, afiliados ) %>% 
  mutate( caracteristica_social_estuvo_afiliado = factor( caracteristica_social_estuvo_afiliado,
                                                          levels = c( 'Aseguramiento Universal de la Salud',
                                                                      'IESS, Seguro General',  
                                                                      'IESS, Seguro Voluntario',
                                                                      'Ninguno',
                                                                      'Seguro Campesino',
                                                                      'Seguro de salud privado con hospitalización',
                                                                      'Seguro de salud privado sin hospitalización',
                                                                      'Seguro del ISSFA ó ISSPOL',
                                                                      'Seguro Ministerio de la Salud Pública',
                                                                      'Seguro Municipales y de Consejos Provinciales' ) ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = afiliados, values_fill = 0) %>% 
  arrange( caracteristica_social_estuvo_afiliado ) %>% 
  mutate( total = mujer + hombre,
          caracteristica_social_estuvo_afiliado = as.character( caracteristica_social_estuvo_afiliado ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = mujer * 100 / total[11],
         porcentaje_hombres = hombre * 100 / total[11],
         porc = total * 100 / total[11]) %>%
  dplyr::select( caracteristica_social_estuvo_afiliado,
                 mujer, porcentaje_mujeres,
                 hombre, porcentaje_hombres,
                 total, porc )


# 6.Recicladores con carnet de discapacidad---------------------------------------------------------

cortes_discapacidad <- c( 0, 29, 39, 74, 100 )

etiquetas_discapacidad <-
  c( paste0( 
    "De ",
    formatC( 
      c( 0, 30, 40 ),
      digits = 0,
      format = 'f',
      big.mark = '.',
      decimal.mark = ','
    ),
    "\\% a ",
    formatC( 
      c( 29, 39, 74 ),
      digits = 0,
      format = 'f',
      big.mark = '.',
      decimal.mark = ','
    ),
    "\\%"
  ), "mayor a  75\\%" )

discapacidad <- censo_miess %>%
  filter( tiene_discapacidad == 'Si' ) %>%
  group_by( sexo_reciclador ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo_reciclador, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, recicladores )
  

discapacidad_con_carnet <- censo_miess %>%
  arrange( porcentaje_discapacidad ) %>% 
  filter( datos_reciclador_carnet_disc == 'Si' ) %>% 
  mutate( rango_discapacidad = cut( 
    porcentaje_discapacidad,
    breaks = cortes_discapacidad,
    labels = etiquetas_discapacidad,
    #include.lowest = TRUE,
    right = TRUE
  ) ) %>%
  group_by( sexo_reciclador, rango_discapacidad ) %>% 
  mutate( recicladores = n( ) ) %>%
  ungroup( ) %>% 
  distinct( rango_discapacidad, sexo_reciclador, .keep_all = TRUE ) %>% 
  mutate( porcentaje = round( 100 * recicladores / sum( recicladores, na.rm = TRUE ), 4 ) ) %>% 
  dplyr::select( sexo_reciclador, rango_discapacidad,  recicladores, porcentaje ) %>% 
  pivot_wider( names_from = sexo_reciclador,
               values_from = c( recicladores, porcentaje ),
               names_prefix = "",
               names_repair = "check_unique" ) %>%
  clean_names( ) %>% 
  mutate( total = recicladores_hombre + recicladores_mujer,
          total_porc = porcentaje_hombre + porcentaje_mujer,
          rango_discapacidad = as.character( rango_discapacidad ) ) %>% 
  rbind( ., c( ( "Total" ), as.integer( colSums( .[ , 2:ncol( . ) ], na.rm = TRUE ) ) ) ) %>%
  mutate_at( c( 4, 5, 7 ), as.numeric ) %>%
  mutate_at( c( 2, 3, 6 ), as.integer ) %>% 
  dplyr::select( rango_discapacidad,
                 recicladores_hombre,
                 porcentaje_hombre,
                 recicladores_mujer,
                 porcentaje_mujer,
                 total,
                 total_porc )
#7. Tablas para elaboración de pirámides------------------------------------------------------------

## 7.1 Pirámide según instrucción y sexo------------------------------------------------------------
pir_instr_sexo <- censo_miess %>%
  group_by( sexo_reciclador, instruccion ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, instruccion ) %>% 
  distinct( sexo_reciclador, instruccion, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, instruccion, recicladores ) %>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, instruccion, porcentaje )

## 7.2 Pirámide según provincia y sexo--------------------------------------------------------------
pir_prov_sexo <- censo_miess %>%
  group_by( sexo_reciclador, provincia ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, provincia ) %>% 
  distinct( sexo_reciclador, provincia, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, provincia, recicladores ) %>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, provincia, porcentaje ) %>% 
  arrange( provincia )

## 7.3. Pirámide de ingreso promedio de reciclaje según instrucción y sexo--------------------------
pir_edad_sal_prom <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( promedio = mean(ingresos_reciclaje)) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, promedio )

#7.4. Pirámide de ingreso de reciclaje según instrucción y sexo-------------------------------------
pir_edad_sal_reciclaje <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = sum(ingresos_reciclaje) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores ) %>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, edad_mies, porcentaje )

#7.5. Pirámide de ingreso total según instrucción y sexo--------------------------------------------
pir_edad_sal_total <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = sum(ingreso_total) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores )%>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, edad_mies, porcentaje )

#7.6. Pirámide de afiliados por sexo----------------------------------------------------------------
pir_afiliados_sexo <- censo_miess %>%
  group_by( caracteristica_social_afiliado, sexo_reciclador ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( caracteristica_social_afiliado, sexo_reciclador ) %>% 
  distinct( caracteristica_social_afiliado, sexo_reciclador, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, recicladores )%>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, porcentaje )

#7.7. Pirámide de antiguos afiliados por sexo-------------------------------------------------------
pir_afiliados_antiguos_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_estuvo_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, recicladores )%>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, porcentaje )


#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  edad_sexo,
       edad_ingreso_rec,
       edad_ingreso_tot,
       edad_sexo_ingreso,
       rang_sal_total,
       rang_sal_rec,
       instr_sexo,
       prov_sexo,
       afiliados_sexo,
       afiliados_antiguos_sexo,
       discapacidad,
       discapacidad_con_carnet,
       pir_instr_sexo,
       pir_prov_sexo,
       pir_edad_sal_prom,
       pir_edad_sal_reciclaje,
       pir_edad_sal_total,
       pir_afiliados_sexo,
       pir_afiliados_antiguos_sexo,
       file = paste0(  parametros$RData, 'IESS_REC_tablas_demografia.RData'  )  )

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )
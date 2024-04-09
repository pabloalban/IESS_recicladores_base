message( paste( rep( "-", 100 ), collapse = "" ) )
#Cargando Rdatas------------------------------------------------------------------------------------
message( "\tCargando datos" )
load( paste0( parametros$RData, "MIESS_censo_recicladores.RData" ) )

#Estadísticas descriptivas--------------------------------------------------------------------------

#Número de recicladores por edad y sexo------------------------------------------------------------


edad_sexo <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 7 a 11 años',
                                                         'De 12 a 17 años',
                                                         'De 18 a 29 años',
                                                         'De 30 a 64 años',
                                                         'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = recicladores, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 

#Porcentaje de recicladores por edad y sexo------------------------------------------------------------

porc_edad_sexo <- edad_sexo %>%
  mutate(porcentaje_mujeres = Mujer * 100 / Mujer[6],
         porcentaje_hombres = Hombre * 100 / Hombre[6]) %>%
  dplyr::select( edad_mies, porcentaje_mujeres, porcentaje_hombres )

#Número de recicladores por instrucción y sexo------------------------------------------------------

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
                                                          'Superior Universitario (Universidades y Escuelas Politécnicas)',
                                                          'Superior Universitario Técnico Tecnologico' ) ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = recicladores, values_fill = 0) %>% 
  arrange( instruccion ) %>% 
  mutate( total = Mujer + Hombre,
          instruccion = as.character( instruccion ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / Mujer[11],
         porcentaje_hombres = Hombre * 100 / Hombre[11])


#Ingreso promedio por edad y sexo-------------------------------------------------------------------

edad_sal_prom <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingreso_promedio = mean(ingresos_reciclaje)) %>%
  ungroup(edad_mies, sexo_reciclador) %>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingreso_promedio ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 7 a 11 años',
                                                      'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingreso_promedio, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 

#Ingreso de reciclaje por edad y sexo--------------------------------------------------------------- 

edad_sal_reciclaje <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingreso_reciclaje = sum(ingresos_reciclaje)) %>%
  ungroup(edad_mies, sexo_reciclador) %>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingreso_reciclaje ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 7 a 11 años',
                                                      'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingreso_reciclaje, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 

#Ingreso total por edad y sexo----------------------------------------------------------------------

edad_sal_total <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingresos_total = sum(ingreso_total)) %>%
  ungroup(edad_mies, sexo_reciclador) %>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingresos_total ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 7 a 11 años',
                                                      'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingresos_total, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 

#Número de afiliados por sexo-----------------------------------------------------------------------

afiliados_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_afiliado ) %>% 
  mutate( afiliados = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, afiliados ) %>% 
  mutate( caracteristica_social_afiliado = factor( caracteristica_social_afiliado,  levels = c( 'IESS, Seguro General',
                                                                           'IESS, Seguro Voluntario',
                                                                           'Ninguno',
                                                                           'Seguro Campesino',
                                                                           'Seguro de salud privado con hospitalización',
                                                                           'Seguro de salud privado sin hospitalización',
                                                                           'Seguro del ISSFA ó ISSPOL',
                                                                           'Seguro Ministerio de la Salud Pública (MSP)',
                                                                           'Seguro Municipales y de Consejos Provinciales') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = afiliados, values_fill = 0) %>% 
  arrange( caracteristica_social_afiliado ) %>% 
  mutate( total = Mujer + Hombre,
          caracteristica_social_afiliado = as.character( caracteristica_social_afiliado ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 

#Número de personas que estuvieron afiliadas por sexo-----------------------------------------------

afiliados_antiguos_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  mutate( afiliados = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_estuvo_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, afiliados ) %>% 
  mutate( caracteristica_social_estuvo_afiliado = factor( caracteristica_social_estuvo_afiliado,  levels = c( 'Aseguramiento Universal de la Salud (AUS)',
                                                                                                              'IESS, Seguro General',
                                                                                                              'IESS, Seguro Voluntario',
                                                                                                              'Ninguno',
                                                                                                              'Seguro Campesino',
                                                                                                              'Seguro de salud privado con hospitalización',
                                                                                                              'Seguro de salud privado sin hospitalización',
                                                                                                              'Seguro del ISSFA ó ISSPOL',
                                                                                                              'Seguro Ministerio de la Salud Pública (MSP)',
                                                                                                              'Seguro Municipales y de Consejos Provinciales') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = afiliados, values_fill = 0) %>% 
  arrange( caracteristica_social_estuvo_afiliado ) %>% 
  mutate( total = Mujer + Hombre,
          caracteristica_social_estuvo_afiliado = as.character( caracteristica_social_estuvo_afiliado ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 


#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  afiliados_antiguos_sexo,
       afiliados_sexo,
       edad_sal_prom,
       edad_sal_reciclaje,
       edad_sal_total,
       edad_sexo,
       instr_sexo,
       file = paste0(  parametros$RData, 'IESS_REC_tablas_demografia.RData'  )  )
#Limpiar Ram----------------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )


message( paste( rep( "-", 100 ), collapse = "" ) )
#Cargando Rdatas------------------------------------------------------------------------------------
message( "\tCargando datos" )
load( paste0( parametros$RData, "MIESS_censo_recicladores.RData" ) )

censo_miess <- censo_miess %>%
  filter(!(edad_mies %in% c('De 7 a 11 años'))) 

#Estadísticas descriptivas--------------------------------------------------------------------------

#Número de recicladores por edad y sexo-------------------------------------------------------------

edad_sexo <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = recicladores, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 

#Porcentaje de recicladores por edad y sexo---------------------------------------------------------

porc_edad_sexo <- edad_sexo %>%
  mutate(porcentaje_mujeres = Mujer * 100 / Mujer[5],
         porcentaje_hombres = Hombre * 100 / Hombre[5]) %>%
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
         porcentaje_hombres = Hombre * 100 / Hombre[11]) %>%
  dplyr::select( instruccion, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total) 

instr_sexo[9,1] <- 'Superior Universitario'


#Ingreso promedio de reciclaje por edad y sexo-------------------------------------------------------

edad_sal_prom <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingreso_promedio = mean(ingresos_reciclaje)) %>%
  ungroup(edad_mies, sexo_reciclador) %>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingreso_promedio ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
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
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingreso_reciclaje, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / Mujer[5],
         porcentaje_hombres = Hombre * 100 / Hombre[5]) %>%
  dplyr::select( edad_mies, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total)

#Ingreso total por edad y sexo----------------------------------------------------------------------

edad_sal_total <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingresos_total = sum(ingreso_total)) %>%
  ungroup(edad_mies, sexo_reciclador) %>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingresos_total ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingresos_total, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / Mujer[5],
         porcentaje_hombres = Hombre * 100 / Hombre[5]) %>%
  dplyr::select( edad_mies, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total)

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
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / Mujer[10],
         porcentaje_hombres = Hombre * 100 / Hombre[10]) %>%
  dplyr::select( caracteristica_social_afiliado, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total)

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
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / Mujer[11],
         porcentaje_hombres = Hombre * 100 / Hombre[11]) %>%
  dplyr::select( caracteristica_social_estuvo_afiliado, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total)

#Tablas para elaboración de pirámides----------------------------------------------------------------
#Pirámide del porcentaje de recicladores base según edad y sexo--------------------------------------
pir_porc_edad_sexo <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores )%>%
  group_by(sexo_reciclador) %>%
  mutate(total_sexo = sum(recicladores)) %>%
  ungroup() %>%
  mutate(porcentaje = ifelse(sexo_reciclador == "Hombre", recicladores / sum(recicladores[sexo_reciclador == "Hombre"]) * 100,
                             recicladores / sum(recicladores[sexo_reciclador == "Mujer"]) * 100)) %>%
  dplyr::select( sexo_reciclador, edad_mies, porcentaje )

#Pirámide según instrucción y sexo------------------------------------------------------------------
pir_instr_sexo <- censo_miess %>%
  group_by( sexo_reciclador, instruccion ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, instruccion ) %>% 
  distinct( sexo_reciclador, instruccion, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, instruccion, recicladores )%>%
  group_by(sexo_reciclador) %>%
  mutate(total_sexo = sum(recicladores)) %>%
  ungroup() %>%
  mutate(porcentaje = ifelse(sexo_reciclador == "Hombre", recicladores / sum(recicladores[sexo_reciclador == "Hombre"]) * 100,
                             recicladores / sum(recicladores[sexo_reciclador == "Mujer"]) * 100)) %>%
  dplyr::select( sexo_reciclador, instruccion, porcentaje )

#Pirámide de ingreso promedio de reciclaje según instrucción y sexo------------------------------------------------------------------
pir_edad_sal_prom <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( promedio = mean(ingresos_reciclaje)) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, promedio )

#Pirámide de ingreso de reciclaje según instrucción y sexo---------------------------------------------------------------------------
pir_edad_sal_reciclaje <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = sum(ingresos_reciclaje) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores )%>%
  group_by(sexo_reciclador) %>%
  mutate(total_sexo = sum(recicladores)) %>%
  ungroup() %>%
  mutate(porcentaje = ifelse(sexo_reciclador == "Hombre", recicladores / sum(recicladores[sexo_reciclador == "Hombre"]) * 100,
                             recicladores / sum(recicladores[sexo_reciclador == "Mujer"]) * 100)) %>%
  dplyr::select( sexo_reciclador, edad_mies, porcentaje )

#Pirámide de ingreso total según instrucción y sexo---------------------------------------------------------------------------------
pir_edad_sal_total <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = sum(ingreso_total) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores )%>%
  group_by(sexo_reciclador) %>%
  mutate(total_sexo = sum(recicladores)) %>%
  ungroup() %>%
  mutate(porcentaje = ifelse(sexo_reciclador == "Hombre", recicladores / sum(recicladores[sexo_reciclador == "Hombre"]) * 100,
                             recicladores / sum(recicladores[sexo_reciclador == "Mujer"]) * 100)) %>%
  dplyr::select( sexo_reciclador, edad_mies, porcentaje )

#Pirámide de afiliados por sexo-----------------------------------------------------------------------------------------------------
pir_afiliados_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_afiliado ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, recicladores )%>%
  group_by(sexo_reciclador) %>%
  mutate(total_sexo = sum(recicladores)) %>%
  ungroup() %>%
  mutate(porcentaje = ifelse(sexo_reciclador == "Hombre", recicladores / sum(recicladores[sexo_reciclador == "Hombre"]) * 100,
                             recicladores / sum(recicladores[sexo_reciclador == "Mujer"]) * 100)) %>%
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, porcentaje )

#Pirámide de antiguos afiliados por sexo-----------------------------------------------------------------------------------------------------
pir_afiliados_antiguos_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_estuvo_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, recicladores )%>%
  group_by(sexo_reciclador) %>%
  mutate(total_sexo = sum(recicladores)) %>%
  ungroup() %>%
  mutate(porcentaje = ifelse(sexo_reciclador == "Hombre", recicladores / sum(recicladores[sexo_reciclador == "Hombre"]) * 100,
                             recicladores / sum(recicladores[sexo_reciclador == "Mujer"]) * 100)) %>%
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, porcentaje )


#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  afiliados_antiguos_sexo,
       afiliados_sexo,
       edad_sal_prom,
       edad_sal_reciclaje,
       edad_sal_total,
       edad_sexo,
       instr_sexo,
       pir_porc_edad_sexo,
       pir_instr_sexo,
       pir_edad_sal_prom,
       pir_edad_sal_reciclaje,
       pir_edad_sal_total,
       pir_afiliados_sexo,
       pir_afiliados_antiguos_sexo,
       file = paste0(  parametros$RData, 'IESS_REC_tablas_demografia.RData'  )  )


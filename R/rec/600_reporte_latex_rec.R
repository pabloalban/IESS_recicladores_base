# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rec/002_configuracion_seguro.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos------------------------------------------------------------------------------------
#source( 'R/rec/100_carga_censo_miess.R', encoding = 'UTF-8', echo = FALSE )

# Estadísticas descriptivas-------------------------------------------------------------------------
#source( 'R/rec/200_estadisticas_descriptivas_rec.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas----------------------------------------------------------------------------------
source( 'R/macro/500_tab_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos--------------------------------------------------------------------------------
source( 'R/macro/400_graf_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicas de REC ----------------------------------------------------------------------
source('R/rec/400_graf_analisis_demografico_rec.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas de REC ------------------------------------------------------------------------
source( 'R/rec/500_tablas_demograficas_rec.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )



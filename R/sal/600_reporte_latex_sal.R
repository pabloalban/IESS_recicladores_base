# Preparación---------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos y tablas de demografía IVM que van en Salud ---------------------------------------------
source( 'R/ivm/400_graf_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/500_tab_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Configurar seguro --------------------------------------------------------------------------------
source( 'R/sal/002_configuracion_seguro.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos--------------------------------------------------------------------------------
source( 'R/macro/400_graf_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/401_graf_tasa_familia_tipo.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/402_graficos_tasas.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/407_graf_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicos SAL--------------------------------------------------------------------------
# source( 'R/sal/400_graf_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/402_graf_analisis_financiero_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/403_graf_balance_actuarial_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/404_graf_inversiones_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/406_graf_analisis_sensibilidad_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/409_graf_estimacion_sal.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
source( 'R/macro/500_tab_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/503_tab_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas SAL----------------------------------------------------------------------------
source( 'R/sal/501_tab_analisis_financiero_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/502_tab_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/503_tab_estimacion_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/504_tab_escenarios_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/506_tab_analisis_sensibilidad_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/507_tab_estadistica_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/508_tab_inversiones_sal.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX-------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

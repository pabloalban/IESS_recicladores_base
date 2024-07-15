# Ejecución SAL ------------------------------------------------------------------------------------
# Script que describe la ejecución de los cálculos para el seguro de salud
 
# Configuración del seguro -------------------------------------------------------------------------
# Usualmente estos scripts se ejecutan una sola vez cuando ya están listos
source( 'R/sal/002_configuracion_seguro.R', encoding = 'UTF-8', echo = FALSE )

# source( 'R/sal/100_lectura_riesgos_sal.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/sal/101_lectura_gastos_administrativos_sal.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/sal/102_lectura_situacion_actual_sal.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/sal/103_lectura_resultados_sal.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/sal/104_lectura_data_sal.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/sal/105_lectura_analisis_financiero_sal.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/sal/106_lectura_tablas_anexos_sal.R', encoding = 'UTF-8', echo = FALSE )

# Filtros e imputación de datos --------------------------------------------------------------------
# source( 'R/sal/107_filtrando_datos_sal.R', encoding = 'UTF-8', echo = FALSE )

# Estadísticas y estimaciones ----------------------------------------------------------------------
# source( 'R/sal/200_estimacion_sal.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/sal/201_estadistica_sal.R', encoding = 'UTF-8', echo = FALSE )

# Cálculos de escenarios ---------------------------------------------------------------------------
source( 'R/sal/302_calculo_escenarios_balance_sal.R', encoding = 'UTF-8', echo = FALSE )

## Análisis actuariales ----------------------------------------------------------------------------
source( 'R/sal/304_calculo_prima_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/305_analisis_ratios_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/306_analisis_sensibilidad_sal.R', encoding = 'UTF-8', echo = FALSE )

# Generación reporte -------------------------------------------------------------------------------
source( parametros$sal_rep_gen, encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/601_sal_reporte_balance_excel.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/602_comparacion_beneficios.R', encoding = 'UTF-8', echo = FALSE )
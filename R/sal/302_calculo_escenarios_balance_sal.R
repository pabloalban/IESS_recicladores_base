# Cálculo balances SAL -----------------------------------------------------------------------------
message( '\tCalculando escenarios del balance para SAL' )

# Carga información --------------------------------------------------------------------------------
load( file = parametros$macro_rdata_macro_est )

# Preparación de información macroeconómica --------------------------------------------------------
predicciones_anuales <- as.data.table( predicciones_anuales )
val_macro <- as.data.table( tasas_macro_anuales )
val_macro <- rbind(
  data.table( anio = 2020, t_pib = 0, t_sal = 0, t_sbu = 0, tp_anual = 0, inf_anual = 0 ),
  val_macro[ , list( 
    anio, t_pib = t_pib / 100, t_sal = t_sal / 100, t_sbu = t_sbu / 100, tp_anual = tp_anual / 100, 
    inf_anual = inf_anual / 100 ) ]
)
val_macro <- merge.data.table(
  val_macro,
  predicciones_anuales[ anio >= parametros$anio_ini, list( anio, sbu = sbu_anual ) ],
  by = 'anio',
  all.x = TRUE
)

# Objetos comunes a todos los cálculos
parametros_lista <- c( 'parametros', 'parametros_lista', 'val_macro',
                       'sgo_act_tran_anio', 'sgo_pen_tran_anio', 'sgo_comp_tran', 'coef_pen',
                       'pob_proy_ts', 'ben_proy', 'sal_proy', 'pen_proy', 
                       'ben_est_sev', 'ben_est_fre', 'gast_proy', 
                       'esc_ivm', 'esc', 'balance', 'balance_anual' )

# Trayendo escenario de IVM
load( paste0( parametros$RData, 'IVM/IESS_IVM_configuracion_', parametros$anio_ini, '_escenario_1.RData' ) )
esc_ivm <- esc
rm( esc )

# Escenario 1 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_1'
esc$descripcion <- 'Escenario legal'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$sal_reserva_ini # se considera el patrimonio total
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini & anio <= parametros$anio_fin, 
  list( t = 0:parametros$sal_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        i_m = 1.2 * inf_anual,  # inflación de costos de atención de salud
        sbu = sbu,
        
        por_apo_2 = c( 0.0516, rep( 0.0516, 10 ) ), # + 0.1 * 0.008, # prima para financiar el gasto administrativo, en porcentaje de los aportes
        por_apo_4 = 0.0,
        por_apo_5 = 0.0,
        por_apo_7 = 0.0,
        por_apo_8 = 0.0,
        por_apo_9 = 0.0,
        por_apo_11 = 0.0, # prima para financiar beneficios de menores a 18
        
        por_apo_ext_2 = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_4 = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_5 = 0.0415,
        por_apo_ext_7 = 0.0415,
        por_apo_ext_8 = 0.0,
        
        por_apo_gas = 0.04, # límite legal del gasto administrativo en porcentaje de los aportes
        
        por_apo_est_2 = 0.00,
        por_apo_est_4 = 1.00,
        por_apo_est_5 = 1.00,
        por_apo_est_7 = 1.00,
        por_apo_est_8 = 1.00,
        por_apo_est_9 = 0.00,
        por_apo_est_11 = 0.00,
        
        por_apo_est_cat_2 = 1.00,
        por_apo_est_cat_4 = 1.00,
        por_apo_est_cat_5 = 1.00,
        por_apo_est_cat_7 = 1.00,
        por_apo_est_cat_8 = 1.00,
        por_apo_est_cat_9 = 1.00, 
        por_apo_est_cat_11 = 1.00,
        
        cal_mas = esc_ivm$apo_act$cal_mas[1:11],
        cal_pen_vej = esc_ivm$apo_act$cal_pen_vej[1:11],
        cal_pen_inv = esc_ivm$apo_act$cal_pen_inv[1:11],
        cal_pen_viu = esc_ivm$apo_act$cal_pen_viu[1:11],
        cal_pen_orf = esc_ivm$apo_act$cal_pen_orf[1:11],
        cal_gast = 1.0,

        # Calibración de beneficios
        cal_ben_2 = 1.0,
        cal_ben_cat_2 = 1.0 * 1.15,
        cal_ben_4 = 1.0,
        cal_ben_cat_4 = 1.0 * 1.15,
        cal_ben_5 = 1.0,
        cal_ben_cat_5 = 1.0 * 1.15,
        cal_ben_7 = 0.35,
        cal_ben_cat_7 = 0.35 * 1.15,
        cal_ben_8 = 0.6,
        cal_ben_cat_8 = 0.6 * 1.15,
        cal_ben_9 = 0.098, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_cat_9 = 0.098 * 1.15, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_11 = 1.17,
        cal_ben_cat_11 = 1.15 * 1.17 ) ]

setorder( esc$apo_act, t )

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p', 'm' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$sal_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$sal_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$sal_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$sal_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 2 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_2'
esc$descripcion <- 'Escenario base'
message( '\t\t\t', esc$nombre )

esc$V0 <- 489002157.62  # solo se consideran las inversiones en el BIESS
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini & anio <= parametros$anio_fin, 
  list( t = 0:parametros$sal_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        i_m = 1.2 * inf_anual,  # inflación de costos de atención de salud
        sbu = sbu,
        
        por_apo_2 = c( 0.0516, rep( 0.0516, 10 ) ), # + 0.1 * 0.008, # prima para financiar el gasto administrativo, en porcentaje de los aportes
        por_apo_4 = 0.0,
        por_apo_5 = 0.0,
        por_apo_7 = 0.0,
        por_apo_8 = 0.0,
        por_apo_9 = 0.0,
        por_apo_11 = 0.0, # prima para financiar beneficios de menores a 18
        
        por_apo_ext_2 = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_4 = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_5 = 0.0415,
        por_apo_ext_7 = 0.0415,
        por_apo_ext_8 = 0.0,
        
        por_apo_gas = 0.04, # límite legal del gasto administrativo en porcentaje de los aportes
        
        por_apo_est_2 = 0.00,
        por_apo_est_4 = 0.00,
        por_apo_est_5 = 0.00,
        por_apo_est_7 = 0.00,
        por_apo_est_8 = 0.00,
        por_apo_est_9 = 0.00,
        por_apo_est_11 = 0.00,
        
        por_apo_est_cat_2 = 0.00,
        por_apo_est_cat_4 = 0.00,
        por_apo_est_cat_5 = 0.00,
        por_apo_est_cat_7 = 0.00,
        por_apo_est_cat_8 = 0.00,
        por_apo_est_cat_9 = 0.00, 
        por_apo_est_cat_11 = 0.00,
        
        cal_mas = esc_ivm$apo_act$cal_mas[1:11],
        cal_pen_vej = esc_ivm$apo_act$cal_pen_vej[1:11],
        cal_pen_inv = esc_ivm$apo_act$cal_pen_inv[1:11],
        cal_pen_viu = esc_ivm$apo_act$cal_pen_viu[1:11],
        cal_pen_orf = esc_ivm$apo_act$cal_pen_orf[1:11],
        cal_gast = 1.0,
        
        # Calibración de beneficios
        cal_ben_2 = 1.0,
        cal_ben_cat_2 = 1.0 * 1.15,
        cal_ben_4 = 1.0,
        cal_ben_cat_4 = 1.0 * 1.15,
        cal_ben_5 = 1.0,
        cal_ben_cat_5 = 1.0 * 1.15,
        cal_ben_7 = 0.35,
        cal_ben_cat_7 = 0.35 * 1.15,
        cal_ben_8 = 0.6,
        cal_ben_cat_8 = 0.6 * 1.15,
        cal_ben_9 = 0.098, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_cat_9 = 0.098 * 1.15, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_11 = 1.17,
        cal_ben_cat_11 = 1.15 * 1.17 ) ]

setorder( esc$apo_act, t )

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p', 'm' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$sal_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$sal_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$sal_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$sal_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 3 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_3'
esc$descripcion <- 'Escenario alternativo'
message( '\t\t\t', esc$nombre )

esc$V0 <- 489002157.62  # solo se consideran las inversiones en el BIESS
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini & anio <= parametros$anio_fin, 
  list( t = 0:parametros$sal_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        i_m = 1.2 * inf_anual,  # inflación de costos de atención de salud
        sbu = sbu,
        
        por_apo_2 = c( 0.0516, rep( 0.0516, 10 ) ), # + 0.1 * 0.008, # prima para financiar el gasto administrativo, en porcentaje de los aportes
        por_apo_4 = 0.0,
        por_apo_5 = 0.0,
        por_apo_7 = 0.0,
        por_apo_8 = 0.0,
        por_apo_9 = 0.0,
        por_apo_11 = 0.0116, # prima para financiar beneficios de menores a 18
        
        por_apo_ext_2 = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_4 = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_5 = 0.0415,
        por_apo_ext_7 = 0.0415,
        por_apo_ext_8 = 0.0,
        
        por_apo_gas = 0.04, # límite legal del gasto administrativo en porcentaje de los aportes
        
        por_apo_est_2 = 0.00,
        por_apo_est_4 = 0.50,
        por_apo_est_5 = 0.50,
        por_apo_est_7 = 0.50,
        por_apo_est_8 = 0.50,
        por_apo_est_9 = 0.00,
        por_apo_est_11 = 0.00,
        
        por_apo_est_cat_2 = 0.50,
        por_apo_est_cat_4 = 0.50,
        por_apo_est_cat_5 = 0.50,
        por_apo_est_cat_7 = 0.50,
        por_apo_est_cat_8 = 0.50,
        por_apo_est_cat_9 = 0.50, 
        por_apo_est_cat_11 = 0.50,
        
        cal_mas = esc_ivm$apo_act$cal_mas[1:11],
        cal_pen_vej = esc_ivm$apo_act$cal_pen_vej[1:11],
        cal_pen_inv = esc_ivm$apo_act$cal_pen_inv[1:11],
        cal_pen_viu = esc_ivm$apo_act$cal_pen_viu[1:11],
        cal_pen_orf = esc_ivm$apo_act$cal_pen_orf[1:11],
        cal_gast = 1.0,
        
        # Calibración de beneficios
        cal_ben_2 = 1.0,
        cal_ben_cat_2 = 1.0 * 1.15,
        cal_ben_4 = 1.0,
        cal_ben_cat_4 = 1.0 * 1.15,
        cal_ben_5 = 1.0,
        cal_ben_cat_5 = 1.0 * 1.15,
        cal_ben_7 = 0.35,
        cal_ben_cat_7 = 0.35 * 1.15,
        cal_ben_8 = 0.6,
        cal_ben_cat_8 = 0.6 * 1.15,
        cal_ben_9 = 0.098, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_cat_9 = 0.098 * 1.15, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_11 = 1.17,
        cal_ben_cat_11 = 1.15 * 1.17 ) ]

setorder( esc$apo_act, t )

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p', 'm' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$sal_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$sal_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$sal_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$sal_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 4 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_4'
esc$descripcion <- 'Escenario pesimista'
message( '\t\t\t', esc$nombre )

esc$V0 <- 0 # solo se consideran las inversiones en el BIESS
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini & anio <= parametros$anio_fin, 
  list( t = 0:parametros$sal_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        i_m = 1.2 * inf_anual,  # inflación de costos de atención de salud
        sbu = sbu,
        
        por_apo_2 = c( 0.0516, rep( 0.0516, 10 ) ), # + 0.1 * 0.008, # prima para financiar el gasto administrativo, en porcentaje de los aportes
        por_apo_4 = 0.0,
        por_apo_5 = 0.0,
        por_apo_7 = 0.0,
        por_apo_8 = 0.0,
        por_apo_9 = 0.0,
        por_apo_11 = 0.00, # prima para financiar beneficios de menores a 18
        
        por_apo_ext_2 = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_4 = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_5 = 0.0415,
        por_apo_ext_7 = 0.0415,
        por_apo_ext_8 = 0.0,
        
        por_apo_gas = 0.04, # límite legal del gasto administrativo en porcentaje de los aportes
        
        por_apo_est_2 = 0.00,
        por_apo_est_4 = 0.00,
        por_apo_est_5 = 0.00,
        por_apo_est_7 = 0.00,
        por_apo_est_8 = 0.00,
        por_apo_est_9 = 0.00,
        por_apo_est_11 = 0.00,
        
        por_apo_est_cat_2 = 0.00,
        por_apo_est_cat_4 = 0.00,
        por_apo_est_cat_5 = 0.00,
        por_apo_est_cat_7 = 0.00,
        por_apo_est_cat_8 = 0.00,
        por_apo_est_cat_9 = 0.00, 
        por_apo_est_cat_11 = 0.00,
        
        cal_mas = esc_ivm$apo_act$cal_mas[1:11],
        cal_pen_vej = esc_ivm$apo_act$cal_pen_vej[1:11],
        cal_pen_inv = esc_ivm$apo_act$cal_pen_inv[1:11],
        cal_pen_viu = esc_ivm$apo_act$cal_pen_viu[1:11],
        cal_pen_orf = esc_ivm$apo_act$cal_pen_orf[1:11],
        cal_gast = 1.0,
        
        # Calibración de beneficios
        cal_ben_2 = 1.075,
        cal_ben_cat_2 = 1.075 * 1.15,
        cal_ben_4 = 1.075,
        cal_ben_cat_4 = 1.075 * 1.15,
        cal_ben_5 = 1.05,
        cal_ben_cat_5 = 1.075 * 1.15,
        cal_ben_7 = 0.45,
        cal_ben_cat_7 = 0.45 * 1.15,
        cal_ben_8 = 0.7,
        cal_ben_cat_8 = 0.7 * 1.15,
        cal_ben_9 = 0.15, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_cat_9 = 0.15 * 1.15, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_11 = 1.33,
        cal_ben_cat_11 = 1.15 * 1.33 ) ]

setorder( esc$apo_act, t )

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p', 'm' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$sal_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$sal_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$sal_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$sal_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 5: propuesta Risko ---------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_5'
esc$descripcion <- 'Escenario *propuesto*, con la propuesta de sostenibilidad de risko.'
message( '\t\t\t', esc$nombre )

esc$V0 <- 489002157.62 + 350e6 # solo se consideran las inversiones en el BIESS
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini & anio <= parametros$anio_fin, 
  list( t = 0:parametros$sal_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        i_m = 1.2 * inf_anual,  # inflación de costos de atención de salud
        sbu = sbu,
        
        por_apo_2 = c( 0.0594,           # aporte del 2020
                       0.0516,           # aporte del 2021
                       0.0516,           # aporte del 2022
                       0.0516,           # aporte del 2023
                       round( seq( 0.06, 0.08, length.out = 7 ), 5 ) # 2024-2030
        ),
        
        por_apo_4 = 0.0,
        por_apo_5 = 0.0,
        por_apo_7 = 0.0,
        por_apo_8 = 0.0,
        por_apo_9 = 0.00,  # prima para menores de cónyuges dependientes
        por_apo_11 = 0.0103, # prima para financiar beneficios de menores a 18 años
        
        por_apo_ext_2 = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_4 = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
        por_apo_ext_5 = 0.0415,
        por_apo_ext_7 = 0.0415,
        por_apo_ext_8 = 0.0,
        
        por_apo_gas = 0.04,  # límite legal del gasto administrativo en porcentaje de los aportes
        
        por_apo_est_2 = 0.00,
        por_apo_est_4 = 1.00,
        por_apo_est_5 = 1.00,
        por_apo_est_7 = 1.00,
        por_apo_est_8 = 1.00,
        por_apo_est_9 = 0.0,  
        por_apo_est_11 = 0.0,
        
        por_apo_est_cat_2 = 1.00,
        por_apo_est_cat_4 = 1.00,
        por_apo_est_cat_5 = 1.00,
        por_apo_est_cat_7 = 1.00,
        por_apo_est_cat_8 = 1.00,
        por_apo_est_cat_9 = 1.00, 
        por_apo_est_cat_11 = 1.00,
        
        cal_mas = esc_ivm$apo_act$cal_mas[1:11],
        cal_pen_vej = esc_ivm$apo_act$cal_pen_vej[1:11],
        cal_pen_inv = esc_ivm$apo_act$cal_pen_inv[1:11],
        cal_pen_viu = esc_ivm$apo_act$cal_pen_viu[1:11],
        cal_pen_orf = esc_ivm$apo_act$cal_pen_orf[1:11],
        cal_gast = 1.0,
        
        # Calibración de beneficios
        cal_ben_2 = 1.0,
        cal_ben_cat_2 = 1.0 * 1.15,
        cal_ben_4 = 1.0,
        cal_ben_cat_4 = 1.0 * 1.15,
        cal_ben_5 = 1.0,
        cal_ben_cat_5 = 1.0 * 1.15,
        cal_ben_7 = 0.35,
        cal_ben_cat_7 = 0.35 * 1.15,
        cal_ben_8 = 0.6,
        cal_ben_cat_8 = 0.6 * 1.15,
        cal_ben_9 = 0.098, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_cat_9 = 0.098 * 1.15, # incluyendo el porcentaje de extensión de cobertura
        cal_ben_11 = 1.17,
        cal_ben_cat_11 = 1.15 * 1.17 ) ]

setorder( esc$apo_act, t )

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p', 'm' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$sal_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$sal_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$sal_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$sal_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Análisis de Sensibilidad -------------------------------------------------------------------------
## Sensibilidad al Escenario 2 ---------------------------------------------------------------------
## Modificando la tasa de descuento actuarial ------------------------------------------------------
esc_sens <- new.env()
esc_sens$nombre <- 'sensibilidad_1'
esc_sens$descripción <- 'sensibilidad ante la tasa actuarial del escenario 2'
esc_sens$escenario <- 'escenario_2'
esc_sens$escenario_pattern <- paste0( esc_sens$escenario, "_sens_tas_act_" )
esc_sens$var_tas <- seq( 0.0525, 0.0725, 0.0025 )

fls_rem <- list.files( parametros$RData_seg, full.names = TRUE, pattern = esc_sens$escenario_pattern )
if ( length( fls_rem ) > 0 ){
  file.remove( fls_rem )  
}

parametros_lista <- unique( c( parametros_lista, 'esc_sens' ) )

esc_sens$escenario_list <- NULL
esc_sens$escenario_conf_list <- NULL
for ( k in 1:length( esc_sens$var_tas ) ) {
  
  # Escenario sobre el que se construye la sensibilidad
  escenario <- esc_sens$escenario
  
  load( paste0( parametros$sal_rdata_icomp_conf_esc, escenario, '.RData' ) )
  
  esc$nombre <- paste0( esc_sens$escenario_pattern, k )
  esc$cont_sens <- k
  esc_sens$escenario_list <- c( esc_sens$escenario_list, esc$nombre )
  
  message( '\t\t\t', esc$nombre )
  
  esc$apo_act[ , i_a := esc_sens$var_tas[ k ] ]
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p', 'm' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$sal_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$sal_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$sal_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
  
  esc_conf_name <- paste0( parametros$sal_rdata_icomp_conf_esc, esc$nombre, '.RData' )
  save( esc, file = esc_conf_name )
  esc_sens$escenario_conf_list <- c( esc_sens$escenario_conf_list, esc_conf_name )
  rm( esc )
  
}

save( esc_sens,
      file = paste0( parametros$RData_seg, 'IESS_SAL_configuracion_analisis_', esc_sens$nombre, '.RData' ) )

## Modificando el crecimiento promedio de los salarios ---------------------------------------------
esc_sens <- new.env()
esc_sens$nombre <- 'sensibilidad_2'
esc_sens$descripción <- 'sensibilidad ante la tasa de crecimiento salarial del escenario 2'
esc_sens$escenario <- 'escenario_2'
esc_sens$escenario_pattern <- paste0( esc_sens$escenario, "_sens_tas_sal_" )
esc_sens$cam_tas <- seq( 0.9, 1.1, 0.025 )

fls_rem <- list.files( parametros$RData_seg, full.names = TRUE, pattern = esc_sens$escenario_pattern )
if ( length( fls_rem ) > 0 ){
  file.remove( fls_rem )  
}

parametros_lista <- unique( c( parametros_lista, 'esc_sens' ) )

esc_sens$escenario_list <- NULL
esc_sens$escenario_conf_list <- NULL
for ( k in 1:length( esc_sens$cam_tas ) ) {
  
  # Escenario sobre el que se construye la sensibilidad
  escenario <- esc_sens$escenario
  
  load( paste0( parametros$sal_rdata_icomp_conf_esc, escenario, '.RData' ) )
  
  esc$nombre <- paste0( esc_sens$escenario_pattern, k )
  esc$cont_sens <- k
  esc_sens$escenario_list <- c( esc_sens$escenario_list, esc$nombre )
  
  message( '\t\t\t', esc$nombre )
  
  esc$apo_act[ , i_r := esc_sens$cam_tas[ k ] * i_r ]
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p', 'm' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$sal_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$sal_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$sal_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
  
  esc_conf_name <- paste0( parametros$sal_rdata_icomp_conf_esc, esc$nombre, '.RData' )
  save( esc, file = esc_conf_name )
  esc_sens$escenario_conf_list <- c( esc_sens$escenario_conf_list, esc_conf_name )
  rm( esc )
  
}

save( esc_sens,
      file = paste0( parametros$RData_seg, 'IESS_SAL_configuracion_analisis_', esc_sens$nombre, '.RData' ) )


## Modificando el aporte de activos ----------------------------------------------------------------
esc_sens <- new.env()
esc_sens$nombre <- 'sensibilidad_3'
esc_sens$descripción <- 'sensibilidad ante variaciones del aporte de activos del escenario 2'
esc_sens$escenario <- 'escenario_2'
esc_sens$escenario_pattern <- paste0( esc_sens$escenario, "_sens_por_apo_2_" )
esc_sens$por_apo_2 <- seq( 0.05, 0.08, 0.0025 )

fls_rem <- list.files( parametros$RData_seg, full.names = TRUE, pattern = esc_sens$escenario_pattern )
if ( length( fls_rem ) > 0 ){
  file.remove( fls_rem )  
}

parametros_lista <- unique( c( parametros_lista, 'esc_sens' ) )

esc_sens$escenario_list <- NULL
esc_sens$escenario_conf_list <- NULL
for ( k in 1:length( esc_sens$por_apo_2 ) ) {
  
  # Escenario sobre el que se construye la sensibilidad
  escenario <- esc_sens$escenario
  
  load( paste0( parametros$sal_rdata_icomp_conf_esc, escenario, '.RData' ) )
  
  esc$nombre <- paste0( esc_sens$escenario_pattern, k )
  esc$cont_sens <- k
  esc_sens$escenario_list <- c( esc_sens$escenario_list, esc$nombre )
  
  message( '\t\t\t', esc$nombre )
  
  esc$apo_act[ , por_apo_2 := esc_sens$por_apo_2[ k ] ]
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p', 'm' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$sal_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$sal_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$sal_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
  
  esc_conf_name <- paste0( parametros$sal_rdata_icomp_conf_esc, esc$nombre, '.RData' )
  save( esc, file = esc_conf_name )
  esc_sens$escenario_conf_list <- c( esc_sens$escenario_conf_list, esc_conf_name )
  rm( esc )
  
}

save( esc_sens,
      file = paste0( parametros$RData_seg, 'IESS_SAL_configuracion_analisis_', esc_sens$nombre, '.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

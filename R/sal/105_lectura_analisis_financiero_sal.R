message( paste( rep('-', 100 ), collapse = '' ) )

# 1a. ACTIVO ----------------------------------------------------------------------------------------
message( '\tLeyendo Activo del Fondo de Salud al 31 de diciembre de cada año.' )

col_nom <- c( 'anio', 'activo', 'incremento_anual', 'incremento_porcentual_anual')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

activo<-read_excel( file,
                    sheet = "Activo",
                    col_names = TRUE,
                    guess_max = 24000
)
activo <- as.data.table( activo )
setnames( activo, col_nom )

message( '\tGuardando Activo del Fondo de Salud al 31 de diciembre de cada año.' )
#save( activo, file = paste0( parametros$RData_seg, 'IESS_SAL_activo.RData' ) )


# 1b. COMPONENTES DEL ACTIVO -----------------------------------------------------------------------
message( '\tLeyendo Análisis de los componentes del activo del Fondo de Salud' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', '2010', '2011', '2012', '2013', '2014','2015','2016',
              '2017','2018' )

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

comp_activo<-read_excel(file, sheet="Comp_activo", col_names = TRUE, guess_max = 24000)
comp_activo <- as.data.table( comp_activo )
setnames( comp_activo, col_nom )

message( '\tGuardando Análisis de los componentes del activo del Fondo de Salud' )
#save( comp_activo, file = paste0( parametros$RData_seg, 'IESS_SAL_comp_activo.RData' ) )


# 1c. ANALISIS HORIZONTAL ACTIVO -------------------------------------------------------------------

message( '\tLeyendo Análisis horizontal del activo del Fondo de Salud ' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
              'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_horiz_act<-read_excel(file,
                            sheet="Anali_horiz_act",
                            col_names=TRUE,
                            guess_max = 24000)
Anali_horiz_act <- as.data.table( Anali_horiz_act )
setnames( Anali_horiz_act, col_nom )

message( '\tGuardando Análisis horizontal del activo del Fondo de Salud ' )
#save( Anali_horiz_act, file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_activo.RData' ) )


# 1d. ANALISIS VERTICAL ACTIVO ---------------------------------------------------------------------

message( '\tLeyendo Análisis vertical del activo del Fondo de Salud' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8','A9')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_vert_act<-read_excel(file,sheet="Anali_vert_act"
                           ,col_names=TRUE,guess_max = 24000)
Anali_vert_act <- as.data.table( Anali_vert_act )
setnames( Anali_vert_act, col_nom )

message( '\tGuardando Análisis vertical del activo del Fondo de Salud' )
#save( Anali_vert_act, file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_activo.RData' ) )


# 2. cuentas por cobrar ----------------------------------------------------------------------------
message( '\tLeyendo Cuentas por cobrar del Fondo de Salud' )

col_nom <- c( 'anio', 'cuentas por cobrar', 'incremento anual', 'incremento_porcentual_anual')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

ctas_x_cobrar<-read_excel(file,sheet="cuentas_cobrar"
                          ,col_names=TRUE,guess_max = 24000)
ctas_x_cobrar <- as.data.table( ctas_x_cobrar )
setnames( ctas_x_cobrar, col_nom )

message( '\tGuardando Cuentas por cobrar del Fondo de Salud' )
#save( ctas_x_cobrar, file = paste0( parametros$RData_seg, 'IESS_SAL_cuentas_cobrar.RData' ) )

# 2. Subcuentas de cuentas por cobrar --------------------------------------------------------------
message( '\tLeyendo Subcuentas de cuentas por cobrar del Fondo de Salud' )

col_nom <- c( 'anio', 'Anticipos_Unidades_Médicas', 'Deuda_del_Gobierno', 'Otras_c_por_cobrar')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

ctas_sub_x_cobrar<-read_excel(file,sheet="Subcuenta_cuentas_cobrar"
                          ,col_names=TRUE,guess_max = 24000)
ctas_sub_x_cobrar <- as.data.table( ctas_sub_x_cobrar )
setnames( ctas_sub_x_cobrar, col_nom )

message( '\tGuardando Subcuentas de cuentas por cobrar del Fondo de Salud' )
#save( ctas_x_cobrar, file = paste0( parametros$RData_seg, 'IESS_SAL_subcuentas_cobrar.RData' ) )

# 3a. PASIVO ---------------------------------------------------------------------------------------
message( '\tLeyendo Pasivo del Fondo de Salud' )

col_nom <- c( 'anio', 'Pasivo', 'incremento_anual', 'incremento_porcentual_anual')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

Pasivo<-read_excel(file,sheet="Pasivo"
                   ,col_names=TRUE,guess_max = 24000)
Pasivo <- as.data.table( Pasivo )
setnames( Pasivo, col_nom )

message( '\tGuardando Pasivo del Fondo de Salud' )
#save( Pasivo, file = paste0( parametros$RData_seg, 'IESS_SAL_pasivo.RData' ) )
## Se guarda la información en extensión .RData


# 3b. COMPONENTES DEL PASIVO ------------------------------------------------------------------
message( '\tLeyendo Análisis de los componentes del pasivo del Fondo de Salud' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', '2010', '2011', '2012', '2013','2014','2015','2016','2017','2018' )

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Comp_pasivo<-read_excel(file,sheet="Comp_pasivo"
                        ,col_names=TRUE,guess_max = 24000)
Comp_pasivo <- as.data.table( Comp_pasivo )
setnames( Comp_pasivo, col_nom )

message( '\tGuardando Análisis de los componentes del pasivo del Fondo de Salud' )
#save( Comp_pasivo, file = paste0( parametros$RData_seg, 'IESS_SAL_comp_pasivo.RData' ) )


# 3c. ANALISIS HORIZONTAL PASIVO --------------------------------------------------------------
message( '\tLeyendo  Análisis horizontal del pasivo del Fondo de Salud' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_Horiz_Pasivo<-read_excel(file,sheet="Anali_Horiz_Pasivo"
                               ,col_names=TRUE,guess_max = 24000)
Anali_Horiz_Pasivo <- as.data.table( Anali_Horiz_Pasivo )
setnames( Anali_Horiz_Pasivo, col_nom )

message( '\tGuardando  Análisis horizontal del pasivo del Fondo de Salud' )
#save( Anali_Horiz_Pasivo, file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_pasivo.RData' ) )


# 3d. ANALISIS VERTICAL PASIVO ----------------------------------------------------------------
message( '\tLeyendo Análisis vertical del pasivo del Fondo de Salud' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8','A9')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_Vert_Pasivo<-read_excel(file,sheet="Anali_Vert_Pasivo"
                              ,col_names=TRUE,guess_max = 24000)
Anali_Vert_Pasivo <- as.data.table( Anali_Vert_Pasivo )
setnames( Anali_Vert_Pasivo, col_nom )

message( '\tGuardando Análisis vertical del pasivo del Fondo de Salud' )
#save( Anali_Vert_Pasivo, file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_pasivo.RData' ) )


# 4. PRESTADORES POR PAGAR  -----------------------------------------------------------------------
message( '\tLeyendo Cuentas prestadores por pagar del Fondo de Salud al 31 de diciembre de cada año' )

col_nom <- c( 'anio', 'Prestadores por pagar', 'incremento anual', 'incremento_porcentual_anual')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

Pres_Pagar<-read_excel(file,sheet="Prestadores_Pagar"
                       ,col_names=TRUE,guess_max = 24000)
Pres_Pagar <- as.data.table( Pres_Pagar )

setnames( Pres_Pagar, col_nom )

message( '\tGuardando prestadores por pagar del Fondo de Salud al 31 de diciembre de cada año' )
#save( Pres_Pagar, file = paste0( parametros$RData_seg, 'IESS_SAL_prestadores_pagar.RData' ) )

# 4.1 CUENTAS PASIVO NO CORRIENTE-------------------------------------------------------------------
message( '\tLeyendo Cuentas pasivo no corriente del Fondo de Salud al 31 de diciembre de cada año' )

col_nom <- c( 'anio', 'Dif neteo', 'Cuentas por pagar', 'A. M. jubilados', 'A.M. catastróficas')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

Pas_no_corr<-read_excel(file,sheet="Pasivo_no_corr"
                       ,col_names=TRUE,guess_max = 24000)
Pas_no_corr <- as.data.table( Pas_no_corr )

setnames( Pas_no_corr, col_nom )

message( '\tGuardando prestadores por pagar del Fondo de Salud al 31 de diciembre de cada año' )
#save( Pas_no_corr, file = paste0( parametros$RData_seg, 'IESS_SAL_pasivo_no_corriente.RData' ) )




# 5a. PATRIMONIO -------------------------------------------------------------------------------
message( '\tLeyendo  Patrimonio del Fondo de Salud al 31 de diciembre de cada año' )

col_nom <- c( 'anio', 'patrimonio', 'incremento anual', 'incremento_porcentual_anual')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

Patrimonio.<-read_excel(file,sheet="Patrimonio."
                        ,col_names=TRUE,guess_max = 24000)
Patrimonio. <- as.data.table( Patrimonio. )
setnames( Patrimonio., col_nom )

message( '\tGuardando Patrimonio del Fondo de Salud al 31 de diciembre de cada año' )
#save( Patrimonio., file = paste0( parametros$RData_seg, 'IESS_SAL_patrimonio.RData' ) )

# 5b. COMPONENTES DEL PATRIMONIO ---------------------------------------------------------------
message( '\tLeyendo Análisis de los componentes del patrimonio del Fondo de Salud' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', '2010', '2011', '2012', '2013','2014','2015','2016','2017','2018' )

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Com_Patrimonio<-read_excel(file,sheet="Com_Patrimonio"
                           ,col_names=TRUE,guess_max = 24000)
Com_Patrimonio <- as.data.table( Com_Patrimonio )
setnames( Com_Patrimonio, col_nom )

message( '\tGuardando Análisis de los componentes del patrimonio del Fondo de Salud' )
#save(Com_Patrimonio, file = paste0( parametros$RData_seg, 'IESS_SAL_comp_patrimonio.RData' ) )

# 5c. ANALISIS HORIZONTAL PATRIMONIO ----------------------------------------------------------
message( '\tLeyendo  Análisis horizontal del patrimonio del Fondo de Salud' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_horiz_Patr<-read_excel(file,sheet="Anali_horiz_Patr"
                             ,col_names=TRUE,guess_max = 24000)
Anali_horiz_Patr <- as.data.table( Anali_horiz_Patr )
setnames( Anali_horiz_Patr, col_nom )

message( '\tGuardando  Análisis horizontal del patrimonio del Fondo de Salud' )
#save( Anali_horiz_Patr, file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_patrimonio.RData' ) )

# 5d. ANALISIS VERTICAL PATRIMONIO ------------------------------------------------------------
message( '\tLeyendo  Análisis vertical del patrimonio del Fondo del Seguro de Salud' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8','A9')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_vert_patr<-read_excel(file,sheet="Anali_vert_patr"
                            ,col_names=TRUE,guess_max = 24000)
Anali_vert_patr <- as.data.table( Anali_vert_patr )
setnames( Anali_vert_patr, col_nom )

message( '\tGuardando Análisis vertical del patrimonio del Fondo del Seguro de Salud' )
#save( Anali_vert_patr, file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_patrimonio.RData' ) )


# 6. INGRESOS ---------------------------------------------------------------------------------
message( '\tLeyendo Análisis de los Ingresos del Fondo del Seguro de Salud' )

col_nom <- c( 'anio', 'ingresos', 'incremento anual', 'incremento_porcentual_anual')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

ingresos.<-read_excel(file,sheet="ingresos."
                      ,col_names=TRUE,guess_max = 24000)
ingresos. <- as.data.table( ingresos. )
setnames( ingresos., col_nom )

message( '\tGuardando Análisis de los Ingresos del Fondo del Seguro de Salud' )
#save( ingresos., file = paste0( parametros$RData_seg, 'IESS_SAL_ingresos.RData' ) )

# COMPONENTES DE LOS INGRESOS

message( '\tLeyendo  Evolución de los componentes de los ingresos del Fondo de Salud.' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', '2010', '2011', '2012', '2013','2014','2015','2016','2017','2018' )

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Comp_Ingresos..<-read_excel(file,sheet="Comp_Ingresos.."
                            ,col_names=TRUE,guess_max = 24000)
Comp_Ingresos.. <- as.data.table( Comp_Ingresos.. )
setnames( Comp_Ingresos.., col_nom )

message( '\tGuardando Evolución de los componentes de los ingresos del Fondo de Salud.' )
#save(Comp_Ingresos.., file = paste0( parametros$RData_seg, 'IESS_SAL_comp_ingresoss.RData' ) )

#NOTA 
message( '\tLeyendo cuentas de intereses del fondo de salud a ser consideradas' )

col_nom <- c( 'N', 'intereses', 'año_2016', 'año_2017', 'año_2018')

col_tip <- c( 'numeric', 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

nota<-read_excel(file,sheet="nota"
                 ,col_names=TRUE,guess_max = 24000)
nota <- as.data.table( nota )
setnames( nota, col_nom )

message( '\tGuardando cuentas de intereses del fondo de salud a ser consideradas' )
#save( nota, file = paste0( parametros$RData_seg, 'IESS_SAL_ingresos_nota.RData' ) )

#NOTA 1
message( '\tLeyendo cuentas de intereses (ingresos extraordinarios) del fondo de salud a no ser consideradas' )

col_nom <- c( 'intereses', 'año_2015','año_2016', 'año_2017', 'año_2018')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

nota_1<-read_excel(file,sheet="nota_1"
                   ,col_names=TRUE,guess_max = 24000)
nota_1 <- as.data.table( nota_1 )
setnames( nota_1, col_nom )

message( '\tGuardando cuentas de intereses del fondo de salud a no ser consideradas' )
#save( nota_1, file = paste0( parametros$RData_seg, 'IESS_SAL_ingresos_nota_1.RData' ) )

#ANALISIS HORIZONTAL INGRESOS
message( '\tLeyendo Análisis horizontal de los ingresos del Fondo de Salud.' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_horz_Ingresos<-read_excel(file,sheet="Anali_horz_Ingresos"
                                ,col_names=TRUE,guess_max = 24000)
Anali_horz_Ingresos <- as.data.table( Anali_horz_Ingresos )
setnames( Anali_horz_Ingresos, col_nom )

message( '\tGuardando Análisis horizontal de los ingresos del Fondo de Salud.' )
#save( Anali_horz_Ingresos, file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_ingresos.RData' ) )

# ANALISIS VERTICAL INGRESOS

message( '\tLeyendo Análisis vertical de los Ingresos del Fondo de Salud.' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8','A9')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_vert_Ingresos<-read_excel(file,sheet="Anali_vert_Ingresos"
                                ,col_names=TRUE,guess_max = 24000)
Anali_vert_Ingresos <- as.data.table( Anali_vert_Ingresos )
setnames( Anali_vert_Ingresos, col_nom )

message( '\tGuardando Análisis vertical de los Ingresos del Fondo de Salud.' )
#save( Anali_vert_Ingresos, file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_ingresos.RData' ) )

# INGRESOS por aportes
message( '\tLeyendo  Evolución de ingresos por aportes del Fondo de Salud.' )

col_nom <- c( 'anio', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

Aportes<-read_excel(file,sheet="Aportes"
                    ,col_names=TRUE,guess_max = 24000)
Aportes <- as.data.table( Aportes )
setnames( Aportes, col_nom )

message( '\tGuardando  Evolución de ingresos por aportes del Fondo de Salud.' )
#save( Aportes, file = paste0( parametros$RData_seg, 'IESS_SAL_ing_aportes.RData' ) )

# OTROS INGRESOS
message( '\tLeyendo  Evolución histórica de la contribución del Estado.' )

col_nom <- c( 'anio', 'contribucion_del_estado', 'incremento_anual', 'incremento_porcentual_anual')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

contri_estado<-read_excel(file,sheet="contri_estado"
                          ,col_names=TRUE,guess_max = 24000)
contri_estado <- as.data.table( contri_estado )
setnames( contri_estado, col_nom )

message( '\tGuardando Evolución histórica de la contribución del Estado.' )
#save( contri_estado, file = paste0( parametros$RData_seg, 'IESS_SAL_contrib_del_estado.RData' ) )

#GASTOS
message( '\tLeyendo Evolución histórica de los gastos' )

col_nom <- c( 'anio', 'contribucion_del_estado', 'incremento_anual', 'incremento_porcentual_anual')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

Egresos<-read_excel(file,sheet="Egresos"
                    ,col_names=TRUE,guess_max = 24000)
Egresos <- as.data.table( Egresos )
setnames( Egresos, col_nom )

message( '\tGuardando Evolución histórica de los gastos' )
#save( Egresos, file = paste0( parametros$RData_seg, 'IESS_SAL_gasto.RData' ) )

# COMPONENTES DE LOS GASTOS
message( '\tLeyendo Análisis del componente de la evolución de los gastos del Fondo de Salud.' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', '2010', '2011', '2012', '2013','2014','2015','2016','2017','2018' )

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Comp_Egresos<-read_excel(file,sheet="Comp_Egresos"
                         ,col_names=TRUE,guess_max = 24000)
Comp_Egresos <- as.data.table( Comp_Egresos )
setnames( Comp_Egresos, col_nom )

message( '\tGuardando Análisis del componente de la evolución de los gastos del Fondo de Salud.' )
#save(Comp_Egresos, file = paste0( parametros$RData_seg, 'IESS_SAL_comp_gastos.RData' ) )

#ANALISIS HORIZONTAL GASTOS
message( '\tLeyendo Análisis horizontal de la evolución de los gastos del Fondo de Salud.' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_horiz_Egresos<-read_excel(file,sheet="Anali_horiz_Egresos"
                                ,col_names=TRUE,guess_max = 24000)
Anali_horiz_Egresos <- as.data.table( Anali_horiz_Egresos )
setnames( Anali_horiz_Egresos, col_nom )

message( '\tGuardando Análisis horizontal de la evolución de los gastos del Fondo de Salud.' )
#save( Anali_horiz_Egresos, file = paste0( parametros$RData_seg, 'IESS_SAL_horizontal_gastos.RData' ) )

# ANALISIS VERTICAL GASTOS
message( '\tLeyendo  Análisis vertical de la evolución de los gastos del Fondo de Salud.' )

col_nom <- c( 'DESCRIPCIÓN DE LAS CUENTAS', 'A1', 'A2', 'A3','A4','A5','A6','A7','A8','A9')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero_horizontal.xlsx' )

Anali_Vert_Egresos<-read_excel(file,sheet="Anali_Vert_Egresos"
                               ,col_names=TRUE,guess_max = 24000)
Anali_Vert_Egresos <- as.data.table( Anali_Vert_Egresos )
setnames( Anali_Vert_Egresos, col_nom )

message( '\tGuardando  Análisis vertical de la evolución de los gastos del Fondo de Salud.' )
#save( Anali_Vert_Egresos, file = paste0( parametros$RData_seg, 'IESS_SAL_vertical_gasto.RData' ) )

#GASTOS PRESTACIONALES
message( '\tLeyendo Evolución de los egresos por pago de prestaciones Salud.' )

col_nom <- c( 'anio', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

Gast_Presta<-read_excel(file,sheet="Gast_Presta"
                        ,col_names=TRUE,guess_max = 24000)
Gast_Presta <- as.data.table( Gast_Presta )
setnames( Gast_Presta, col_nom )

message( '\tGuardando Evolución de los egresos por pago de prestaciones Salud.' )
#save( Gast_Presta, file = paste0( parametros$RData_seg, 'IESS_SAL_gasto_prestac.RData' ) )

#OTROS GASTOS
message( '\tLeyendo Evolución de Gastos de Administración del Fondo Salud.' )

col_nom <- c( 'anio', 'A1', 'A2', 'A3', 'A4' )

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

file <- paste0( parametros$Data_seg, 'IESS_SAL_lectura_finaciero.xlsx' )

Gast_Adminis<-read_excel(file,sheet="Gast_Adminis"
                         ,col_names=TRUE,guess_max = 24000)
Gast_Adminis <- as.data.table( Gast_Adminis )
setnames(Gast_Adminis, col_nom )


# SAVER --------------------------------------------------------------------------------------------
message( '\tGuardando Evolución de Gastos de Administración del Fondo Salud.' )
#save( Gast_Adminis, file = paste0( parametros$RData_seg, 'IESS_SAL_otros_gasto.RData' ) )
lista <- c('activo', 'comp_activo', 'Anali_horiz_act', 'Anali_vert_act', 'ctas_x_cobrar','ctas_sub_x_cobrar',
           'Pasivo', 'Comp_pasivo', 'Anali_Horiz_Pasivo', 'Anali_Vert_Pasivo',
           'Pres_Pagar','Pas_no_corr', 'Patrimonio.', 'Com_Patrimonio', 'Anali_horiz_Patr',
           'Anali_vert_patr', 'ingresos.', 'Comp_Ingresos..', 'nota', 'nota_1',
           'Anali_horz_Ingresos', 'Anali_vert_Ingresos', 'Aportes', 'contri_estado',
           'Egresos', 'Comp_Egresos', 'Anali_horiz_Egresos', 'Anali_Vert_Egresos',
           'Gast_Presta', 'Gast_Adminis')

save( list = lista,
      file = paste0( parametros$RData_seg, 'IESS_SAL_analisis_financiero.RData' ) )

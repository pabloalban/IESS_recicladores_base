# ####################################################################################
# PARA LETURA DE LAS TABLAS DE SALUD DEL CAPITULO 7: VALORACION DE RIESGOS INVOLUCRADOS
# ####################################################################################
# LECTURA DE AFILIADOS Y MASA SALARIAL -------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo población afiliada y masa salarial' )

col_nom <- c( 'Anio', 'Afiliados', 'Creci_afi','Tasa_creci_afi','Masa','Creci_masa', 
              'Tasa_creci_masa')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pob_afi_masa<-read_excel(file,sheet="Afiliados_masa"
                        ,col_names=TRUE,guess_max = 24000)
pob_afi_masa <- as.data.table( pob_afi_masa )
setnames( pob_afi_masa, col_nom )

message( '\tGuardando población afiliada y masa salarial' )


# Poblacion Ecuatoriana 2010-2018(INEC) ------------------------------------------------------------
message( '\tLeyendo poblacion ecuatoriana 2010-2018 INEC' )
col_nom <- c( 'Edad', '2010','2011','2012','2013','2014','2015','2016','2017','2018')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
              'numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
poblacion_ecuatoriana_inec_sal<-read_excel(file,sheet="Poblacion_ecuatorina",col_names=TRUE, 
                                           guess_max = 24000)
poblacion_ecuatoriana_inec_sal <- as.data.table( poblacion_ecuatoriana_inec_sal )
setnames( poblacion_ecuatoriana_inec_sal, col_nom )
message( '\tGuardando poblacion ecuatoriana INEC' )

# Poblacion ecuatoriana y distribucion geografica --------------------------------------------------
message( '\tLeyendo poblacion ecuatoriana y distribucion geografica' )
col_nom <- c( 'Anio','Sierra', 'Costa', 'Amazonia','Galapagos','No_delimitada','Total_pais')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pob_ecu_region_inec_sal <-read_excel(file,sheet="Poblacion_geografica",col_names=TRUE, 
                                     guess_max = 24000)
pob_ecu_region_inec_sal <- as.data.table( pob_ecu_region_inec_sal )
setnames( pob_ecu_region_inec_sal, col_nom )
message( '\tGuardando pensionistas  inicial' )
 

# Natalidad y Mortalidad en el Ecuador -------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLeyendo Natalidad y Mortalidad en el Ecuador' )
col_nom <- c( 'Anio', 'GENERAL_na', 'MASCULINA_na', 'FEMENINA_na','GENERAL_mor','MASCULINA_mor',
              'FEMENINA_mor' )
col_tip <- c( 'character', 'numeric', 'numeric', 'character', 'numeric', 'numeric', 'character'  )
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
natalidad_mortalidad_sal<-read_excel(file,sheet="Natalidad",col_names=TRUE,guess_max = 24000)
natalidad_mortalidad_sal <- as.data.table( natalidad_mortalidad_sal )
setnames( natalidad_mortalidad_sal, col_nom )
message( '\tGuardando población afiliada activa inicial' )


# Poblacion afiliada por rangos de edad  ----------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPoblacion afiliada por rangos de edad' )
col_nom <- c( 'Edad','2011','2012','2013','2014','2015','2016','2017','2018')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
              'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
afiliados_edad <-read_excel(file,sheet="Afiliados_edad",col_names=TRUE,guess_max = 24000)
afiliados_edad <- as.data.table( afiliados_edad )
setnames( afiliados_edad, col_nom )
message( '\tGuardando poblacion afiliada por rangos de edad' )



# Pensionistas de vejez   -----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPensionistas de vejez' )
col_nom <- c( 'Anio', 'Numero')
col_tip <- c( 'character', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pensionistas_vejez_sal <-read_excel(file,sheet="Pensionistas_vejez",col_names=TRUE, 
                                    guess_max = 24000)
pensionistas_vejez_sal <- as.data.table( pensionistas_vejez_sal )
setnames( pensionistas_vejez_sal, col_nom )
message( '\tGuardando pensionistas de vejez' )


# Pensionistas de invalidez   ----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPensionistas de invalidez' )
col_nom <- c( 'Anio', 'Numero')
col_tip <- c( 'character', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pensionistas_inva_sal <-read_excel(file,sheet="Pensionistas_inva",col_names=TRUE, 
                                   guess_max = 24000)
pensionistas_inva_sal <- as.data.table( pensionistas_inva_sal )
setnames( pensionistas_inva_sal, col_nom )
message( '\tGuardando pensionistas de vejez' )


# Pensionistas de discapacidad   -------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPensionistas de discapacidad' )
col_nom <- c( 'Anio', 'Numero')
col_tip <- c( 'character', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pensionistas_disc_sal <-read_excel(file,sheet="Pensionistas_disc",col_names=TRUE,guess_max = 24000)
pensionistas_disc_sal <- as.data.table( pensionistas_disc_sal )
setnames( pensionistas_disc_sal, col_nom )
message( '\tGuardando pensionistas de vejez' )

# Montepio   ---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPensionistas de montepio' )
col_nom <- c( 'Anio', 'Numero')
col_tip <- c( 'character', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pensionistas_monte_sal <-read_excel(file,sheet="Montepio",col_names=TRUE,guess_max = 24000)
pensionistas_monte_sal <- as.data.table( pensionistas_monte_sal )
setnames( pensionistas_monte_sal, col_nom )
message( '\tGuardando pensionistas de vejez' )


# Total pensionistas   -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPensionistas total' )
col_nom <- c( 'Anio', 'Vejez','Invalidez','Discapacidad','Montepio','Total')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pensionistas_total_sal <-read_excel(file,sheet="Total_pen",col_names=TRUE,guess_max = 24000)
pensionistas_total_sal <- as.data.table( pensionistas_total_sal )
setnames( pensionistas_total_sal, col_nom )
message( '\tGuardando total' )


# Afiliados por sexo   -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAfiliados por sexo' )
col_nom <- c( 'Anio', 'Hombres','Mujeres','Total')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
afiliados_sexo_sal <-read_excel(file,sheet="Afiliado_sexo",col_names=TRUE,guess_max = 24000)
afiliados_sexo_sal <- as.data.table( afiliados_sexo_sal )
setnames( afiliados_sexo_sal, col_nom )
message( '\tGuardando total' )



# Afiliados historico por sexo   -------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAfiliados historico por sexo' )
col_nom <- c( 'Anio', 'Edad','Hombre','Mujer')
col_tip <- c( 'character', 'character',  'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
afiliados_hist_sexo_sal <-read_excel(file,sheet="Afiliados_hist_edad",col_names=TRUE, 
                                     guess_max = 24000)
afiliados_hist_sexo_sal <- as.data.table( afiliados_hist_sexo_sal )
setnames( afiliados_hist_sexo_sal, col_nom )
message( '\tGuardando total' )



# Mujeres en edad fertil   -------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tMujeres en edad fertil' )
col_nom <- c( 'Edad', '2013','2014','2015','2016','2017','2018')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
mujeres_fertil <-read_excel(file,sheet="Mujer_fertil",col_names=TRUE,guess_max = 24000)
mujeres_fertil <- as.data.table( mujeres_fertil )
setnames( mujeres_fertil, col_nom )
message( '\tGuardando total' )

# Poblacion ecuatoriana afiliaos, sexo, edad, año   ------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPoblacion ecuatoriana afiliaos, sexo, edad,' )
col_nom <- c( 'Anio','Edad','Personas','Sexo','Tipo')
col_tip <- c( 'character', 'numeric', 'numeric', 'character', 'character')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
sgo_sexo_edad_anio <-read_excel(file,sheet="SGO_edad_exo_anio",col_names=TRUE,guess_max = 24000)
sgo_sexo_edad_anio <- as.data.table( sgo_sexo_edad_anio )
setnames( sgo_sexo_edad_anio, col_nom )
message( '\tPoblacion ecuatoriana afiliaos, sexo, edad,' )

# Poblacion SGO 1978-2018   ------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPoblacion SGO 1978-2018' )
col_nom <- c( 'Anio','Afi_sgo')
col_tip <- c( 'character', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
afi_sgo_1978_2018 <-read_excel(file,sheet="Afi_sgo_histo",col_names=TRUE,guess_max = 24000)
afi_sgo_1978_2018 <- as.data.table( afi_sgo_1978_2018 )
setnames( afi_sgo_1978_2018, col_nom )
message( '\tPoblacion SGO 1978-2018' )

# Se guardan las tablas de datos demograficos  -----------------------------------------------------
save( afi_sgo_1978_2018, sgo_sexo_edad_anio,pob_afi_masa, poblacion_ecuatoriana_inec_sal, 
      pob_ecu_region_inec_sal, 
      natalidad_mortalidad_sal, afiliados_edad,pensionistas_vejez_sal,pensionistas_inva_sal,
      pensionistas_disc_sal,pensionistas_monte_sal,pensionistas_total_sal,afiliados_sexo_sal,
      afiliados_hist_sexo_sal,mujeres_fertil, 
      file = paste0( parametros$RData_seg, 'IESS_SAL_poblacion_ecuatoriana_afiliados.RData' ) )


# Pensionistas atendidos en unidades medicas   ----------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPensionistas atendidos en unidades medicas' )
col_nom <- c( 'Anio','Edad', 'Hombre','Mujer')
col_tip <- c( 'character','character', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pen_unida_me <-read_excel(file,sheet="Pen_unidad",col_names=TRUE,guess_max = 24000)
pen_unida_me <- as.data.table( pen_unida_me )
setnames( pen_unida_me, col_nom )
message( '\tGuardando total' )


# Pancientes del SGO por area   --------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPancientes del SGO por area ' )
col_nom <- c( 'Anio','Afiliados_ho', 'Atenciones_ho','Afiliados_co', 'Atenciones_co','Afiliados_em',
              'Atenciones_em')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pacientes_area <-read_excel(file,sheet="Pacientes_area",col_names=TRUE,guess_max = 24000)
pacientes_area <- as.data.table( pacientes_area )
setnames( pacientes_area, col_nom )
message( '\tGuardando Pancientes del SGO por area' )



# Atencion medica unidades externas   --------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAtencion medica unidades externas ' )
col_nom <- c( 'Tipo','Hombres', 'USDh','Mujeres', 'USDm')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
aten_med_acre <-read_excel(file,sheet="Aten_med",col_names=TRUE,guess_max = 24000)
aten_med_acre <- as.data.table( aten_med_acre )
setnames( aten_med_acre, col_nom )
message( '\tGuardando Atencion medica unidades externas' )


# Hsopitalizacion por la CIE10   -------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tHsopitalizacion por la CIE10 ' )
col_nom <- c( 'CIE10','2012', '2013','2014', '2015','2016','2017')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
hospi_cie <-read_excel(file,sheet="Aten_cie",col_names=TRUE,guess_max = 24000)
hospi_cie <- as.data.table( hospi_cie )
setnames( hospi_cie, col_nom )
message( '\tGuardando Hsopitalizacion por la CIE10' )

# Hospitalizacion IESS 2012-2017   -----------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tHospitalizacion IESS 2012-2017 ' )
col_nom <- c( 'Anio','Edad','Personas','Sexo','Tipo','Rango')
col_tip <- c( 'character', 'numeric', 'numeric','character','character','character')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
hospi_sexo_edad_2012_2017 <-read_excel(file,sheet="Piramides_hospitaliza",col_names=TRUE, 
                                       guess_max = 24000)
hospi_sexo_edad_2012_2017 <- as.data.table( hospi_sexo_edad_2012_2017 )
setnames( hospi_sexo_edad_2012_2017, col_nom )
message( '\tHospitalizacion IESS 2012-2017' )


# Se cargar los datos de hospitalizacion  ----------------------------------------------------------
save( hospi_sexo_edad_2012_2017, pen_unida_me,pacientes_area,aten_med_acre,hospi_cie, 
      file = paste0( parametros$RData_seg, 'IESS_SAL_hospitalizacion_CIE10.RData' ) )


# Tasa mortalidad por la CIE10   -------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tTasa mortalidad por la CIE10 ' )
col_nom <- c( 'CIE10','2012', '2013','2014', '2015','2016','2017')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
tasa_mor_cie <-read_excel(file,sheet="Tasa_mor_cie",col_names=TRUE,guess_max = 24000)
tasa_mor_cie <- as.data.table( tasa_mor_cie )
setnames( tasa_mor_cie, col_nom )
message( '\tGuardando Tasa mortalidad por la CIE10' )



# Defuncion de pasientes    ------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tDefuncion de pasientes ' )
col_nom <- c( 'Edad','Defuncionesh', 'Altash','Defuncionesm', 'Altasm','Total_egreso','Egreso', 
              'Muertes')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
def_paci_hos <-read_excel(file,sheet="Defuncion",col_names=TRUE,guess_max = 24000)
def_paci_hos <- as.data.table( def_paci_hos )
setnames( def_paci_hos, col_nom )
message( '\tGuardando Defuncion de pasientes' )



# Tasa de pacientes    -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tTasa de pacientes ' )
col_nom <- c( 'CIE10','2012', '2013','2014', '2015','2016','2017')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
tasa_uni_cie <-read_excel(file,sheet="Tasa_uni_cie",col_names=TRUE,guess_max = 24000)
tasa_uni_cie <- as.data.table( tasa_uni_cie )
setnames( tasa_uni_cie, col_nom )
message( '\tGuardando Tasa de pacientes' )



# Egreso de pacientes    ---------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tEgreso de pacientes ' )
col_nom <- c( 'Anio','Provincia', 'Hombres','Mujeres', 'Total')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
egreso_paci <-read_excel(file,sheet="Egreso_pa",col_names=TRUE,guess_max = 24000)
egreso_paci <- as.data.table( egreso_paci )
setnames( egreso_paci, col_nom )
message( '\tGuardando Egreso de pacientes' )



# Tasa bruta mortalidad    -------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tTasa bruta mortalidad ' )
col_nom <- c( 'Anio','Provincia', 'Egresos','Altas_vivos', 'Falle_me48','Falle_mas48', 
              'Total_fallecidos','Tasa_neta_mor','Tasa_bruta_mor')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
tasa_net_bru <-read_excel(file,sheet="Tas_neta_brut",col_names=TRUE,guess_max = 24000)
tasa_net_bru <- as.data.table( tasa_net_bru )
setnames( tasa_net_bru, col_nom )
message( '\tGuardando Tasa bruta mortalidad' )



# Esperanza de vida    -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tEsperanza de vida ' )
col_nom <- c( 'Sexo','x', 'lx','nlx', 'ex')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
esperanza_hm <-read_excel(file,sheet="Espe_h_m",col_names=TRUE,guess_max = 24000)
esperanza_hm <- as.data.table( esperanza_hm )
setnames( esperanza_hm, col_nom )
message( '\tGuardando Esperanza de vida' )



# Se cargan los datos de mortalidad pacientes ------------------------------------------------------
save( tasa_mor_cie,def_paci_hos,tasa_uni_cie,egreso_paci,tasa_net_bru,esperanza_hm,
      file = paste0( parametros$RData_seg, 'IESS_SAL_mortalidad_pacientes.RData' ) )



# Indicadores demograficos   -----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tIndicadores demograficos ' )
col_nom <- c( 'Anio','sex', 'h','m', '14','64','65','dep','med','ra','vi','ni','fer')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
indica_demo <-read_excel(file,sheet="Indi_demo",col_names=TRUE,guess_max = 24000)
indica_demo <- as.data.table( indica_demo )
setnames( indica_demo, col_nom )
message( '\tIndicadores demograficos' )


# Poblacion cubierta   -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPoblacion cubierta ' )
col_nom <- c( 'Tipo','2013', '2014','2015', '2016','2017','2018')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pob_cubierta <-read_excel(file,sheet="Pob_cub",col_names=TRUE,guess_max = 24000)
pob_cubierta <- as.data.table( pob_cubierta )
setnames( pob_cubierta, col_nom )
message( '\tPoblacion cubierta' )

# Poblacion atendida   -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPoblacion atendida ' )
col_nom <- c( 'Tipo','2013', '2014','2015', '2016','2017','2018')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pob_atend <-read_excel(file,sheet="Pob_atend",col_names=TRUE,guess_max = 24000)
pob_atend <- as.data.table( pob_atend )
setnames( pob_atend, col_nom )
message( '\tPoblacion atendida' )



# Porcentaje de uso   ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPorcentaje de uso ' )
col_nom <- c( 'Tipo','a2013', 'a2014','a2015', 'a2016','a2017','a2018')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
por_uso <-read_excel(file,sheet="Por_uso",col_names=TRUE,guess_max = 24000)
por_uso <- as.data.table( por_uso )
setnames( por_uso, col_nom )
message( '\tPorcentaje de uso' )


# Afi y pen atendidos   ----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAfi pen atendidos ' )
col_nom <- c( 'Anio','Servicio', 'Pacientes','Atenciones', 'Media')
col_tip <- c( 'character', 'character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
afipen_aten <-read_excel(file,sheet="Afi_pen",col_names=TRUE,guess_max = 24000)
afipen_aten <- as.data.table( afipen_aten )
setnames( afipen_aten, col_nom )
message( '\tAfi pen atendidos' )



# Atenciones afiliados SGO   -----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAtenciones afiliados SGO ' )
col_nom <- c( 'Anio','Edad', 'Feme','Mascu')
col_tip <- c( 'character', 'character', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
aten_afisgo <-read_excel(file,sheet="Aten_afisgo",col_names=TRUE,guess_max = 24000)
aten_afisgo <- as.data.table( aten_afisgo )
setnames( aten_afisgo, col_nom )
message( '\tAtenciones afiliados SGO' )



# Atenciones pensionistas SGO   --------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAtenciones pensionistas SGO ' )
col_nom <- c( 'Anio','Edad', 'Feme','Mascu')
col_tip <- c( 'character', 'character', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
aten_pensgo <-read_excel(file,sheet="Aten_pensgo",col_names=TRUE,guess_max = 24000)
aten_pensgo <- as.data.table( aten_pensgo )
setnames( aten_pensgo, col_nom )
message( '\tAtenciones pensionistas SGO' )


# Se guarda la poblacion cubierta
save( indica_demo,pob_cubierta,pob_atend,por_uso,afipen_aten,aten_afisgo,aten_pensgo, 
      file = paste0( parametros$RData_seg, 'IESS_SAL_poblacion_cubierta.RData' ) )


# facturación de unidades médicas   ----------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tfacturación de unidades médicas ' )
col_nom <- c( 'Rubro','a2013', 'a2014','a2015','a2016','a2017','a2018','total','porcentaj')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
factu_uni <-read_excel(file,sheet="Factu_uni",col_names=TRUE,guess_max = 24000)
factu_uni <- as.data.table( factu_uni )
setnames( factu_uni, col_nom )
message( '\tfacturación de unidades médicas' )



# Facturación promedio unidades médicas   ----------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación promedio unidades médicas ' )
col_nom <- c( 'Area','Nivel','a2013', 'a2014','a2015','a2016','a2017','a2018')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
factu_pro <-read_excel(file,sheet="Factu_pro",col_names=TRUE,guess_max = 24000)
factu_pro <- as.data.table( factu_pro )
setnames( factu_pro, col_nom )
message( '\tFacturación promedio unidades médicas' )



# Facturación Externos   ---------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación Externos ' )
col_nom <- c( 'Area','Nivel','a2012','a2013', 'a2014','a2015','a2016','a2017','a2018')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
factu_ext <-read_excel(file,sheet="Factu_ext",col_names=TRUE,guess_max = 24000)
factu_ext <- as.data.table( factu_ext )
setnames( factu_ext, col_nom )
message( '\tFacturación Externos' )


# Facturacion unidades medicas
save( factu_uni,factu_pro,factu_ext,
      file = paste0( parametros$RData_seg, 'IESS_SAL_facturacion_medica.RData' ) )


# Capacidad instalada unidades   -------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCapacidad instalada unidades ' )
col_nom <- c( 'Anio','camas_dot','camas_dis','dias_cama','dias_est','falle_menos','falle_mas', 
              'egresos','mortalidad')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
cap_ins <-read_excel(file,sheet="Capa_ins",col_names=TRUE,guess_max = 24000)
cap_ins <- as.data.table( cap_ins )
setnames( cap_ins, col_nom )
message( '\tCapacidad instalada unidades' )



# Edad pacientes hospitalizadops  ------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tEdad pacientes hospitalizadops ' )
col_nom <- c( 'Tipo','Media','DS','Numero','Porcentaje')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
edad_pa_hos <-read_excel(file,sheet="Edad_pa",col_names=TRUE,guess_max = 24000)
edad_pa_hos <- as.data.table( edad_pa_hos )
setnames( edad_pa_hos, col_nom )
message( '\tEdad pacientes hospitalizadops' )


# Diez causas mujeres y hombres --------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tDiez causas mujeres y hombres ' )
col_nom <- c( 'Sexo','CozCIE10','Diagn_CIE10','a2013','a2014','a2015','a2016','a2017','a2018')
col_tip <- c( 'character','character','character', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
diez_morb_muj <-read_excel(file,sheet="Diez_muj",col_names=TRUE,guess_max = 24000)
diez_morb_muj <- as.data.table( diez_morb_muj )
setnames( diez_morb_muj, col_nom )
message( '\tDiez causas mujeres y hombres' )


# Vida Potenciales perdidos ------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tVida Potenciales perdidos ' )
col_nom <- c('Cap_CIE10','AVPP_h','TASA_AVPP_h','AVPP_m','TASA_AVPP_m','AVPP_t','TASA_AVPP_t')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
poten_per <-read_excel(file,sheet="Potenciales",col_names=TRUE,guess_max = 24000)
poten_per <- as.data.table( poten_per )
setnames( poten_per, col_nom )
message( '\tVida Potenciales perdidos' )


# Esperanza de vida pacientes ----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tEsperanza de vida pacientes ' )
col_nom <- c('x','dxxn','pxxn','nmx','nqx','npx','lx','ndx','nlx','tx','ex')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
espe_paci <-read_excel(file,sheet="Espe_paci",col_names=TRUE,guess_max = 24000)
espe_paci <- as.data.table( espe_paci )
setnames( espe_paci, col_nom )
message( '\tEsperanza de vida pacientes' )



# Subsidios ----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tSubsidios ' )
col_nom <- c('Subsidios','No_muj','USD_muj','No_hom','USD_hom','Total_afi','Total_USD')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
subsidios_afi <-read_excel(file,sheet="Subsidio",col_names=TRUE,guess_max = 24000)
subsidios_afi <- as.data.table( subsidios_afi )
setnames( subsidios_afi, col_nom )
message( '\tSubsidios' )


# Carga capacidad de unidades medicas
save( cap_ins,edad_pa_hos,diez_morb_muj,poten_per,espe_paci,subsidios_afi,
      file = paste0( parametros$RData_seg, 'IESS_SAL_capacidad_instalaciones.RData' ) )



# Infarto agudo de miocardio  ----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tInfarto agudo de miocardio ' )
col_nom <- c('Anio','Naf','Aten_a','Fact_a','Np','Aten_p','Fact_p','Total_fact')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
infarto_agudo <-read_excel(file,sheet="Infarto_agu",col_names=TRUE,guess_max = 24000)
infarto_agudo <- as.data.table( infarto_agudo )
setnames( infarto_agudo, col_nom )
message( '\tInfarto agudo de miocardio ' )


# Hemofilia  ---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tHemofilia' )
col_nom <- c('Anio','Naf','Aten_a','Fact_a','Np','Aten_p','Fact_p','Total_fact')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
hemofilia <-read_excel(file,sheet="Hemofilia",col_names=TRUE,guess_max = 24000)
hemofilia <- as.data.table( hemofilia )
setnames( hemofilia, col_nom )
message( '\tHemofilia ' )



# Insuficiencia renal cronica  ---------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tInsuficiencia renal cronica' )
col_nom <- c('Anio','Naf','Aten_a','Fact_a','Np','Aten_p','Fact_p','Total_fact')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
insuficien_renal <-read_excel(file,sheet="Insuficien_renal",col_names=TRUE,guess_max = 24000)
insuficien_renal <- as.data.table( insuficien_renal )
setnames( insuficien_renal, col_nom )
message( '\tInsuficiencia renal cronica ' )


# Infeccion adquirida ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tInfeccion adquirida' )
col_nom <- c('Anio','Naf','Aten_a','Fact_a','Np','Aten_p','Fact_p','Total_fact')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
infeccion_adquirida <-read_excel(file,sheet="Infeccion",col_names=TRUE,guess_max = 24000)
infeccion_adquirida <- as.data.table( infeccion_adquirida )
setnames( infeccion_adquirida, col_nom )
message( '\tInfeccion adquirida ' )


# Quimioterapia ------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tQuimioterapia' )
col_nom <- c('Anio','Nume','USD','Promedio_USD')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
quimioterapia <-read_excel(file,sheet="Quimioterapia",col_names=TRUE,guess_max = 24000)
quimioterapia <- as.data.table( quimioterapia )
setnames( quimioterapia, col_nom )
message( '\tQuimioterapia ' )


# Trasplantes --------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tTrasplantes' )
col_nom <- c('Anio','Nume','USD','Promedio_USD')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
trasplantes <-read_excel(file,sheet="Trasplantes",col_names=TRUE,guess_max = 24000)
trasplantes <- as.data.table( trasplantes )
setnames( trasplantes, col_nom )
message( '\tTrasplantes ' )


# Valores promedio  --------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tValores promedio' )
col_nom <- c('Anio','IAM','Hemofilia','IRC','VIH')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
costo_promedio_enfermedad <-read_excel(file,sheet="Valor_promed",col_names=TRUE,guess_max = 24000)
costo_promedio_enfermedad <- as.data.table( costo_promedio_enfermedad )
setnames( costo_promedio_enfermedad, col_nom )
message( '\tValores promedio ' )


# Enfermedades de alto costo
save( costo_promedio_enfermedad,trasplantes,quimioterapia,infeccion_adquirida,insuficien_renal,
      hemofilia,infarto_agudo,
      file = paste0( parametros$RData_seg, 'IESS_SAL_enfermedades_alto_costo.RData' ) )


# Maternidad interno y externo ---------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tMaternidad interno externo' )
col_nom <- c('Anio','Pacientes_parto','USD_parto','Paci_cesarea','USD_cesarea')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
maternidad_interno_externo <-read_excel(file,sheet="Maternidad_1",col_names=TRUE,guess_max = 24000)
maternidad_interno_externo <- as.data.table( maternidad_interno_externo )
setnames( maternidad_interno_externo, col_nom )
message( '\tMaternidad interno externo ' )


# Maternidad unidades internas ---------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tMaternidad unidades internas' )
col_nom <- c('Anio','Pacientes_parto','USD_parto','Paci_cesarea','USD_cesarea')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
maternidad_interno <-read_excel(file,sheet="Maternidad_2",col_names=TRUE,guess_max = 24000)
maternidad_interno <- as.data.table( maternidad_interno )
setnames( maternidad_interno, col_nom )
message( '\tMaternidad unidades internas ' )



# Maternidad unidades externas ---------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tMaternidad unidades externas' )
col_nom <- c('Anio','Pacientes_parto','USD_parto','Paci_cesarea','USD_cesarea')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
maternidad_externas <-read_excel(file,sheet="Maternidad_3",col_names=TRUE,guess_max = 24000)
maternidad_externas <- as.data.table( maternidad_externas )
setnames( maternidad_externas, col_nom )
message( '\tMaternidad unidades externas ' )


# Porcentaje de cesareas ---------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPorcentaje de cesareas' )
col_nom <- c('Anio','Red_interna','Red_externa')
col_tip <- c( 'character', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
porcentaje_cesareas <-read_excel(file,sheet="Cesareas",col_names=TRUE,guess_max = 24000)
porcentaje_cesareas <- as.data.table( porcentaje_cesareas )
setnames( porcentaje_cesareas, col_nom )
message( '\tPorcentaje de cesareas ' )




# Maternidad   -------------------------------------------------------------------------------------
save( porcentaje_cesareas,maternidad_externas,maternidad_interno,maternidad_interno_externo, 
      file = paste0( parametros$RData_seg, 'IESS_SAL_datos_maternidad.RData' ) )



# Facturación total, internos y externos -----------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación total' )
col_nom <- c('Cuadro','Anio','Afiliados','Pensionistas','Totales')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
facturacion_sgsif <-read_excel(file,sheet="Facturacion_ttal",col_names=TRUE,guess_max = 24000)
facturacion_sgsif <- as.data.table( facturacion_sgsif )
setnames( facturacion_sgsif, col_nom )
message( '\tFacturación total ' )


# Facturación extension de cobertura ---------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación extension de cobertura' )
col_nom <- c('Cuadro','Anio','N_cob','Total_cob','N_menores','Total_menores','Total_general')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
facturacion_extension <-read_excel(file,sheet="Facturacion_extens",col_names=TRUE, 
                                   guess_max = 24000)
facturacion_extension <- as.data.table( facturacion_extension )
setnames( facturacion_extension, col_nom )
message( '\tFacturación extension de cobertura ' )


# Deudas internas y externas -----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tDeudas internas y externas' )
col_nom <- c('Cuadro','Anio','N_ssc','Total_ssc','N_msp','Total_msp','N_rt','Total_rt','N_ci', 
             'Total_ci','N_issfa','Total_issfa','Deuda_total')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
deudas_medicas <-read_excel(file,sheet="Deudas_int",col_names=TRUE,guess_max = 24000)
deudas_medicas <- as.data.table( deudas_medicas )
setnames( deudas_medicas, col_nom )
message( '\tDeudas internas y externas ' )


# Costo total de atenciones ------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCosto total de atenciones' )
col_nom <- c('Anios','Afi_pen','Exten','Menores','Otros','Total_ge','Errores','Valor_final')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
costo_final <-read_excel(file,sheet="Costo_final",col_names=TRUE,guess_max = 24000)
costo_final <- as.data.table( costo_final )
setnames( costo_final, col_nom )
message( '\tCosto total de atenciones ' )


# Egresos proyectados ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tEgresos proyectados' )
col_nom <- c('Anio','Afi','Pen','Total')
col_tip <- c( 'character','numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
egresos_proyectados <-read_excel(file,sheet="Egresos_proyect",col_names=TRUE,guess_max = 24000)
egresos_proyectados <- as.data.table( egresos_proyectados )
setnames( egresos_proyectados, col_nom )
message( '\tEgresos proyectados ' )


# Variacion porcentual -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tVariacion porcentual' )
col_nom <- c('Cuadro','Anio','Afiliados','Variacion','Costo','Var_costo')
col_tip <- c( 'character','character','numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
variacion_atendidos <-read_excel(file,sheet="Variciones",col_names=TRUE,guess_max = 24000)
variacion_atendidos <- as.data.table( variacion_atendidos )
setnames( variacion_atendidos, col_nom )
message( '\tVariacion porcentual ' )


# Costo de atenación médica ------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCosto de atenación médica ' )
col_nom <- c('Anio','Afi_pen','Extend', 'menores', 'otros', 'total_ge', 'errores', 'valor_final')
col_tip <- c( 'character','numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
costo_atencion_medica <-read_excel(file,sheet="Costo_final",col_names=TRUE,guess_max = 24000)
costo_atencion_medica <- as.data.table( costo_atencion_medica )
setnames( costo_atencion_medica, col_nom )
message( '\tCosto de atenación médica' )


# Atenciones Salud red Interna----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAtenciones Salud red Interna' )
col_nom <- c('tipo', 'proceso', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
atenciones_red_interna <-read_excel(file,sheet="Atenciones_salud_red_interna",col_names=TRUE, 
                                    guess_max = 24000)
atenciones_red_interna <- as.data.table( atenciones_red_interna )
setnames( atenciones_red_interna, col_nom )
message( '\tAtenciones Salud red Interna' )


# Atenciones Salud red Externa----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAtenciones Salud red Externa' )
col_nom <- c('tipo', 'proceso', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
atenciones_red_externa <-read_excel(file,sheet="Atenciones_salud_red_externa",col_names=TRUE, 
                                    guess_max = 24000)
atenciones_red_externa <- as.data.table( atenciones_red_externa )
setnames( atenciones_red_externa, col_nom )
message( '\tAtenciones Salud red Externa' )


# Atenciones Salud red Externa----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAtenciones Salud red Interna y Externa' )
col_nom <- c('tipo', 'proceso', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6')
col_tip <- c( 'character','character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
aten_red_int_ext <-read_excel(file,sheet="Atenciones_red_interna_externa"
                                    ,col_names=TRUE,guess_max = 24000)
aten_red_int_ext <- as.data.table( aten_red_int_ext )
setnames( aten_red_int_ext, col_nom )
message( '\tAtenciones Salud red Interna y Externa' )



# Facturación proyectada de  por prestaciones médicas-----------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación proyectada por prestaciones médicas' )
col_nom <- c('anio', 'afi' ,'pen', 'total' )
col_tip <- c( 'character','numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
fact_proy_prest_med <-read_excel(file,sheet="Fact_proy_prestaciones_medicas"
                              ,col_names=TRUE,guess_max = 24000)
fact_proy_prest_med <- as.data.table( fact_proy_prest_med )
setnames( fact_proy_prest_med, col_nom )
message( '\tFacturación proyectada por prestaciones médicas' )


# Facturación por la cobertura de salud interna y externa-------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación por la cobertura de salud de las unidades internas y externas del IESS' )
col_nom <- c('anio', 'pac1', 'uds1', 'pac2', 'uds2', 'pac3', 'uds3' )
col_tip <- c( 'character','numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
fact_cob_int_ext <-read_excel(file,sheet="Fact_cob_unid_inte_ext"
                                 ,col_names=TRUE,guess_max = 24000)
fact_cob_int_ext <- as.data.table( fact_cob_int_ext )
setnames( fact_cob_int_ext, col_nom )
message( '\tFacturación por la cobertura de salud de las unidades internas y externas del IESS' )


# Facturación por la cobertura de salud interna y externa-------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación por atenciones médicas' )
col_nom <- c('tipo', 'anio', 'pac1', 'uds1', 'pac2', 'uds2', 'pac3', 'uds3' )
col_tip <- c( 'character', 'character','numeric', 'numeric', 'numeric','numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
fact_aten_med <-read_excel(file,sheet="Fac_aten_medi"
                              ,col_names=TRUE,guess_max = 24000)
fact_aten_med <- as.data.table( fact_aten_med )
setnames( fact_aten_med, col_nom )
message( '\tFacturación por atenciones médicas' )


# Facturación por atenciones médicas menores y extensión de cobertura-------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación por atenciones médicas menores y extensión de cobertura' )
col_nom <- c('tipo', 'anio', 'pac', 'uds' )
col_tip <- c( 'character', 'character','numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
fact_aten_med_otros <-read_excel(file,sheet="Fact_aten_med_otros"
                           ,col_names=TRUE,guess_max = 24000)
fact_aten_med_otros <- as.data.table( fact_aten_med_otros )
setnames( fact_aten_med_otros, col_nom )
message( '\tFacturación por atenciones médicas menores y extensión de cobertura' )


# Costo promedio de atenciones médcias--------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCosto promedio de atenciones' )
col_nom <- c('tipo', 'anio', 'soam', 'prosick', 'as400' )
col_tip <- c( 'character', 'character','numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
costo_prom_aten_med <-read_excel(file,sheet="costo_prom_aten_med"
                                 ,col_names=TRUE,guess_max = 24000)
costo_prom_aten_med <- as.data.table( costo_prom_aten_med )
setnames( costo_prom_aten_med, col_nom )
message( '\tCosto promedio de atenciones' )



# Total de facturación por atenciones médicas por provincia-----------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tTotal de facturación por atenciones médicas por provincia' )
col_nom <- c('provincia', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7' )
col_tip <- c( 'character','numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
tot_fact_aten_med_prov_int_ext <-read_excel(file,sheet="tot_fact_aten_med_prov_int_ext"
                                 ,col_names=TRUE,guess_max = 24000)
tot_fact_aten_med_prov_int_ext <- as.data.table( tot_fact_aten_med_prov_int_ext )
setnames( tot_fact_aten_med_prov_int_ext, col_nom )
message( '\tTotal de facturación por atenciones médicas por provincia' )



# Afiliados y pensionistas atendidos por unidad médica----------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAfiliados y pensionistas atendidos por unidad médica' )
col_nom <- c('tipo', 'proceso', 'A1', 'P1', 'T1', 'A2', 'P2', 'T2', 'A3', 'P3', 'T3' )
col_tip <- c( 'character', 'character', 'numeric', 'numeric', 'numeric','numeric', 'numeric'
              , 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
afi_pen_aten_unidad_med <-read_excel(file,sheet="afi_pen_aten_unidad_med"
                                            ,col_names=TRUE,guess_max = 24000)
afi_pen_aten_unidad_med <- as.data.table( afi_pen_aten_unidad_med )
setnames( afi_pen_aten_unidad_med, col_nom )
message( '\tAfiliados y pensionistas atendidos por unidad médica' )



# Costos de atención médica en el IESS ambos sexos--------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCostos de atención médica en el IESS ambos sexos' )
col_nom <- c('tipo', 'area', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6' )
col_tip <- c( 'character', 'character', 'numeric', 'numeric', 'numeric','numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
costo_aten_med_cob <-read_excel(file,sheet="costo_aten_med_cob"
                                     ,col_names=TRUE,guess_max = 24000)
costo_aten_med_cob <- as.data.table( costo_aten_med_cob )
setnames( costo_aten_med_cob, col_nom )
message( '\tCostos de atención médica en el IESS ambos sexos' )


# Subsidios entregados a ambos sexos----------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tSubsidios entregados a ambos sexos' )
col_nom <- c('tipo', 'anio', 'N1', 'V1', 'N2', 'V2', 'N3', 'V3'  )
col_tip <- c( 'character', 'character', 'numeric', 'numeric', 'numeric','numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
subsidios_entregados <-read_excel(file,sheet="subsidios_entregados"
                                ,col_names=TRUE,guess_max = 24000)
subsidios_entregados <- as.data.table( subsidios_entregados )
setnames( subsidios_entregados, col_nom )
message( '\tSubsidios entregados a ambos sexos' )



# Pacientes por edad y sexo atendidos --------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPacientes por edad y sexo atendidos' )
col_nom <- c('edad', 'h1', 'm1', 'h2', 'm2', 'h3', 'm3', 'h4', 'm4', 'h5', 'm5', 'h6', 'm6')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric'
              , 'numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
pac_aten_unidad_medica_edad_sex <-read_excel(file,sheet="pac_aten_unidad_medica_edad_sex"
                                  ,col_names=TRUE,guess_max = 24000)
pac_aten_unidad_medica_edad_sex <- as.data.table( pac_aten_unidad_medica_edad_sex )
setnames( pac_aten_unidad_medica_edad_sex, col_nom )
message( '\tPacientes por edad y sexo atendidos' )


# Total atendidos en el IESS -----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tTotal atendidos en el IESS' )
col_nom <- c('tipo','anio', 'hombres', 'mujeres', 'total')
col_tip <- c( 'character', 'character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
total_atendidos <-read_excel(file,sheet="macro_tot_afi_pen_aten_med"
                                             ,col_names=TRUE,guess_max = 24000)
total_atendidos <- as.data.table( total_atendidos )
setnames( total_atendidos, col_nom )
message( '\tTotal atendidos en el IESS' )



# Rubros de Facturación ----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tRubros de Facturación' )
col_nom <- c('tipo','rubro', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'total')
col_tip <- c( 'character', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
              'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
rubros_facturacion <-read_excel(file,sheet="rubros_facturacion"
                             ,col_names=TRUE,guess_max = 24000)
rubros_facturacion <- as.data.table( rubros_facturacion )
setnames( rubros_facturacion, col_nom )
message( '\tRubros de Facturación' )


# Facturación por tipo de enfermedad----------------------------------------------------------------

message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación por tipo de enfermedad' )
col_nom <- c('tipo','anio','N', 'usd')
col_tip <- c( 'character', 'character', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
facturacion_enfermedad <-read_excel(file,sheet="fact_tipo_enfer"
                                ,col_names=TRUE,guess_max = 24000)
facturacion_enfermedad <- as.data.table( facturacion_enfermedad )
setnames( facturacion_enfermedad, col_nom )
message( '\tFacturación por tipo de enfermedad' )


# Facturación por tipo de afiliado------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tFacturación por tipo de afiliado' )
col_nom <- c('tipo','A1', 'A2', 'A3', 'A4', 'A5', 'total')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
fact_tipo_afi <-read_excel(file,sheet="fact_tipo_afiliado"
                                    ,col_names=TRUE,guess_max = 24000)
fact_tipo_afi <- as.data.table( fact_tipo_afi )
setnames( fact_tipo_afi, col_nom )
message( '\tFacturación por tipo de afiliado' )



# Compra de servicios médicos-----------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCompra de servicios médicos' )
col_nom <- c('provincia', 'AMB', 'CI', 'DIA', 'EMER', 'HOSP', 'TOTAL')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
compra_serv_medi <-read_excel(file,sheet="compra_serv_medicos_ext"
                           ,col_names=TRUE,guess_max = 24000)
compra_serv_medi <- as.data.table( compra_serv_medi )
setnames( compra_serv_medi, col_nom )
message( '\tCompra de servicios médicos' )


# Subsidios pagados---------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tSubsidios pagados' )
col_nom <- c('tipo', 'N1', 'usd1', 'prom1', 'N2', 'usd2', 'prom2', 'N3', 'usd3', 'prom3')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'
              , 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
subsidios_pagados <-read_excel(file,sheet="subsidios_pagados"
                              ,col_names=TRUE,guess_max = 24000)
subsidios_pagados <- as.data.table( subsidios_pagados )
setnames( subsidios_pagados, col_nom )
message( '\tSubsidios pagados' )


# Costo total de la atención médica al universo-----------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCosto total de la atención médica al universo' )
col_nom <- c('anio', 'p1', 'p2', 'costo', 'aportes', 'deficit')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
costo_total_universal <-read_excel(file,sheet="costo_total_universal"
                               ,col_names=TRUE,guess_max = 24000)
costo_total_universal <- as.data.table( costo_total_universal )
setnames( costo_total_universal, col_nom )
message( '\tCosto total de la atención médica al universo' )

#UNIDADES MÉDICAS DEL IESS -----------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tUNIDADES MÉDICAS DEL IESS ' )
col_nom <- c('n', 'nivel', 'unidad', 'provincia', 'ciudad')
col_tip <- c( 'character', 'character', 'character', 'character', 'character')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
unidades_medicas <-read_excel(file,sheet="Unidades_medicas"
                                   ,col_names=TRUE,guess_max = 24000)
unidades_medicas <- as.data.table( unidades_medicas )
setnames( unidades_medicas, col_nom )

#MAPAS-----------------------------------------------------
message( '\tLeyendo Datos para la generación de Mapas' )
col_nom <- c(  'Nom', 'HASC_1', 'Provincia', 'Porc_resi', 'Porc_ubi', 'morbilidad')
col_tip <- c(  'character', 'character', 'character', 'numeric', 'numeric', 'numeric')
file <- paste0( parametros$Data_seg, 'IESS_SAL_entradas.xlsx' )
map_egresos <-read_excel(file,sheet="map_egresos_hospitalarios"
                            ,col_names=TRUE,guess_max = 24000)
map_egresos <- as.data.table( map_egresos )
setnames( map_egresos, col_nom )

save(map_egresos,file = paste0( parametros$RData_seg, 
                                              'IESS_SAL_datos_mapas.RData' ) )
# Evolucion de las prestaciones  -------------------------------------------------------------------
save( unidades_medicas,costo_total_universal,subsidios_pagados,compra_serv_medi,fact_tipo_afi, 
      facturacion_enfermedad,rubros_facturacion,total_atendidos,pac_aten_unidad_medica_edad_sex, 
      subsidios_entregados
      ,costo_aten_med_cob,afi_pen_aten_unidad_med,tot_fact_aten_med_prov_int_ext, 
      costo_prom_aten_med
      ,fact_aten_med_otros,fact_aten_med,fact_cob_int_ext,fact_proy_prest_med,aten_red_int_ext, 
      atenciones_red_externa , atenciones_red_interna, costo_atencion_medica 
      ,variacion_atendidos,egresos_proyectados,costo_final,deudas_medicas,facturacion_extension
      ,facturacion_sgsif, file = paste0( parametros$RData_seg, 
                                         'IESS_SAL_evolucion_prestaciones.RData' ) )

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()



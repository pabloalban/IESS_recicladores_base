message( paste( rep( '-', 100 ), collapse = '' ) )

# Cargando información -----------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_pob_proy )
load( file = parametros$sal_rdata_est_sev )
load( file = esc$rdata_sal_proy )
load( file = esc$rdata_ben_proy )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% parametros_lista ) ] )
if ( exists( 'balance' ) ) rm( balance )
if ( exists( 'balance_anual' ) ) rm( balance_anual )

message( '\tBalance seguro ', parametros$seguro, ' calculando escenario: ', esc$nombre )

# Cruce con proyecciones ---------------------------------------------------------------------------
message( '\tCruce con proyecciones financieras' )
aportes <- pob_proy_ts[ t <= parametros$sal_horizonte ]

aportes <- merge.data.table(
  aportes, 
  sal_proy[ , list( t, sexo, x, s, sal ) ], 
  by = c( 't', 'sexo', 'x', 's' ),
  all.x = TRUE )

aportes <- merge.data.table( 
  aportes, 
  ben_proy[ , list( 
    t, sexo, x, s,
    pen_ant_4, pen_4, 
    pen_ant_5, pen_5, 
    pen_ant_7, pen_7_4, pen_7_5, 
    pen_ant_8, pen_8_4, pen_8_5 ) ], 
  by = c( 't', 'sexo', 'x', 's' ), 
  all.x = TRUE )

aportes[ is.na( sal ), sal := 0 ]
aportes[ is.na( pen_4 ), pen_4 := 0 ]
aportes[ is.na( pen_5 ), pen_5 := 0 ]
aportes[ is.na( pen_7_4 ), pen_7_4 := 0 ]
aportes[ is.na( pen_7_5 ), pen_7_5 := 0 ]
aportes[ is.na( pen_8_4 ), pen_8_4 := 0 ]
aportes[ is.na( pen_8_5 ), pen_8_5 := 0 ]
aportes[ is.na( pen_ant_4 ), pen_ant_4 := 0 ]
aportes[ is.na( pen_ant_5 ), pen_ant_5 := 0 ]
aportes[ is.na( pen_ant_7 ), pen_ant_7 := 0 ]
aportes[ is.na( pen_ant_8 ), pen_ant_8 := 0 ]

aportes <- merge.data.table(
  aportes,
  esc$apo_act[ , list( 
    t, i_a, u_m, sbu, 
    por_apo_2, por_apo_4, por_apo_5, por_apo_7, por_apo_8, por_apo_9, por_apo_11,
    por_apo_ext_2, por_apo_ext_4, por_apo_ext_5, por_apo_ext_7, por_apo_ext_8, 
    por_apo_gas,
    cal_mas, cal_pen_vej, cal_pen_inv, cal_pen_viu, cal_pen_orf, cal_gast ) ],
  by = c( 't' ), 
  all.x = TRUE )

setorder( aportes, t, sexo, x, s )

message( '\tProyectando masa salarial' )
if ( esc$mas_sal_con_dec ) {  # incluye los décimos en la masa salarial
  aportes[ , M := cal_mas * ( 13 * sal / 12 + sbu ) * l2 ]
} else {
  aportes[ , M := cal_mas * sal * l2 ]
}

# Proyección de beneficios -------------------------------------------------------------------------
message( '\tProyectando beneficios por pensiones' )

## Beneficio por pensiones de vejez ----------------------------------------------------------------
aportes[ , P4_nodec := cal_pen_vej * ( 12 / 13 ) * ( ( pen_ant_4 - sbu ) * l4_4 + ( pen_4 - sbu ) * ( l1_4 + l2_4 + l3_4 + l5_4 ) ) ]
aportes[ t == 0, P4_nodec := 0 ]
aportes[ , P4_ant := cal_pen_vej * pen_ant_4 * l4_4 ]
aportes[ t == 0, P4_ant := 0 ]
aportes[ , P4_nue := cal_pen_vej * pen_4 * ( l1_4 + l2_4 + l3_4 + l5_4 ) ]
aportes[ t == 0, P4_nue := 0 ]
aportes[ , P4 := P4_ant + P4_nue ]
aportes[ t == 0, P4 := 0 ]

## Beneficio por pensiones de invalidez ------------------------------------------------------------
aportes[ , P5_nodec := cal_pen_inv * ( 12 / 13 ) * ( ( pen_ant_5 - sbu ) * l5_5 + ( pen_5 - sbu ) * ( l1_5 + l2_5 + l3_5 + l4_5 ) ) ]
aportes[ t == 0, P5_nodec := 0 ]
aportes[ , P5_ant := cal_pen_inv * pen_ant_5 * l5_5 ]
aportes[ t == 0, P5_ant := 0 ]
aportes[ , P5_nue := cal_pen_inv * pen_5 * ( l1_5 + l2_5 + l3_5 + l4_5 ) ]
aportes[ t == 0, P5_nue := 0 ]
aportes[ , P5 := P5_ant + P5_nue ]
aportes[ t == 0, P5 := 0 ]

## Beneficios de montepios viudas ------------------------------------------------------------------
aportes[ , P7_nodec := cal_pen_viu * ( 12 / 13 ) * ( pen_ant_7 - sbu ) * l7 ]
aportes[ t == 0, P7_nodec := 0 ]
# aportes[ t > 0, P7_ant := cal_pen_viu * pen_ant_7 * l7 ]
# aportes[ t > 0, P7_nue := cal_pen_viu * pen_7 * ( l1_7 + l2_ + l3_5 + l4_5 ) ]
# aportes[ t > 0, P7_nodec := cal_pen_viu * ( 12 / 13 ) * ( ( pen_ant_7 - sbu ) * l7 + ( pen_7 - sbu ) * ( l1_4 + l2_4 + l3_4 + l5_4 ) ) ]
aportes[ , P7 := cal_pen_viu * pen_ant_7 * l7 ]
aportes[ t == 0, P7 := 0 ]

## Beneficios de montepios huérfanos ---------------------------------------------------------------
aportes[ , P8_nodec := cal_pen_orf * ( 12 / 13 ) * ( pen_ant_8 - sbu ) * l8 ]
aportes[ t == 0, P8_nodec := 0 ]
aportes[ , P8 := cal_pen_orf * pen_ant_8 * l8 ]
aportes[ t == 0, P8 := 0 ]

## Beneficios por pensiones ------------------------------------------------------------------------
aportes[ , P_nodec := P4_nodec + P5_nodec + P7_nodec + P8_nodec ]
aportes[ t == 0, P_nodec := 0 ]
aportes[ , P := P4 + P5 + P7 + P8 ]
aportes[ t == 0, P := 0 ]

## Proyección de aportes ---------------------------------------------------------------------------
message( '\tProyectando aportes' )
aportes[ , l := l2 + l4 + l5 + l7 + l8 ] 

# Aportes de activos
aportes[ , A2 := por_apo_2 * M ]
aportes[ t == 0 , A2 := 0 ]

# Aportes de pensionistas de vejez sin décimos
aportes[ , A4 := por_apo_4 * P4_nodec ] 
aportes[ t == 0 , A3 := 0 ]

# Aportes de pensionistas de invalidez sin décimos
aportes[ , A5 := por_apo_5 * P5_nodec ]
aportes[ t == 0 , A5 := 0 ]

# Aportes de pensionistas de viudedad sin décimos
aportes[ , A7 := por_apo_7 * P7_nodec ]
aportes[ t == 0 , A7 := 0 ]

# Aportes de pensionistas de orfandad sin décimos
aportes[ , A8 := por_apo_8 * P8_nodec ]
aportes[ t == 0 , A8 := 0 ]

# Aportes de dependientes cónyuges
aportes[ , A9 := por_apo_9 * M ]
aportes[ t == 0 , A9 := 0 ]

# Aportes de cotizantes para menores de 18
aportes[ , A11 := por_apo_11 * M ]
aportes[ t == 0 , A11 := 0 ]

aportes[ , fac_ext := 0 ]
aportes[ l > 0, fac_ext := ( l9 + l11 ) / l ]
aportes[ , fac_ext := pmin( fac_ext, mean( fac_ext ) ), by = list( t ) ]

aportes[ , A_ext_2 := por_apo_ext_2 * fac_ext *  M ]
aportes[ t == 0 , A_ext_2 := 0 ]

aportes[ , A_ext_4 := por_apo_ext_4 * fac_ext * P4_nodec ] 
aportes[ t == 0 , A_ext_4 := 0 ]

aportes[ , A_ext_5 := por_apo_ext_5 * fac_ext * P5_nodec ] 
aportes[ t == 0 , A_ext_4 := 0 ]

aportes[ , A_ext_7 := por_apo_ext_7 * fac_ext * P7_nodec ] 
aportes[ t == 0 , A_ext_7 := 0 ]

aportes[ , A_ext_8 := por_apo_ext_8 * fac_ext * P8_nodec ] 
aportes[ t == 0 , A_ext_8 := 0 ]

# Aportes afiliados
aportes[ , A_pen_ext := A_ext_4 + A_ext_5 + A_ext_7 + A_ext_8 ]
aportes[ , A_ext := A_ext_2 + A_ext_4 + A_ext_5 + A_ext_7 + A_ext_8 ]
aportes[ , A_afi := A2 + A4 + A5 + A7 + A9 + A11 ]

# Aportes totales
aportes[ , A := A_afi + A_ext ]

# Gasto administrativo
aportes[ , G := por_apo_2 * por_apo_gas * M ]
aportes[ t == 0 , G := 0 ]

# cat( names( aportes ), sep = "', '" )
colsadd <- c( 
  'M', 
  'P4_nodec', 'P4_ant', 'P4_nue', 'P4', 
  'P5_nodec', 'P5_ant', 'P5_nue', 'P5', 
  'P7_nodec', 'P7', 
  'P8_nodec', 'P8', 
  'P_nodec', 'P', 
  'A2', 'A4', 'A3', 'A5', 'A7', 'A8', 'A9', 'A11', 
  'A_ext_2', 'A_ext_4', 'A_ext_5', 'A_ext_7', 'A_ext_8', 
  'A_pen_ext', 'A_ext', 'A_afi', 'A', 'G' )

aportes_anual <- aportes[ , c( i_a = first( i_a ), lapply( .SD, sum, na.rm = TRUE ) ), .SDcols = colsadd, by = list( t ) ]

# Estimación de beneficios -------------------------------------------------------------------------
beneficios <- aportes[ , list( 
  u_m = first( u_m ), 
  l2 = sum( l2 ), 
  l4 = sum( l4 ), 
  l5 = sum( l5 ), 
  l7 = sum( l7 ), 
  l8 = sum( l8 ), 
  l9 = sum( l9 ), 
  l11 = sum( l11 ), 
  M = sum( M ) ),
  by = list( t, sexo, x ) ]

# Estimación de beneficios de salud
beneficios[ , u := as.character( 
  cut( x, breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
       include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ) ]
aux <- copy( beneficios )
aux[ , u := as.character( 
  cut( x, breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
       include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ) ]
beneficios <- rbind( beneficios, aux )

# Incluyendo severidad estimada
beneficios <- merge.data.table( 
  beneficios, 
  ben_est_sev[ , list( sexo, u, enf, ser, cap, icd, ED, EX, q_p, q_s, q_c ) ],
  by = c( 'sexo', 'u' ), 
  all.x = TRUE, 
  allow.cartesian = TRUE )
beneficios <- beneficios[ !is.na( enf ) ]

# Incluyendo frecuencia estimada
beneficios <- merge.data.table( 
  beneficios, 
  ben_est_fre[ t == t_max, list( sexo, u, enf, lambda ) ],
  by = c( 'sexo', 'u', 'enf' ), 
  all.x = TRUE, 
  allow.cartesian = TRUE )

beneficios[ is.na( q_p ), q_p := 0 ]
beneficios[ is.na( q_s ), q_s := 0 ]
beneficios[ is.na( q_c ), q_c := 0 ]
beneficios[ is.na( lambda ), lambda := 0 ]
beneficios[ is.na( ED ), ED := 0 ]
beneficios[ is.na( EX ), EX := 0 ]

## Inflación médica --------------------------------------------------------------------------------
beneficios[ , EX := u_m * EX ]

# Población total cubierta
beneficios[ , l := l2 + l4 + l5 + l7 + l8 + l9 + l11 ]

# Veces al año que requiere de atenciones médicas un activo, viuda, huérfano, dependiente
beneficios[ , q_e := 2.5 ] 

# Veces al año que requiere de atenciones médicas un pensionista
beneficios[ , q_e_pen := 3.8 ]

# Veces al año que requiere de atenciones médicas de hijos
beneficios[ , q_e_hij := 2.8 ]

beneficios <- merge.data.table( 
  esc$apo_act[ , list( 
    t,
    cal_ben_2, cal_ben_cat_2,
    cal_ben_4, cal_ben_cat_4,
    cal_ben_5, cal_ben_cat_5,
    cal_ben_7, cal_ben_cat_7,
    cal_ben_8, cal_ben_cat_8,
    cal_ben_9, cal_ben_cat_9,
    cal_ben_11, cal_ben_cat_11,
    por_apo_est_2,
    por_apo_est_4,
    por_apo_est_5,
    por_apo_est_7,
    por_apo_est_8,
    por_apo_est_9,
    por_apo_est_11,
    por_apo_est_cat_2,
    por_apo_est_cat_4,
    por_apo_est_cat_5,
    por_apo_est_cat_7,
    por_apo_est_cat_8,
    por_apo_est_cat_9,
    por_apo_est_cat_11 ) ],
  beneficios, 
  by = c( 't' ), 
  all.y = TRUE )

## Proyección de beneficios por grupo de asegurados ------------------------------------------------
# Cotizantes
beneficios[ enf == 'E' & ser != 'HO', B2 := cal_ben_2 * lambda * EX * q_p * q_s * q_c * q_e * l2 ]
beneficios[ enf == 'E' & ser == 'HO', B2 := cal_ben_2 * lambda * ED * EX * q_p * q_s * q_c * q_e * l2 ]
beneficios[ enf == 'C' & ser != 'HO', B2 := cal_ben_cat_2 * lambda * EX * q_p * q_s * q_c * q_e * l2 ]
beneficios[ enf == 'C' & ser == 'HO', B2 := cal_ben_cat_2 * lambda * ED * EX * q_p * q_s * q_c * q_e * l2 ]

# Pensionistas de vejez
beneficios[ enf == 'E' & ser != 'HO', B4 := cal_ben_4 * lambda * EX * q_p * q_s * q_c * q_e_pen * l4 ]
beneficios[ enf == 'E' & ser == 'HO', B4 := cal_ben_4 * lambda * ED * EX * q_p * q_s * q_c * q_e_pen * l4 ]
beneficios[ enf == 'C' & ser != 'HO', B4 := cal_ben_cat_4 * lambda * EX * q_p * q_s * q_c * q_e_pen * l4 ]
beneficios[ enf == 'C' & ser == 'HO', B4 := cal_ben_cat_4 * lambda * ED * EX * q_p * q_s * q_c * q_e_pen * l4 ]

# Pensionistas de invalidez
beneficios[ enf == 'E' & ser != 'HO', B5 := cal_ben_5 * lambda * EX * q_p * q_s * q_c * q_e_pen * l5 ]
beneficios[ enf == 'E' & ser == 'HO', B5 := cal_ben_5 * lambda * ED * EX * q_p * q_s * q_c * q_e_pen * l5 ]
beneficios[ enf == 'C' & ser != 'HO', B5 := cal_ben_cat_5 * lambda * EX * q_p * q_s * q_c * q_e_pen * l5 ]
beneficios[ enf == 'C' & ser == 'HO', B5 := cal_ben_cat_5 * lambda * ED * EX * q_p * q_s * q_c * q_e_pen * l5 ]

# Pensionistas de montepio por viudedad
beneficios[ enf == 'E' & ser != 'HO', B7 := cal_ben_7 * lambda * EX * q_p * q_s * q_c * q_e * l7 ]
beneficios[ enf == 'E' & ser == 'HO', B7 := cal_ben_7 * lambda * ED * EX * q_p * q_s * q_c * q_e * l7 ]
beneficios[ enf == 'C' & ser != 'HO', B7 := cal_ben_cat_7 * lambda * EX * q_p * q_s * q_c * q_e * l7 ]
beneficios[ enf == 'C' & ser == 'HO', B7 := cal_ben_cat_7 * lambda * ED * EX * q_p * q_s * q_c * q_e * l7 ]

# Pensionistas de montepio por orfandad
beneficios[ enf == 'E' & ser != 'HO', B8 := cal_ben_8 * lambda * EX * q_p * q_s * q_c * q_e * l8 ]
beneficios[ enf == 'E' & ser == 'HO', B8 := cal_ben_8 * lambda * ED * EX * q_p * q_s * q_c * q_e * l8 ]
beneficios[ enf == 'C' & ser != 'HO', B8 := cal_ben_cat_8 * lambda * EX * q_p * q_s * q_c * q_e * l8 ]
beneficios[ enf == 'C' & ser == 'HO', B8 := cal_ben_cat_8 * lambda * ED * EX * q_p * q_s * q_e * q_e * l8 ]

# Dependientes cónyuges
beneficios[ enf == 'E' & ser != 'HO', B9 := cal_ben_9 * lambda * EX * q_p * q_s * q_c * q_e * l9 ]
beneficios[ enf == 'E' & ser == 'HO', B9 := cal_ben_9 * lambda * ED * EX * q_p * q_s * q_c * q_e * l9 ]
beneficios[ enf == 'C' & ser != 'HO', B9 := cal_ben_cat_9 * lambda * EX * q_p * q_s * q_c * q_e * l9 ]
beneficios[ enf == 'C' & ser == 'HO', B9 := cal_ben_cat_9 * lambda * ED * EX * q_p * q_s * q_c * q_e * l9 ]

# Dependientes hijos menores de 18 años
beneficios[ , B11 := 0 ]
beneficios[ enf == 'E' & ser != 'HO', B11 := cal_ben_11 * lambda * EX * q_p * q_s * q_c * q_e_hij * l11 ] 
beneficios[ enf == 'E' & ser == 'HO', B11 := cal_ben_11 * lambda * ED * EX * q_p * q_s * q_c * q_e_hij * l11 ]
beneficios[ enf == 'C' & ser != 'HO', B11 := cal_ben_cat_11 * lambda * EX * q_p * q_s * q_c * q_e_hij * l11 ] 
beneficios[ enf == 'C' & ser == 'HO', B11 := cal_ben_cat_11 * lambda * ED * EX * q_p * q_s * q_c * q_e_hij * l11 ]

# Beneficio por subsidios
cal_sub <- 0.015702013
por_mas <- 70 / 365
beneficios[ , B12 := 0 ]
beneficios[ enf == 'E', B12 := cal_sub * q_p * q_s * q_c * q_e * por_mas * M ]

beneficios[ t == 0, `:=`( B2 = 0, B4 = 0, B5 = 0, B7 = 0, B8 = 0 , B9 = 0 , B11 = 0, B12 = 0 ) ]

# Separando beneficios de catastróficas y no catastróficas
beneficios[ , `:=`( B2_cat = B2, 
                    B4_cat = B4, 
                    B5_cat = B5, 
                    B7_cat = B7, 
                    B8_cat = B8,
                    B9_cat = B9,
                    B11_cat = B11 ) ]

beneficios[ enf == 'E',`:=`( B2_cat = 0, 
                             B4_cat = 0, 
                             B5_cat = 0, 
                             B7_cat = 0, 
                             B8_cat = 0,
                             B9_cat = 0, 
                             B11_cat = 0 ) ]

beneficios[ , `:=`( B2_ncat = B2, 
                    B4_ncat = B4, 
                    B5_ncat = B5, 
                    B7_ncat = B7, 
                    B8_ncat = B8,
                    B9_ncat = B9,
                    B11_ncat = B11 ) ]

beneficios[ enf == 'C',`:=`( B2_ncat = 0, 
                             B4_ncat = 0, 
                             B5_ncat = 0, 
                             B7_ncat = 0, 
                             B8_ncat = 0,
                             B9_ncat = 0,
                             B11_ncat = 0 ) ]

# Beneficios totales
beneficios[ , B_pen_cat := B4_cat + B5_cat + B7_cat + B8_cat ]
beneficios[ , B_pen_ncat := B4_ncat + B5_ncat + B7_ncat + B8_ncat ]
beneficios[ , B_pen := B4 + B5 + B7 + B8 ]

beneficios[ , B_dep_cat := B9_cat + B11_cat ]
beneficios[ , B_dep_ncat := B9_ncat + B11_ncat ]
beneficios[ , B_dep := B9 + B11 ]

beneficios[ , B_cat := B2_cat + B4_cat + B5_cat + B7_cat + B8_cat + B9_cat + B11_cat ]
beneficios[ , B_ncat := B2_ncat + B4_ncat + B5_ncat + B7_ncat + B8_ncat + B9_ncat + B11_ncat + B12 ]
beneficios[ , B := B2 + B4 + B5 + B7 + B8 + B9 + B11 + B12 ]

# Sin asegurados no hay beneficios
# beneficios[ l == 0, `:=`( 
#   B = 0, B_cat = 0, B_ncat = 0, 
#   B_pen = 0, B_pen_cat = 0, B_pen_ncat = 0, B_dep = 0, B_dep_cat = 0, B_dep_ncat = 0, 
#   B2 = 0, B4 = 0, B5 = 0, B7 = 0, B8 = 0, B9 = 0, B11 = 0,
#   B2_cat = 0, B4_cat = 0, B5_cat = 0, B7_cat = 0, B8_cat = 0, B9_cat = 0, B11_cat = 0, 
#   B2_ncat = 0, B4_ncat = 0, B5_ncat = 0, B7_ncat = 0, B8_ncat = 0, B9_ncat = 0, B11_ncat = 0,
#   B12 = 0 ) ]

## Contribuciones estatales en función de los beneficios -------------------------------------------
# Porcentajes para dividir el gasto administrativo
beneficios[ , BT := sum( B, na.rm = TRUE ), by = list( t ) ]
beneficios[ , per_2_cat := ifelse( BT > 0, B2_cat / BT, 0 ) ]
beneficios[ , per_2_ncat := ifelse( BT > 0, ( B2_ncat + B12 ) / BT, 0 ) ]
beneficios[ , per_2 := ifelse( BT > 0, ( B2 + B12 ) / BT, 0 ) ]
beneficios[ , per_4_cat := ifelse( BT > 0, B4_cat / BT, 0 ) ]
beneficios[ , per_4_ncat := ifelse( BT > 0, B4_ncat / BT, 0 ) ]
beneficios[ , per_4 := ifelse( BT > 0, B4 / BT, 0 ) ]
beneficios[ , per_5_cat := ifelse( BT > 0, B5_cat / BT, 0 ) ]
beneficios[ , per_5_ncat := ifelse( BT > 0, B5_ncat / BT, 0 ) ]
beneficios[ , per_5 := ifelse( BT > 0, B5 / BT, 0 ) ]
beneficios[ , per_7_cat := ifelse( BT > 0, B7_cat / BT, 0 ) ]
beneficios[ , per_7_ncat := ifelse( BT > 0, B7_ncat / BT, 0 ) ]
beneficios[ , per_7 := ifelse( BT > 0, B7 / BT, 0 ) ]
beneficios[ , per_8_cat := ifelse( BT > 0, B8_cat / BT, 0 ) ]
beneficios[ , per_8_ncat := ifelse( BT > 0, B8_ncat / BT, 0 ) ]
beneficios[ , per_8 := ifelse( BT > 0, B8 / BT, 0 ) ]
beneficios[ , per_9_cat := ifelse( BT > 0, B9_cat / BT, 0 ) ]
beneficios[ , per_9_ncat := ifelse( BT > 0, B9_ncat / BT, 0 ) ]
beneficios[ , per_9 := ifelse( BT > 0, B9 / BT, 0 ) ]
beneficios[ , per_11_cat := ifelse( BT > 0, B11_cat / BT, 0 ) ]
beneficios[ , per_11_ncat := ifelse( BT > 0, B11_ncat / BT, 0 ) ]
beneficios[ , per_11 := ifelse( BT > 0, B11 / BT, 0 ) ]
beneficios[ , BT := NULL ]

beneficios <- merge.data.table( 
  beneficios, 
  aportes_anual[ , list( t, G ) ], 
  by = c( 't' ), all.x = TRUE )

beneficios[ , A_est_2_cat := 0 ]
beneficios[ enf == 'C', A_est_2_cat := por_apo_est_cat_2 * ( B2 + per_2_cat * G )  ]
beneficios[ , A_est_4_cat := 0 ]
beneficios[ enf == 'C', A_est_4_cat := por_apo_est_cat_4 * ( B4  + per_4_cat * G ) ]
beneficios[ , A_est_5_cat := 0 ]
beneficios[ enf == 'C', A_est_5_cat := por_apo_est_cat_5 * ( B5  + per_5_cat * G ) ]
beneficios[ , A_est_7_cat := 0 ]
beneficios[ enf == 'C', A_est_7_cat := por_apo_est_cat_7 * ( B7 + per_7_cat * G ) ]
beneficios[ , A_est_8_cat := 0 ]
beneficios[ enf == 'C', A_est_8_cat := por_apo_est_cat_8 * ( B8 + per_8_cat * G ) ]
beneficios[ , A_est_9_cat := 0 ]
beneficios[ enf == 'C', A_est_9_cat := por_apo_est_cat_9 * ( B9  + per_9_cat * G ) ]
beneficios[ , A_est_11_cat := 0 ]
beneficios[ enf == 'C', A_est_11_cat := por_apo_est_cat_11 * ( B11 + per_11_cat * G ) ]

beneficios[ , A_est_2_ncat := 0 ]
beneficios[ enf == 'E', A_est_2 := por_apo_est_2 * ( B2  + per_2_ncat * G ) ]
beneficios[ , A_est_4_ncat := 0 ]
beneficios[ enf == 'E' , A_est_4_ncat := por_apo_est_4 * ( B4 + per_4_ncat * G ) ]
beneficios[ , A_est_5_ncat := 0 ]
beneficios[ enf == 'E' , A_est_5_ncat := por_apo_est_5 * ( B5 + per_5_ncat * G ) ]
beneficios[ , A_est_7_ncat := 0 ]
beneficios[ enf == 'E' , A_est_7_ncat := por_apo_est_7 * ( B7 + per_7_ncat * G ) ]
beneficios[ , A_est_8_ncat := 0 ]
beneficios[ enf == 'E' , A_est_8_ncat := por_apo_est_8 * ( B8 + per_8_ncat * G ) ]
beneficios[ , A_est_9_ncat := 0 ]
beneficios[ enf == 'E' , A_est_9_ncat := por_apo_est_9 * ( B9 + per_9_ncat * G ) ]
beneficios[ , A_est_11_ncat := 0 ]
beneficios[ enf == 'E' , A_est_11_ncat := por_apo_est_11 * ( B11 + per_11_ncat * G ) ]

beneficios[ , A_est_2 := A_est_2_cat + A_est_2_ncat ]
beneficios[ , A_est_4 := A_est_4_cat + A_est_4_ncat ]
beneficios[ , A_est_5 := A_est_5_cat + A_est_5_ncat ]
beneficios[ , A_est_7 := A_est_7_cat + A_est_7_ncat ]
beneficios[ , A_est_8 := A_est_8_cat + A_est_8_ncat ]
beneficios[ , A_est_9 := A_est_9_cat + A_est_9_ncat ]
beneficios[ , A_est_11 := A_est_11_cat + A_est_11_ncat ]

beneficios[ , A_est_pen_cat := A_est_4_cat + A_est_5_cat + A_est_7_cat + A_est_8_cat ]
beneficios[ , A_est_pen_ncat := A_est_4_ncat + A_est_5_ncat + A_est_7_ncat + A_est_8_ncat ]
beneficios[ , A_est_pen := A_est_4 + A_est_5 + A_est_7 + A_est_8 ]

beneficios[ , A_est_dep_cat := A_est_9_cat + A_est_11_cat ]
beneficios[ , A_est_dep_ncat := A_est_9_ncat + A_est_11_ncat ]
beneficios[ , A_est_dep := A_est_9 + A_est_11 ]

beneficios[ , A_est_cat := A_est_2_cat + A_est_4_cat + A_est_5_cat + A_est_7_cat + A_est_8_cat + A_est_9_cat + A_est_11_cat ]
beneficios[ , A_est_ncat := A_est_2_ncat + A_est_4_ncat + A_est_5_ncat + A_est_7_ncat + A_est_8_ncat + A_est_9_ncat + A_est_11_ncat ]
beneficios[ , A_est := A_est_2 + A_est_4 + A_est_5 + A_est_7 + A_est_8 + A_est_9 + A_est_11 ]

# Balance ------------------------------------------------------------------------------------------
# cat( names( beneficios ), sep = "', '" )
colsadd <- c( 
  'B2', 'B4', 'B5', 'B7', 'B8', 'B9', 'B11', 'B12', 
  'B2_cat', 'B4_cat', 'B5_cat', 'B7_cat', 'B8_cat', 'B9_cat', 'B11_cat', 
  'B2_ncat', 'B4_ncat', 'B5_ncat', 'B7_ncat', 'B8_ncat', 'B9_ncat', 'B11_ncat', 
  'B_pen_cat', 'B_pen_ncat', 'B_pen', 'B_dep_cat', 'B_dep_ncat', 'B_dep', 'B_cat', 'B_ncat', 'B',
  'A_est_2_cat', 'A_est_4_cat', 'A_est_5_cat', 'A_est_7_cat', 'A_est_8_cat', 'A_est_9_cat', 'A_est_11_cat', 
  'A_est_2_ncat', 'A_est_4_ncat', 'A_est_5_ncat', 'A_est_7_ncat', 'A_est_8_ncat', 'A_est_9_ncat', 'A_est_11_ncat', 
  'A_est_2', 'A_est_4', 'A_est_5', 'A_est_7', 'A_est_8', 'A_est_9', 'A_est_11', 
  'A_est_pen_cat', 'A_est_pen_ncat', 'A_est_pen', 'A_est_dep_cat', 'A_est_dep_ncat', 'A_est_dep',
  'A_est_cat', 'A_est_ncat', 'A_est' )

beneficios_anual <- beneficios[ , lapply( .SD, sum, na.rm = TRUE ), .SDcols = colsadd, by = list( t ) ]
beneficios_anual[ , anio := t + parametros$anio_ini ]
setorder( beneficios_anual, t )

balance_anual <- merge.data.table( aportes_anual, beneficios_anual, by = 't' )
balance_anual <- merge.data.table( balance_anual, esc$apo_act[ , list( t, u_a, v_a, por_apo_gas ) ], by = 't' )

balance_anual[ , A := A_afi + A_ext + A_est ]

## Balance corriente -------------------------------------------------------------------------------
balance_anual[ , V_cor := A - B - G ]

# Balance actuarial --------------------------------------------------------------------------------
setorder( balance_anual, t )

# cat( names( balance_anual ), sep = "', '" )
colsadd <- c(
  'M', 'P', 'P_nodec', 
  'P4', 'P4_nodec', 'P5', 'P5_nodec', 'P7', 'P7_nodec', 'P8', 'P8_nodec', 
  'A', 'A_afi', 'A_ext', 'A2', 'A4', 'A5', 'A7', 'A8', 'A9', 'A11', 
  'A_ext_2', 'A_ext_4', 'A_ext_5', 'A_ext_7', 'A_ext_8', 
  'G', 
  'B2', 'B4', 'B5', 'B7', 'B8', 'B9', 'B11', 'B12',
  'B2_cat', 'B4_cat', 'B5_cat', 'B7_cat', 'B8_cat', 'B9_cat', 'B11_cat', 
  'B2_ncat', 'B4_ncat', 'B5_ncat', 'B7_ncat', 'B8_ncat', 'B9_ncat', 'B11_ncat', 
  'B_pen_cat', 'B_pen_ncat', 'B_pen', 'B_dep_cat', 'B_dep_ncat', 'B_dep', 'B_cat', 'B_ncat', 'B', 
  'A_est_2_cat', 'A_est_4_cat', 'A_est_5_cat', 'A_est_7_cat', 'A_est_8_cat', 'A_est_9_cat', 'A_est_11_cat',
  'A_est_2_ncat', 'A_est_4_ncat', 'A_est_5_ncat', 'A_est_7_ncat', 'A_est_8_ncat', 'A_est_9_ncat', 'A_est_11_ncat',
  'A_est_2', 'A_est_4', 'A_est_5', 'A_est_7', 'A_est_8', 'A_est_9', 'A_est_11',
  'A_est_pen_cat', 'A_est_pen_ncat', 'A_est_pen', 'A_est_dep_cat', 'A_est_dep_ncat', 'A_est_dep',
  'A_est_cat', 'A_est_ncat', 'A_est'
)
colsvap <- paste0( colsadd, '_vap' )
balance_anual[ , ( colsvap ) := lapply( .SD, FUN = function( c ) cumsum( v_a * c ) ), .SDcols = colsadd ]

## Balance inicial ---------------------------------------------------------------------------------
balance_anual[ , V0 := esc$V0 ]

## Balance capitalizado ----------------------------------------------------------------------------
balance_anual[ , V_cap := V_cor ]
balance_anual[ t == 0, V_cap := esc$V0 ]
balance_anual[ , V_cap := u_a * cumsum( v_a * V_cap ) ]

## Balance actuarial -------------------------------------------------------------------------------
balance_anual[ , V := v_a * V_cap ]

## Intereses que genera la reserva -----------------------------------------------------------------
balance_anual[ , I := i_a * shift( V_cap, fill = 0 ) ]

## Activo y Pasivo ---------------------------------------------------------------------------------
balance_anual[ , Activo := A_vap + V0 ]
balance_anual[ , Pasivo := B_vap + G_vap ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( aportes, beneficios, aportes_anual, beneficios_anual, balance_anual, 
      file = esc$rdata_balance )

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( aportes, beneficios, aportes_anual, beneficios_anual, balance_anual )
rm( list = ls()[ !( ls() %in% c( parametros_lista ) ) ] )
gc()

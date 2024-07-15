message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando reportes excel para SAL' )

if ( !dir.exists( parametros$resultado_seguro ) ) {
  source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )  
}

font_name <- 'Calibri'

head_style <- createStyle( fontName = font_name, fontSize = 11, fontColour = 'black', 
                           textDecoration = 'bold',
                           halign = 'center', valign = 'center', wrapText = TRUE )

money_style <- createStyle( fontName = font_name, fontSize = 11, fontColour = 'black',
                          numFmt = '#,##0.00;[RED]-#,##0.00', 
                          valign = 'center' )

per_style <- createStyle( fontName = font_name, fontSize = 11, fontColour = 'black',
                          numFmt = '0.00%', 
                          valign = 'center' )

text_style <- createStyle( fontName = font_name, fontSize = 11, fontColour = 'black',
                           valign = 'center' )

# Reporte de primas --------------------------------------------------------------------------------
message( '\tGenerando reportes de primas' )

rep_nom <- paste0( parametros$resultado_seguro, 'IESS_SAL_primas_', parametros$anio_ini, '.xlsx' )
wb <- createWorkbook()

escenarios <- paste0( 'escenario_', 1:5 )
names_escenarios <- c( 'Legal', 'Base', 'Alternativo', 'Pesimista', 'Risko' )

rep_fin <- data.table( 
  id = 1:30,
  var = c( 'pmg', 'pmg_cat', 'pmg_ncat', 
           'pmg_2', 'pmg_2_cat', 'pmg_2_ncat', 
           'pmg_pen', 'pmg_pen_cat', 'pmg_pen_ncat', 
           'pmg_4', 'pmg_4_cat', 'pmg_4_ncat', 
           'pmg_5', 'pmg_5_cat', 'pmg_5_ncat', 
           'pmg_7', 'pmg_7_cat', 'pmg_7_ncat', 
           'pmg_8', 'pmg_8_cat', 'pmg_8_ncat', 
           'pmg_dep', 'pmg_dep_cat', 'pmg_dep_ncat', 
           'pmg_9', 'pmg_9_cat', 'pmg_9_ncat', 
           'pmg_11', 'pmg_11_cat', 'pmg_11_ncat' ),
  'Descripción' = c( 
    'Prima media general total',
    'Prima media general total para enfermedades catastróficas',
    'Prima media general total para enfermedades no castastróficas',
    'Prima media general para activos, estado 2',
    'Prima media general para cubrir enfermedades catastróficas de activos, estado 2',
    'Prima media general para cubrir enfermedades no catastróficas de activos, estado 2',
    'Prima media general para pensionistas, estados 4, 5, 7, 8',
    'Prima media general para cubrir enfermedades catastróficas de pensionistas, estado 4, 5, 7, 8',
    'Prima media general para cubrir enfermedades no catastróficas de pensionistas, estado 4, 5, 7 8',
    'Prima media general para pensionistas de vejez, estado 4',
    'Prima media general para cubrir enfermedades catastróficas de pensionistas de vejez, estado 4',
    'Prima media general para cubrir enfermedades no catastróficas de pensionistas de vejez, estado 4',
    'Prima media general para pensionistas de invalidez, estado 5',
    'Prima media general para cubrir enfermedades catastróficas de pensionistas de invalidez, estado 5',
    'Prima media general para cubrir enfermedades no catastróficas de pensionistas de invalidez, estado 5',
    'Prima media general para pensionistas de viudedad, estado 7',
    'Prima media general para cubrir enfermedades catastróficas de pensionistas de viudedad, estado 7',
    'Prima media general para cubrir enfermedades no catastróficas de pensionistas de viudedad, estado 7',
    'Prima media general para pensionistas de orfandad, estado 8',
    'Prima media general para cubrir enfermedades catastróficas de pensionistas de orfandad, estado 8',
    'Prima media general para cubrir enfermedades no catastróficas de pensionistas de orfandad, estado 8',
    'Prima media general para dependientes, estados 9 y 11',
    'Prima media general para cubrir enfermedades catastróficas de dependientes, estados 9 y 11',
    'Prima media general para cubrir enfermedades no catastróficas de dependientes, estados 9 y 11',
    'Prima media general para dependientes cónyuges, estado 9',
    'Prima media general para cubrir enfermedades catastróficas de dependientes cónyuges, estado 9',
    'Prima media general para cubrir enfermedades no catastróficas de dependientes cónyuges, estado 9',
    'Prima media general para dependientes hijos menores de 18 años, estado 11',
    'Prima media general para cubrir enfermedades catastróficas de dependientes hijos menores de 18 años, estado 11',
    'Prima media general para cubrir enfermedades no catastróficas de dependientes hijos menores de 18 años, estado 11' )
)

for ( i in 1:length( escenarios ) ) {
  
  escenario <- escenarios[ i ]
  load( paste0( parametros$sal_rdata_icomp_primas, escenario, '.RData' ) )
  
  # cat( names( prima ), sep = ", " )
  rep <- prima[ anio == parametros$anio_fin, list( 
    anio, 
    pmg, pmg_cat, pmg_ncat, 
    pmg_2, pmg_2_cat, pmg_2_ncat, 
    pmg_pen, pmg_pen_cat, pmg_pen_ncat, 
    pmg_4, pmg_4_cat, pmg_4_ncat, 
    pmg_5, pmg_5_cat, pmg_5_ncat, 
    pmg_7, pmg_7_cat, pmg_7_ncat, 
    pmg_8, pmg_8_cat, pmg_8_ncat, 
    pmg_dep, pmg_dep_cat, pmg_dep_ncat, 
    pmg_9, pmg_9_cat, pmg_9_ncat, 
    pmg_11, pmg_11_cat, pmg_11_ncat ) ]
  
  rep <- melt.data.table( data = rep, id.vars = 'anio', variable.name = 'var', value.name = 'val' )
  # rep[ grepl( '(4|5|7|8)', var ) & val < 1e-6, val := 0 ]
  rep[ val < 1e-6, val := 0 ]
  rep <- dcast.data.table( data = rep, formula = var ~ anio, value.var = 'val' )
  setnames( rep, c( 'var', names_escenarios[ i ] ) )
  rep_fin <- merge.data.table( rep_fin, rep, by = 'var' )
  
}

setorder( rep_fin, id )
rep_fin[ , id := NULL ]
rep_fin[ , var := NULL ]
sheet_name <- 'Primas'

cols <- 1:ncol( rep_fin )
rows <- 2:( nrow( rep_fin ) + 1 )

addWorksheet( wb, sheet_name )
addStyle( wb, sheet_name, head_style, rows = 1, cols = cols, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, text_style, rows = rows, cols = 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, per_style, rows = rows, cols = 2:ncol( rep_fin ), gridExpand = TRUE, stack = TRUE )
# setRowHeights( wb, sheet_name, rows, 15 )
setColWidths( wb, sheet_name, cols = 1, widths = 100 )
setColWidths( wb, sheet_name, cols = 2:ncol( rep_fin ), widths = 15 )

writeData( wb, sheet_name, rep_fin, startRow = 1, startCol = 1 )

# Reporte de balances ------------------------------------------------------------------------------
escenarios <- paste0( 'escenario_', 1:5 )

balances <- NULL
for ( i in 1:length( escenarios ) ) {
  message( '\r\tProcesando ', escenarios[ i ] )
  load( paste0( parametros$sal_rdata_icomp_balance, escenarios[ i ], '.RData' ) )
  if ( i == 1 ) {
    balances <- balance_anual[ , list( anio, escenario = i, V_cor, V ) ]
  } else {
    balances <- rbindlist( l = list( balances, balance_anual[ , list( anio, escenario = i, V_cor, V ) ] ) )
  }
}

balances <- dcast.data.table( balances, formula = anio ~ escenario, value.var = c( 'V_cor', 'V' ) )

sheet_name <- 'Balances'
addWorksheet( wb, sheet_name )
addStyle( wb, sheet_name, head_style, rows = 1, cols = 1:ncol( balances ), gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, money_style, rows = 2:( nrow( balances ) + 1 ), cols = 2:ncol( balances ), gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, cols = 1, widths = 10 )
setColWidths( wb, sheet_name, cols = 2:ncol( balances ), widths = 20 )
writeData( wb, sheet_name, balances, startRow = 1, startCol = 1 )

saveWorkbook( wb, file = rep_nom, overwrite = TRUE )

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
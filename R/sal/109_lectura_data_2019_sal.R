message( paste( rep('-', 100 ), collapse = '' ) )

# Número de threads que utilizará data table para operar
setDTthreads( threads = 8 )

# Nota ------------------------------------------------------------------------.
# Esta carga consume mucha memoria RAM, no puede ser realizada en una computadora con poca RAM

# Afiliados --------------------------------------------------------------------
# SOAM
file <- paste0( parametros$Data_seg, 'TAB_SALUD_SOAM_2019_2020/', 'AFI_SOAM_2019.txt' )
afi_sal_2019 <- fread( file, header = TRUE, sep = 'auto', showProgress = TRUE )
afi_sal_2019 <- data.table( afi_sal_2019 )

afi_sal_2019 <- afi_sal_2019[ , list( TIP_SER = SERVICIO, 
                                      ID_AFIL = ID_AFILIADO,
                                      ID_PACI = ID_PACIENTE,
                                      SEXO = SEXO,
                                      FEC_NAC = FECHA_NACIMIENTO,
                                      TIPO_ENFERMEDAD = TIPO_ENFERMEDAD,
                                      DIAG_PRINCIPAL = DIAGNOSTICO_PRINCIPAL,
                                      CAPITULO = CAPITULO,
                                      FEC_ING = FECHA_INGRESO,
                                      FEC_SAL = FECHA_SALIDA,
                                      VSOL = VALOR_SOLICITADO ) ]

#Tipo de servicio
afi_sal_2019[ TIP_SER == 'HOSPITALIZACION', TIP_SER := 'HO']
afi_sal_2019[ TIP_SER == 'EMERGENCIA', TIP_SER := 'EM']
afi_sal_2019[ TIP_SER == 'AMBULATORIO', TIP_SER := 'CE']

#Identificacion de afiliado y beneficiario
afi_sal_2019[ , ID_AFIL := c(1:dim(afi_sal_2019)[1]) ]
afi_sal_2019[ , ID_PACI := c(1:dim(afi_sal_2019)[1]) ]

#Sexo, sin problema
afi_sal_2019[ SEXO == 'M', SEXO := 'F' ]
afi_sal_2019[ SEXO == 'H', SEXO := 'M' ]

#Fecha nacimiento
afi_sal_2019$FEC_NAC <- as.Date( afi_sal_2019$FEC_NAC, format = "%d/%m/%Y" )

#Tipo de enfermedad
afi_sal_2019[ TIPO_ENFERMEDAD == 'Enfermedad', TIPO_ENFERMEDAD := 'E' ]
afi_sal_2019[ TIPO_ENFERMEDAD == 'Enfermedad Catastrofica', TIPO_ENFERMEDAD := 'C' ]

#Diagnostipo principal: sin problema

#Capitulo
afi_sal_2019[ , CAPITULO := gsub( 'C', 'Cap.', CAPITULO ) ]

#Fecha de ingreso y salida
afi_sal_2019$FEC_ING <- as.Date( afi_sal_2019$FEC_ING, format = "%d/%m/%Y" )
afi_sal_2019$FEC_SAL <- as.Date( afi_sal_2019$FEC_SAL, format = "%d/%m/%Y" )


# AS400 -----------------------------------------------------------------------.
file <- paste0( parametros$Data_seg, 'TAB_SALUD_AS400_2019/', 'Afiliados.txt' )
afi_sal_2019_as4 <- fread( file, header = TRUE, sep = 'auto', showProgress = TRUE )
afi_sal_2019_as4 <- data.table( afi_sal_2019_as4 )

#Tipo servicio, AD, AX y 43 representan menos del 1%, se excluyen.
afi_sal_2019_as4 <- afi_sal_2019_as4[ TIP_SER != 'AD' & TIP_SER != 'AX' & TIP_SER != '43' ]

#Identificacion afiliado beneficiario
afi_sal_2019_as4[ , ID_AFIL := c(1:dim(afi_sal_2019_as4)[1]) ]
afi_sal_2019_as4[ , ID_PACI := c(1:dim(afi_sal_2019_as4)[1]) ]

#Sexo; los valores: " " "C" tienen un registro, se excluye
afi_sal_2019_as4 <- afi_sal_2019_as4[ SEXO == 'M' | SEXO == 'F' ]

# Fechas
afi_sal_2019_as4$FEC_NAC <- as.Date( afi_sal_2019_as4$FEC_NAC, format = "%d/%m/%Y" )
afi_sal_2019_as4$FEC_ING <- as.Date( afi_sal_2019_as4$FEC_ING, format = "%d/%m/%Y" )
afi_sal_2019_as4$FEC_SAL <- as.Date( afi_sal_2019_as4$FEC_SAL, format = "%d/%m/%Y" )

afi_sal_2019 <- rbind( afi_sal_2019, afi_sal_2019_as4 )

save( afi_sal_2019, file = paste0( parametros$RData_seg, 'IESS_SAL_afiliados_2019.RData' ),
      ascii = FALSE, compress = 'bzip2', compression_level = 9 )

rm( afi_sal_2019, afi_sal_2019_as4 )
gc()

# Pensionistas -----------------------------------------------------------------
#SOAM
file <- paste0( parametros$Data_seg, 'TAB_SALUD_SOAM_2019_2020/', 'JUB_SOAM_2019.txt' )
pen_sal_19 <- fread( file, header = TRUE, sep = 'auto', showProgress = TRUE )
pen_sal_19 <- data.table( pen_sal_19 )

pen_sal_19 <- pen_sal_19[ , list( TIP_SER = SERVICIO, 
                                      ID_AFIL = ID_AFILIADO,
                                      ID_PACI = ID_PACIENTE,
                                      SEXO = SEXO,
                                      FEC_NAC = FECHA_NACIMIENTO,
                                      TIPO_ENFERMEDAD = TIPO_ENFERMEDAD,
                                      DIAG_PRINCIPAL = DIAGNOSTICO_PRINCIPAL,
                                      CAPITULO = CAPITULO,
                                      FEC_ING = FECHA_INGRESO,
                                      FEC_SAL = FECHA_SALIDA,
                                      VSOL = VALOR_SOLICITADO ) ]

#Tipo de servicio
pen_sal_19[ TIP_SER == 'HOSPITALIZACION', TIP_SER := 'HO']
pen_sal_19[ TIP_SER == 'EMERGENCIA', TIP_SER := 'EM']
pen_sal_19[ TIP_SER == 'AMBULATORIO', TIP_SER := 'CE']

#Identificacion de afiliado y beneficiario
pen_sal_19[ , ID_AFIL := c(1:dim(pen_sal_19)[1]) ]
pen_sal_19[ , ID_PACI := c(1:dim(pen_sal_19)[1]) ]

#Sexo, sin problema
pen_sal_19[ SEXO == 'M', SEXO := 'F' ]
pen_sal_19[ SEXO == 'H', SEXO := 'M' ]

#Fecha nacimiento
pen_sal_19$FEC_NAC <- as.Date( pen_sal_19$FEC_NAC, format = "%d/%m/%Y" )

#Tipo de enfermedad
pen_sal_19[ TIPO_ENFERMEDAD == 'Enfermedad', TIPO_ENFERMEDAD := 'E' ]
pen_sal_19[ TIPO_ENFERMEDAD == 'Enfermedad Catastrofica', TIPO_ENFERMEDAD := 'C' ]

#Diagnostipo principal: sin problema

#Capitulo
pen_sal_19[ , CAPITULO := gsub( 'C', 'Cap.', CAPITULO ) ]

#Fecha de ingreso y salida
pen_sal_19$FEC_ING <- as.Date( pen_sal_19$FEC_ING, format = "%d/%m/%Y" )
pen_sal_19$FEC_SAL <- as.Date( pen_sal_19$FEC_SAL, format = "%d/%m/%Y" )

# AS400 -----------------------------------------------------------------------.
file <- paste0( parametros$Data_seg, 'TAB_SALUD_AS400_2019/', 'pensionistas.txt' )
pen_sal_2019_as4 <- fread( file, header = TRUE, sep = 'auto', showProgress = TRUE )
pen_sal_2019_as4 <- data.table( pen_sal_2019_as4 )

#Tipo servicio, los AD, AX y 43 son menos del 1%, se descarta.
pen_sal_2019_as4 <- pen_sal_2019_as4[ TIP_SER != 'AD' & TIP_SER != 'AX' & TIP_SER != '43' ]

#Identificacion afiliado beneficiario
pen_sal_2019_as4[ , ID_AFIL := c(1:dim(pen_sal_2019_as4)[1]) ]
pen_sal_2019_as4[ , ID_PACI := c(1:dim(pen_sal_2019_as4)[1]) ]

#Sexo, sin problema
pen_sal_2019_as4 <- pen_sal_2019_as4[ SEXO == 'M' | SEXO == 'F' ]

# Fechas
pen_sal_2019_as4$FEC_NAC <- as.Date( pen_sal_2019_as4$FEC_NAC, format = "%d/%m/%Y" )
pen_sal_2019_as4$FEC_ING <- as.Date( pen_sal_2019_as4$FEC_ING, format = "%d/%m/%Y" )
pen_sal_2019_as4$FEC_SAL <- as.Date( pen_sal_2019_as4$FEC_SAL, format = "%d/%m/%Y" )

pen_sal_19 <- rbind( pen_sal_19, pen_sal_2019_as4 )

save( pen_sal_19, file = paste0( parametros$RData_seg, 'IESS_SAL_pensionistas_2019.RData' ),
      ascii = FALSE, compress = 'bzip2', compression_level = 9 )
rm( pen_sal_19, pen_sal_2019_as4 )
gc()

# Hijos ------------------------------------------------------------------------
#SOAM
file <- paste0( parametros$Data_seg, 'TAB_SALUD_SOAM_2019_2020/', 'HIJ_SOAM_2019.txt' )
hij_sal_19 <- fread( file, header = TRUE, sep = 'auto', showProgress = TRUE )
hij_sal_19 <- data.table( hij_sal_19 )

hij_sal_19 <- hij_sal_19[ , list( TIP_SER = SERVICIO, 
                                  ID_AFIL = ID_AFILIADO,
                                  ID_PACI = ID_PACIENTE,
                                  SEXO = SEXO,
                                  FEC_NAC = FECHA_NACIMIENTO,
                                  TIPO_ENFERMEDAD = TIPO_ENFERMEDAD,
                                  DIAG_PRINCIPAL = DIAGNOSTICO_PRINCIPAL,
                                  CAPITULO = CAPITULO,
                                  FEC_ING = FECHA_INGRESO,
                                  FEC_SAL = FECHA_SALIDA,
                                  VSOL = VALOR_SOLICITADO ) ]

#Tipo de servicio
hij_sal_19[ TIP_SER == 'HOSPITALIZACION', TIP_SER := 'HO']
hij_sal_19[ TIP_SER == 'EMERGENCIA', TIP_SER := 'EM']
hij_sal_19[ TIP_SER == 'AMBULATORIO', TIP_SER := 'CE']

#Identificacion de afiliado y beneficiario
hij_sal_19[ , ID_AFIL := c(1:dim(hij_sal_19)[1]) ]
hij_sal_19[ , ID_PACI := c(1:dim(hij_sal_19)[1]) ]

#Sexo, sin problema
hij_sal_19[ SEXO == 'M', SEXO := 'F' ]
hij_sal_19[ SEXO == 'H', SEXO := 'M' ]

#Fecha nacimiento
hij_sal_19$FEC_NAC <- as.Date( hij_sal_19$FEC_NAC, format = "%d/%m/%Y" )

#Tipo de enfermedad
hij_sal_19[ TIPO_ENFERMEDAD == 'Enfermedad', TIPO_ENFERMEDAD := 'E' ]
hij_sal_19[ TIPO_ENFERMEDAD == 'Enfermedad Catastrofica', TIPO_ENFERMEDAD := 'C' ]

#Diagnostipo principal: sin problema

#Capitulo
hij_sal_19[ , CAPITULO := gsub( 'C', 'Cap.', CAPITULO ) ]

#Fecha de ingreso y salida
hij_sal_19$FEC_ING <- as.Date( hij_sal_19$FEC_ING, format = "%d/%m/%Y" )
hij_sal_19$FEC_SAL <- as.Date( hij_sal_19$FEC_SAL, format = "%d/%m/%Y" )

# AS400 -----------------------------------------------------------------------.
file <- paste0( parametros$Data_seg, 'TAB_SALUD_AS400_2019/', 'hijos_18.txt' )
hij_sal_2019_as4 <- fread( file, header = TRUE, sep = 'auto', showProgress = TRUE )
hij_sal_2019_as4 <- data.table( hij_sal_2019_as4 )

hij_sal_2019_as4 <- hij_sal_2019_as4[ , list( TIP_SER, ID_AFIL =  c(1:dim(hij_sal_2019_as4)[1]),
                                  ID_PACI = ID_HIJO,
                                  SEXO,
                                  FEC_NAC,
                                  TIPO_ENFERMEDAD,
                                  DIAG_PRINCIPAL,
                                  CAPITULO,
                                  FEC_ING,
                                  FEC_SAL,
                                  VSOL ) ]

#Tipo servicio, AD, AX y 43 representan menos del 1%, se excluyen.
hij_sal_2019_as4 <- hij_sal_2019_as4[ TIP_SER != 'AD' & TIP_SER != 'AX' & TIP_SER != '43' ]

#Identificacion afiliado beneficiario
hij_sal_2019_as4[ , ID_PACI := c(1:dim(hij_sal_2019_as4)[1]) ]

#Sexo, solo 1 registro distinto de M y F. se excluye
hij_sal_2019_as4 <- hij_sal_2019_as4[ SEXO == 'M' | SEXO == 'F' ]

# Fechas
hij_sal_2019_as4$FEC_NAC <- as.Date( hij_sal_2019_as4$FEC_NAC, format = "%d/%m/%Y" )
hij_sal_2019_as4$FEC_ING <- as.Date( hij_sal_2019_as4$FEC_ING, format = "%d/%m/%Y" )
hij_sal_2019_as4$FEC_SAL <- as.Date( hij_sal_2019_as4$FEC_SAL, format = "%d/%m/%Y" )

hij_sal_19 <- rbind( hij_sal_19, hij_sal_2019_as4 )

save( hij_sal_19, file = paste0( parametros$RData_seg, 'IESS_SAL_hijos_2019.RData' ),
      ascii = FALSE, compress = 'bzip2', compression_level = 9 )


# ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

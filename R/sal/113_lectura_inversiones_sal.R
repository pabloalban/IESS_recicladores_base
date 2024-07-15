message( paste( rep( '-', 100 ), collapse = '' ) )

#message( '\tReconstruyendo archivo excel del 2018' )
# library("xlsx")
# # Carga de datos -----------------------------------------------------------------------------------
# file_inversiones <- paste0( parametros$Data_seg, 'IESS_SAL_inversiones.RData' )
# load( file = file_inversiones )
# 
# 
# 
# 
# 
# #Archivo en Excel-----------------------------------------------------------------------------------
# file_xlxs<-paste0(parametros$resultado_seguro, 'IESS_SAL_inversiones.xlsx' )
# 
# message( '\tGenerando fechas de cese de beneficiarios de liquidaciones' )
# 
# 
# #Guardar recursos administrados por el BIESS--------------------------------------------------------
# 
# write.xlsx( as.data.frame( iess_inv_tot ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "recurs_adm_biess",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# 
# write.xlsx( as.data.frame( iess_inv_corte  ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "inver_corte",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# 
# 
# write.xlsx( as.data.frame( iess_rend_inv ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "rendimientos_netos",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# 
# write.xlsx( as.data.frame(  iess_ingre_inv ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "ingresos",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# 
# write.xlsx( as.data.frame(  iess_gastos_inv ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "gastos_opera",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# write.xlsx( as.data.frame(  sal_instrumento_inv ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "inv_instrumento",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# 
# 
# write.xlsx( as.data.frame(  iess_bonos_inv_detalle ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "detalle_bonos",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# write.xlsx( as.data.frame(  iess_oblig_inv_detalle ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "detalle_obligaciones",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# write.xlsx( as.data.frame(  iess_titularizaciones_inv_detalle ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "detalle_titulos",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# write.xlsx( as.data.frame(  iess_cupones_inv_detalle ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "detalle_cupones",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
message( '\tLectura de las inversiones de Salud' )
#Cargando información financiera--------------------------------------------------------------------
file <- paste0(parametros$Data_seg, 'IESS_SAL_inversiones.xlsx' )

#Carga de recursos administrados por el BIESS-------------------------------------------------------
recurs_adm_biess <- read_excel( file,
                                sheet = 'recurs_adm_biess',
                                col_names = TRUE,
                                col_types = NULL,
                                na = "",
                                skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
inver_corte <- read_excel( file,
                           sheet = 'inver_corte',
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
rendimientos_netos <- read_excel( file,
                                  sheet = 'rendimientos_netos',
                                  col_names = TRUE,
                                  col_types = NULL,
                                  na = "",
                                  skip = 0 ) %>% clean_names() %>%
  mutate(corte=as.Date(corte,"%d/%m/%Y"))
# --------------------------------------------------------------------------------------------------
ingresos <- read_excel( file,
                        sheet = 'ingresos',
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
gastos_opera <- read_excel( file,
                            sheet = 'gastos_opera',
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
inv_instrumento <- read_excel( file,
                               sheet = 'inv_instrumento',
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
creditos <- read_excel( file,
                        sheet = 'creditos',
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
detalle_bonos <- read_excel( file,
                             sheet = 'detalle_bonos',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
detalle_obligaciones <- read_excel( file,
                                    sheet = 'detalle_obligaciones',
                                    col_names = TRUE,
                                    col_types = NULL,
                                    na = "",
                                    skip = 0 ) %>% clean_names() 
# --------------------------------------------------------------------------------------------------
recuperacion_bonos <- read_excel( file,
                                  sheet = 'recup_bonos',
                                  col_names = TRUE,
                                  col_types = NULL,
                                  na = "",
                                  skip = 0 ) %>% clean_names() %>%
  mutate(fecha_cupon =as.Date(fecha_cupon ,"%d/%m/%Y"),
         fecha_vcmto=as.Date(fecha_vcmto,"%d/%m/%Y"))
# --------------------------------------------------------------------------------------------------
sp_saldo <- read_excel( file,
                        sheet = 'sp_saldo',
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
sp_rendimiento <- read_excel( file,
                              sheet = 'sp_rendimiento',
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
sp_plazo <- read_excel( file,
                        sheet = 'sp_plazo',
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
sector_publico <- read_excel( file,
                              sheet = 'sector_publico',
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
primas_sal <- read_excel( file,
                          sheet = 'primas_sal',
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0 ) %>% clean_names()
# --------------------------------------------------------------------------------------------------
resumen_recup <- read_excel( file,
                             sheet = 'resumen_recup',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0 ) %>% clean_names()
#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando inversiones en un solo data.frame' )

save( recurs_adm_biess,
      inver_corte,
      rendimientos_netos,
      ingresos,
      gastos_opera,
      inv_instrumento,
      detalle_bonos,
      creditos,
      detalle_obligaciones,
      recuperacion_bonos,
      sp_saldo,
      sp_rendimiento,
      sp_plazo,
      sector_publico,
      primas_sal,
      resumen_recup,
      file = paste0( parametros$RData_seg, 'IESS_SAL_inversiones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
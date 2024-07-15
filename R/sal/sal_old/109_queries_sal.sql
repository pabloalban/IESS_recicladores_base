-- Documentación de consultas SQL ------------------------------------------------------------------
SELECT 'AFILIADOS ACTIVOS', 
  anio, 
  count( DISTINCT IDENTIFICACION_BENEFICIARIO ) AS N, 
  sum(usd) AS USD_TOT, 
  count(*) AS NUMREG
   FROM
( SELECT EXTRACT( YEAR FROM TO_DATE( FECHA_ATENCION, 'DD/MM/YYYY' ) ) as anio , 
    IDENTIFICACION_BENEFICIARIO,
    TO_NUMBER( REPLACE( VALOR_TOTAL,'.',',' ) ) as usd
  FROM MFIGUEROA.BD_FACT_2013 
    where tipo_AF in( 'SG', 'S', 'CV', 'C','JT','J','BE','B','SV','MJ','M','SA','SC','SD','CE','MP') union
  
  SELECT EXTRACT( YEAR FROM TO_DATE( FECHA_ATENCION, 'DD/MM/YYYY' ) ) as anio, 
    IDENTIFICACION_BENEFICIARIO,
    TO_NUMBER( REPLACE( VALOR_TOTAL, '.', ',' ) ) as usd 
  FROM MFIGUEROA.BD_FACT_2014 
    where tipo_AF in(  'SG', 'S', 'CV', 'C','JT','J','BE','B','SV','MJ','M','SA','SC','SD','CE','MP') union 
  
  SELECT EXTRACT(YEAR FROM TO_DATE(FECHA_ATENCION,'DD/MM/YYYY'))as anio, 
    IDENTIFICACION_BENEFICIARIO, 
    TO_NUMBER(REPLACE(VALOR_TOTAL,'.',',')) as usd 
  FROM MFIGUEROA.BD_FACT_2015   
    where tipo_AF in( 'SG', 'S', 'CV', 'C','JT','J','BE','B','SV','MJ','M','SA','SC','SD','CE','MP') union
    
  SELECT EXTRACT(YEAR FROM TO_DATE(FECHA_ATENCION,'DD/MM/YYYY')) as anio, 
    IDENTIFICACION_BENEFICIARIO, 
    TO_NUMBER(REPLACE(VALOR_TOTAL,'.',',')) as usd 
  FROM MFIGUEROA.BD_FACT_2016  
    where tipo_AF in(  'SG', 'S', 'CV', 'C','JT','J','BE','B','SV','MJ','M','SA','SC','SD','CE','MP') union

  SELECT EXTRACT(YEAR FROM TO_DATE(FECHA_ATENCION,'DD/MM/YYYY'))as anio,  
    IDENTIFICACION_BENEFICIARIO, 
    TO_NUMBER(REPLACE(VALOR_TOTAL,'.',',')) as usd 
  FROM MFIGUEROA.BD_FACT_2017   
     where tipo_AF in( 'SG', 'S', 'CV', 'C','JT','J','BE','B','SV','MJ','M','SA','SC','SD','CE','MP') union
     
  SELECT EXTRACT(YEAR FROM TO_DATE(FECHA_ATENCION,'DD/MM/YYYY'))as anio,  
    IDENTIFICACION_BENEFICIARIO, 
    TO_NUMBER(REPLACE(VALOR_TOTAL,'.',',')) as usd 
  FROM MFIGUEROA.BD_FACT_2018   
    where tIPO_AF in( 'SG', 'S', 'CV', 'C','JT','J','BE','B','SV','MJ','M','SA','SC','SD','CE','MP')
)
GROUP BY anio


-- Enfermedades raras ------------------------------------------------------------------------------
 select count( distinct identif ), ANIO from
( select EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, 
    identificacion_beneficiario as identif, 
    DIAGNOSTICO_1 AS DIAG   
  from as400.bd_fact_2013 union
  
  select EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, 
    identificacion_beneficiario as identif, 
    DIAGNOSTICO_1 AS DIAG 
  from as400.bd_fact_2014 union
  
  select EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, 
    identificacion_beneficiario as identif, 
    DIAGNOSTICO_1 AS DIAG   
  from as400.bd_fact_2015 union
  
  select EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, 
    identificacion_beneficiario as identif, 
    DIAGNOSTICO_1 AS DIAG   
  from as400.bd_fact_2016 union
  
  select EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, 
    identificacion_beneficiario as identif, 
    DIAGNOSTICO_1 AS DIAG   
  from as400.bd_fact_2017 union
  
  select EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, 
    identificacion_beneficiario as identif, 
    DIAGNOSTICO_1 AS DIAG 
  from as400.bd_fact_2018 union

   select ANIO AS ANIO, identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Pichincha  union
     
   select ANIO AS ANIO,  identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Azuay union
     
   select ANIO AS ANIO, identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Chimborazo union
     
   select ANIO AS ANIO, identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Eloro union
     
   select ANIO AS ANIO, identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Guayas union
     
   select ANIO AS ANIO, identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Imbabura union
     
   select ANIO AS ANIO, identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Loja union
     
   select ANIO AS ANIO, identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Manabi union
     
   select ANIO AS ANIO, identif_afiliado as identif, cod_diagnostico as DIAG  
     from bi_catalog_salud.bd_soam_Tungurahua
)
WHERE DIAG IN ( 'D56','D58','D610','D66','D67','D680','D681','D682',
'E00','E222', 'E240','E250','E272','E343','E700','E703','E710','E711','E713','E721','E742','E752',
'E760','E761','E762','E78','E831','E833','E840','E841','E848','E854',
'F208','F840',
'G10','G110','G111','G112','G113','G114','G120','G122','G318','G35','G60','G710','G711','G712',
'H810',
'I270',
'L123','L93','L930','L931','L932',
'M023','M028','M061','M080','M081','M082','M083','M084','M303','M321',
'Q038','Q05','Q336','Q390','Q391','Q419', 'Q620','Q643','Q743','Q751','Q754','Q758','Q774','Q778',
'Q780','Q784','Q786','Q790','Q792','Q793','Q796','Q80','Q800','Q801','Q802','Q803','Q824','Q850',
'Q870','Q871','Q873','Q874','Q960','Q961','Q962', 'Q963','Q964','Q973','Q987','Q991','Q992'
)
group by anio


-- Todos -------------------------------------------------------------------------------------------
 select count(distinct identif), ANIO, SUM(USD) from
(select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario) as EDAD, EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2013 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2014 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2015 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2016 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2017 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2018 union

 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Pichincha   union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Azuay       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Chimborazo   union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Eloro        union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Guayas       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Imbabura     union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Loja         union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Manabi       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Tungurahua)
 
 WHERE  BENEFICIARIO IN ( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI','MP', 'SD' , 'SP' ,'MJ' , 'TJ' ,
 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI',
 'AH','A', 'N6','M6', 'CY', 'AR', 'HJ')
 group  by anio 

-- Menores a 18 ------------------------------------------------------------------------------------
 select count(distinct identif), ANIO, SUM(USD)
from
(select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario) as EDAD, EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2013 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2014 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2015 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2016 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2017 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2018 union

 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Pichincha   union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Azuay       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Chimborazo   union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Eloro        union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Guayas       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Imbabura     union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Loja         union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Manabi       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Tungurahua)
 
 WHERE  EDAD < 18  AND BENEFICIARIO IN ('AG', 'NI', 'N6', 'M6')
   group  by anio
   
   
    select count(IDENTIF), COUNT(distinct identif), ANIO, SUM(USD)
from
(select TIPO_BEN AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2013 union
 select TIPO_BEN AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2014  union
 select TIPO_BEN AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2015 union
 select TIPO_BEN AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG    from MFIGUEROA.PROSICK_2016) 
  WHERE  EDAD < 18  AND BENEFICIARIO IN ('AG', 'NI', 'N6', 'M6')
 
 GROUP BY ANIO



-- Afiliados activos -------------------------------------------------------------------------------
select count(IDENTIF), COUNT(distinct identif), ANIO, SUM(USD) from
(select TIPO_SEG AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2013 union
select TIPO_SEG AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2014  union
select TIPO_SEG AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2015 union
select TIPO_SEG AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG    from MFIGUEROA.PROSICK_2016) 
WHERE   BENEFICIARIO LIKE 'SG%'
 
GROUP BY ANIO

---------------
select count(distinct identif), ANIO, SUM(USD)
from
(select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario) as EDAD, EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2013 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2014 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2015 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2016 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2017 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2018 union

 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Pichincha   union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Azuay       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Chimborazo   union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Eloro        union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Guayas       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Imbabura     union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Loja         union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Manabi       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Tungurahua)
 
 WHERE  BENEFICIARIO IN ( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S')
   group  by anio 


-- Jubilados ---------------------------------------------------------------------------------------
      select count(IDENTIF), COUNT(distinct identif), ANIO, SUM(USD)
from
(select TIPO_SEG AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2013 union
 select TIPO_SEG AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2014  union
 select TIPO_SEG AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG   from MFIGUEROA.PROSICK_2015 union
 select TIPO_SEG AS BENEFICIARIO, TOTAL_APROBADO as USD,  EDAD, substr(FECH_ATEN,7,4) AS anio, id_PACIENTE as identif, DIAG_PRINCIPAL AS DIAG    from MFIGUEROA.PROSICK_2016) 
  WHERE  ( BENEFICIARIO LIKE 'JU' OR BENEFICIARIO LIKE 'MO') AND EDAD  < 18
 GROUP BY ANIO


-- Extensión de cobertura --------------------------------------------------------------------------
 select count(distinct identif), ANIO, SUM(USD)
from
(select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario) as EDAD, EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2013 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2014 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2015 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2016 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2017 union
 select TIPO_AF AS BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL, '.',',')) as USD, TO_NUMBER(edad_beneficiario), EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, identificacion_beneficiario as identif, DIAGNOSTICO_1 AS DIAG   from as400.bd_fact_2018 union

 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Pichincha   union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Azuay       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Chimborazo   union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Eloro        union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Guayas       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Imbabura     union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Loja         union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Manabi       union
 select TIPO_BENEFICIARIO AS BENEFICIARIO,(case  WHEN total_aprobado =0 THEN total_solicitado ELSE total_aprobado END)AS USD, EDAD  , ANIO , identif_afiliado as identif, cod_diagnostico as DIAG  from bi_catalog_salud.bd_soam_Tungurahua)
 
 WHERE  BENEFICIARIO IN ( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ')
   group  by anio 


-- AS400 + SOAM corregido --------------------------------------------------------------------------
SELECT anio, COUNT(DISTINCT  ID_BENEFICIARIO) AS NUMERO_PAC, sum(VALOR_TOTAL)  as suma_VALOR_TOTAL, 
SEXO,CIE10, GRUPO_EDAD, grupo_tipo_serv
FROM ( ( SELECT anio, 
  IDENTIF_afiliado AS ID_BENEFICIARIO, 
  ( case WHEN total_aprobado = 0 THEN total_solicitado ELSE total_aprobado END ) AS VALOR_TOTAL,
  edad, 
  TRIM( SEXO ) AS SEXO, 
  COD_DIAGNOSTICO AS DIAG, 
  TIPO_SERVICIO AS TIPO_SERV,
  tipo_beneficiario AS tipo_benef,
  ( CASE 
    WHEN COD_DIAGNOSTICO >='A00' AND COD_DIAGNOSTICO <= 'B999' THEN 'CAPÍTULO 01'
    WHEN COD_DIAGNOSTICO>='C00' AND COD_DIAGNOSTICO<='D499' THEN 'CAPÍTULO 02'
    WHEN COD_DIAGNOSTICO>='D50' AND COD_DIAGNOSTICO<='D899' THEN 'CAPÍTULO 03'
    WHEN COD_DIAGNOSTICO>='E00' AND COD_DIAGNOSTICO<='E909' THEN 'CAPÍTULO 04'
    WHEN COD_DIAGNOSTICO>='F00' AND COD_DIAGNOSTICO<='F999' THEN 'CAPÍTULO 05'
    WHEN COD_DIAGNOSTICO>='G00' AND COD_DIAGNOSTICO<='G999' THEN 'CAPÍTULO 06'
    WHEN COD_DIAGNOSTICO>='H00' AND COD_DIAGNOSTICO<='H599' THEN 'CAPÍTULO 07'
    WHEN COD_DIAGNOSTICO>='H60' AND COD_DIAGNOSTICO<='H959' THEN 'CAPÍTULO 08'
    WHEN COD_DIAGNOSTICO>='I00' AND COD_DIAGNOSTICO<='I999' THEN 'CAPÍTULO 09'
    WHEN COD_DIAGNOSTICO>='J00' AND COD_DIAGNOSTICO<='J999' THEN 'CAPÍTULO 10'
    WHEN COD_DIAGNOSTICO>='K00' AND COD_DIAGNOSTICO<='K939' THEN 'CAPÍTULO 11'
    WHEN COD_DIAGNOSTICO>='L00' AND COD_DIAGNOSTICO<='L999' THEN 'CAPÍTULO 12'
    WHEN COD_DIAGNOSTICO>='M00' AND COD_DIAGNOSTICO<='M999' THEN 'CAPÍTULO 13'
    WHEN COD_DIAGNOSTICO>='N00' AND COD_DIAGNOSTICO<='N999' THEN 'CAPÍTULO 14'
    WHEN COD_DIAGNOSTICO>='O00' AND COD_DIAGNOSTICO<='O999' THEN 'CAPÍTULO 15'
    WHEN COD_DIAGNOSTICO>='P00' AND COD_DIAGNOSTICO<='P966' THEN 'CAPÍTULO 16'
    WHEN COD_DIAGNOSTICO>='Q00' AND COD_DIAGNOSTICO<='Q999' THEN 'CAPÍTULO 17'
    WHEN COD_DIAGNOSTICO>='R00' AND COD_DIAGNOSTICO<='R999' THEN 'CAPÍTULO 18'
    WHEN COD_DIAGNOSTICO>='S00' AND COD_DIAGNOSTICO<='T988' THEN 'CAPÍTULO 19'
    WHEN COD_DIAGNOSTICO>='V01' AND COD_DIAGNOSTICO<='Y988' THEN 'CAPÍTULO 20'
    WHEN COD_DIAGNOSTICO>='Z00' AND COD_DIAGNOSTICO<='Z999' THEN 'CAPÍTULO 21'
    WHEN COD_DIAGNOSTICO>='U00' AND COD_DIAGNOSTICO<='U999' THEN 'CAPÍTULO 22'
  ELSE 'OTRO' END )AS CIE10,
  (case
  WHEN edad < 0  THEN 'G-'
  WHEN edad >= 0 AND edad <= 15 THEN 'G1'
  WHEN edad >= 16 AND edad <= 25 THEN 'G2'
  WHEN edad >= 26 AND edad <= 35 THEN 'G3'
  WHEN edad >= 36 AND edad <= 45 THEN 'G4'
  WHEN edad >= 46 AND edad <= 55 THEN 'G5'
  WHEN edad >= 56 AND edad <= 65 THEN 'G6'
  WHEN edad >= 66 AND edad <= 75 THEN 'G7'
  WHEN edad >= 76 AND edad <= 85 THEN 'G8'
  ELSE 'G9' end) as grupo_edad, 
  (case
 WHEN trim(TIPO_SERVICIO) ='AMBULATORIO'       THEN 'AMB' 
 WHEN trim(TIPO_SERVICIO) ='HOSPITAL DEL DIA'  THEN 'AMB' 
 WHEN trim(TIPO_SERVICIO) ='HEMODIALISIS'      THEN 'AMB' 
 WHEN trim(TIPO_SERVICIO) ='ACCIDENTES'        THEN 'EM' 
 WHEN trim(TIPO_SERVICIO) ='EMERGENCIA'        THEN 'EM'  
 WHEN trim(TIPO_SERVICIO) ='HOSPITALIZACIÓN QUIRURGICA'  THEN 'HO' 
 WHEN trim(TIPO_SERVICIO) ='HOSPITALIZACIÓN CLINICA'     THEN 'HO'
 WHEN trim(TIPO_SERVICIO) ='UCI'               THEN 'HO'
 WHEN trim(TIPO_SERVICIO) ='MATERNIDAD'        THEN 'HO'
 WHEN trim(TIPO_SERVICIO) ='PAQUETES'          THEN 'HO'
 ELSE 'OTRA' end) as grupo_tipo_serv  
  
  FROM
  (select * from bi_catalog_salud.bd_soam_Pichincha   where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') union
   select * from bi_catalog_salud.bd_soam_Azuay       where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') union
   select * from bi_catalog_salud.bd_soam_Chimborazo  where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') union
     select * from bi_catalog_salud.bd_soam_Eloro     where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') union
   select * from bi_catalog_salud.bd_soam_Guayas      where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') union
   select * from bi_catalog_salud.bd_soam_Imbabura    where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') union
     select * from bi_catalog_salud.bd_soam_Loja      where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') UNION
   select * from bi_catalog_salud.bd_soam_Manabi      where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') union
   select * from bi_catalog_salud.bd_soam_Tungurahua  where tipo_beneficiario in( 'SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ'))
   )   

UNION

(SELECT anio, IDENTIFICACION_BENEFICIARIO AS ID_BENEFICIARIO, TO_NUMBER(REPLACE(VALOR_TOTAL,'.',',')) AS VALOR_TOTAL,
 EDAD, TRIM(genero) AS SEXO, DIAGNOSTICO_1 AS DIAG, DES_AREA AS TIPO_SERV, TIPO_AF AS tipo_benef,
(CASE 
  WHEN DIAGNOSTICO_1>='A00' AND DIAGNOSTICO_1<='B999' THEN 'CAPÍTULO 01'
  WHEN DIAGNOSTICO_1>='C00' AND DIAGNOSTICO_1<='D499' THEN 'CAPÍTULO 02'
  WHEN DIAGNOSTICO_1>='D50' AND DIAGNOSTICO_1<='D899' THEN 'CAPÍTULO 03'
  WHEN DIAGNOSTICO_1>='E00' AND DIAGNOSTICO_1<='E909' THEN 'CAPÍTULO 04'
  WHEN DIAGNOSTICO_1>='F00' AND DIAGNOSTICO_1<='F999' THEN 'CAPÍTULO 05'
  WHEN DIAGNOSTICO_1>='G00' AND DIAGNOSTICO_1<='G999' THEN 'CAPÍTULO 06'
  WHEN DIAGNOSTICO_1>='H00' AND DIAGNOSTICO_1<='H599' THEN 'CAPÍTULO 07'
  WHEN DIAGNOSTICO_1>='H60' AND DIAGNOSTICO_1<='H959' THEN 'CAPÍTULO 08'
  WHEN DIAGNOSTICO_1>='I00' AND DIAGNOSTICO_1<='I999' THEN 'CAPÍTULO 09'
  WHEN DIAGNOSTICO_1>='J00' AND DIAGNOSTICO_1<='J999' THEN 'CAPÍTULO 10'
  WHEN DIAGNOSTICO_1>='K00' AND DIAGNOSTICO_1<='K939' THEN 'CAPÍTULO 11'
  WHEN DIAGNOSTICO_1>='L00' AND DIAGNOSTICO_1<='L999' THEN 'CAPÍTULO 12'
  WHEN DIAGNOSTICO_1>='M00' AND DIAGNOSTICO_1<='M999' THEN 'CAPÍTULO 13'
  WHEN DIAGNOSTICO_1>='N00' AND DIAGNOSTICO_1<='N999' THEN 'CAPÍTULO 14'
  WHEN DIAGNOSTICO_1>='O00' AND DIAGNOSTICO_1<='O999' THEN 'CAPÍTULO 15'
  WHEN DIAGNOSTICO_1>='P00' AND DIAGNOSTICO_1<='P966' THEN 'CAPÍTULO 16'
  WHEN DIAGNOSTICO_1>='Q00' AND DIAGNOSTICO_1<='Q999' THEN 'CAPÍTULO 17'
  WHEN DIAGNOSTICO_1>='R00' AND DIAGNOSTICO_1<='R999' THEN 'CAPÍTULO 18'
  WHEN DIAGNOSTICO_1>='S00' AND DIAGNOSTICO_1<='T988' THEN 'CAPÍTULO 19'
  WHEN DIAGNOSTICO_1>='V01' AND DIAGNOSTICO_1<='Y988' THEN 'CAPÍTULO 20'
  WHEN DIAGNOSTICO_1>='Z00' AND DIAGNOSTICO_1<='Z999' THEN 'CAPÍTULO 21'
  WHEN DIAGNOSTICO_1>='U00' AND DIAGNOSTICO_1<='U999' THEN 'CAPÍTULO 22'
ELSE 'OTRO' END) AS CIE10, 
(case
 WHEN  EDAD < 0  THEN 'G-'
 WHEN  EDAD >= 0 AND   EDAD <= 15 THEN 'G1'
 WHEN  EDAD >= 16 AND  EDAD <= 25 THEN 'G2'
 WHEN  EDAD >= 26 AND  EDAD <= 35 THEN 'G3'
 WHEN  EDAD >= 36 AND  EDAD <= 45 THEN 'G4'
 WHEN  EDAD >= 46 AND  EDAD <= 55 THEN 'G5'
 WHEN  EDAD >= 56 AND  EDAD <= 65 THEN 'G6'
 WHEN  EDAD >= 66 AND  EDAD <= 75 THEN 'G7'
 WHEN  EDAD >= 76 AND  EDAD <= 85 THEN 'G8'
 ELSE 'G9' end) as grupo_edad, 
  
 (case
 WHEN trim(DES_AREA) = 'DISPENSARIOS ANEXOS'  THEN 'AMB' 
 WHEN trim(DES_AREA) = 'EMERGENCIAS'  THEN 'EM' 
 WHEN trim(DES_AREA) = 'DPTO. MEDICINA'  THEN 'DEPM' 
 WHEN trim(DES_AREA) =' HOSPITALIZACION'  THEN 'HO' 
 ELSE 'OTRO' end) as grupo_tipo_serv  
 FROM 

(
SELECT des_area,EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, TO_NUMBER(EDAD_BENEFICIARIO) AS EDAD, diagnostico_1,genero, valor_total, identificacion_beneficiario, tipo_af FROM AS400.BD_FACT_2013 WHERE TIPO_AF IN ('SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') UNION
SELECT des_area,EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, TO_NUMBER(EDAD_BENEFICIARIO) AS EDAD, diagnostico_1,genero, valor_total, identificacion_beneficiario, tipo_af FROM AS400.BD_FACT_2014 WHERE TIPO_AF IN ('SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') UNION
SELECT des_area,EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, TO_NUMBER(EDAD_BENEFICIARIO)AS EDAD, diagnostico_1,genero, valor_total, identificacion_beneficiario, tipo_af FROM AS400.BD_FACT_2015 WHERE TIPO_AF IN ('SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') UNION
SELECT des_area,EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, TO_NUMBER(EDAD_BENEFICIARIO)AS EDAD, diagnostico_1,genero, valor_total, identificacion_beneficiario, tipo_af FROM AS400.BD_FACT_2016 WHERE TIPO_AF IN ('SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') UNION
SELECT des_area,EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, TO_NUMBER(EDAD_BENEFICIARIO)AS EDAD, diagnostico_1,genero, valor_total, identificacion_beneficiario, tipo_af FROM AS400.BD_FACT_2017 WHERE TIPO_AF IN ('SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ') UNION
SELECT des_area,EXTRACT (YEAR FROM TO_DATE(FECHA_ATENCION, 'DD/MM/YYYY')) AS ANIO, TO_NUMBER(EDAD_BENEFICIARIO)AS EDAD, diagnostico_1,genero, valor_total, identificacion_beneficiario, tipo_af FROM AS400.BD_FACT_2018 WHERE TIPO_AF IN ('SG', 'SV', 'CV', 'SC', 'JT', 'SA', 'SI', 'MP', 'SD' , 'SP' ,'MJ' , 'TJ' , 'RP' , 'BE', 'CE', 'AT', 'MJ', 'SA' , 'M', 'B', 'J', 'C', 'S','JU', 'MO', 'VI', 'J','M', 'AG','NI','AH','A', 'N6','M6', 'CY', 'AR', 'HJ')
)
)
)
GROUP BY anio, SEXO,CIE10, GRUPO_EDAD, grupo_tipo_serv



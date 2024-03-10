# Analisis de datos TBC

# Activacion de los paquetes:

library(tidyverse)
library(googlesheets4)
library(janitor)
library(lubridate)
library(googledrive)
library(readxl)
library(writexl)
library(beepr)
library(eeptools)
library(stringr)
library(dplyr)

# limpia ambiente
rm(list = ls()) 
options(error = beep)

# definir variables globales
fecha_actualizacion <- today()

# autorizar usuario

gs4_auth(email = "soporte.cetec.pba@gmail.com")
drive_auth(email = "soporte.cetec.pba@gmail.com")

fecha_actualizacion <- today()

# descargamos la hoja modelo (donde ir bajando los datos)

hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "1ER_LLAMADO",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas

links <- read_sheet(ss = "1quDT_34ZTy9wtZUHHV0rNbilFdbLRv64CHTko-Aoztk",
                    sheet = "links",
                    col_types = "c")

# ahora usamos el for (comando de iteraci?n) para bajar todas las hojas:

for (i in 1:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "1ER_LLAMADO",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

hoja_trabajo <- hoja_modelo

#  transformamos base para trabajar:

hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_llamado = date(parse_date_time(hoja_trabajo$Fecha_llamado, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_proximo_llamado = date(parse_date_time(hoja_trabajo$Fecha_proximo_llamado, c("%d%m%Y", "%Y%m%d"))))

# Realizamos estadisticas de base de datos:

ESTUVO_O_ESTA_EN_TTO_PARA_TBC <- hoja_trabajo %>% filter(ESTUVO_O_ESTA_EN_TTO_PARA_TBC == "ESTA ACTUALMENTE EN TTO" | ESTUVO_O_ESTA_EN_TTO_PARA_TBC == "NUNCA ESTUVO EN TTO" | ESTUVO_O_ESTA_EN_TTO_PARA_TBC== "ESTUVO EN TTO Y DISCONTINUO" | ESTUVO_O_ESTA_EN_TTO_PARA_TBC == "ESTUVO EN TTO Y LE DIERON EL ALTA") %>% count(ESTUVO_O_ESTA_EN_TTO_PARA_TBC,CETEC)

sheet_write(ESTUVO_O_ESTA_EN_TTO_PARA_TBC,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ESTUVO_O_ESTA_EN_TTO_PARA_TBC")

base_tbc <- hoja_trabajo %>% filter( DNI != "") %>% count(CETEC) %>% arrange(CETEC)

# para informe se necesita los casos del central repartidos en sus RS:


sheet_write(base_tbc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "base")

# Personas sin llamar:

sin_llamar <- hoja_trabajo %>% filter(Fallecidx == "NO" | is.na(Fallecidx) | Fallecidx=="No") %>% filter(Estado_del_llamado == "Sin llamar")  %>% count(CETEC) 

sheet_write(sin_llamar,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Sin llamar")


# Estado del llamado:

contactados <- hoja_trabajo %>% filter( DNI != "") %>%  count(CETEC, Estado_del_llamado) %>%  arrange(CETEC)

sheet_write(contactados,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "contactados")


# Fallidos:

Fallidos_1er_contacto <- hoja_trabajo %>% count(CETEC, CANTIDAD_DE_FALLIDOS)

sheet_write(Fallidos_1er_contacto,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Fallidos_1er_contacto")


# Fallecidos

Fallecidos <- hoja_trabajo %>% filter (Fallecidx=="SI" ) %>%  count(CETEC)

sheet_write(Fallecidos,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Fallecidxs")

# Llamados por mes por CETEC

diciembre_cetec <- hoja_trabajo %>% filter(Fecha_llamado  >= "2022-12-01" & Fecha_llamado <= "2022-12-31") %>% count(Estado_del_llamado,CANTIDAD_DE_FALLIDOS,CETEC)

sheet_write(diciembre_cetec,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Diciembre_cetec")

enero_cetec <- hoja_trabajo %>% filter(Fecha_llamado  >= "2023-01-01" & Fecha_llamado <= "2023-01-31") %>% count(Estado_del_llamado,CANTIDAD_DE_FALLIDOS,CETEC)

sheet_write(enero_cetec,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Enero_cetec")

febrero_cetec <- hoja_trabajo %>% filter(Fecha_llamado  >= "2023-02-01" & Fecha_llamado <= "2023-02-28") %>% count(Estado_del_llamado,CANTIDAD_DE_FALLIDOS,CETEC)

sheet_write(febrero_cetec,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Febrero_cetec")

colnames(hoja_trabajo)

# Info adicional:

Hoja_test <- hoja_trabajo %>% rename(INFO = 'Info adicional sobre la condición del tratamiento (proporcionada por la región) (embarazo/DBT/VIH)')

info_adicional <- Hoja_test %>% count(INFO)

sheet_write(info_adicional,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "info_adicional")

# estadisticas del tratamiento:


tipo_tto <- hoja_trabajo %>% filter( DNI != "") %>% filter(Estado_del_llamado == "Contactadx(pasa a seguimiento)" | Estado_del_llamado == "Tiene el alta(caso finalizado)" ) %>% count(ESTUVO_O_ESTA_EN_TTO_PARA_TBC,CETEC)


sheet_write(tipo_tto,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "tipo_tto")


# División por casos: (ver si hay que juntar cetec central con los demas, pero todavía No empezaron estos llamados)

#-------------------------------- Esta actualmente en tto ---------------------------

# hacemos el for para bajar las hojas de esta pestaña:



hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "ACTUALMENTE_TTO",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas


for (i in 1:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "ACTUALMENTE_TTO",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

hoja_trabajo <- hoja_modelo

# ahora realizamos analisis de casos:

colnames(hoja_trabajo)


# cantidad de personas en este grupo:

cantidad_tto <- hoja_trabajo  %>% filter( DNI != "") %>% count(CETEC)

sheet_write(cantidad_tto,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "cantidad_tto")

#hace cuanto tiempo inicio el tratamiento: (hay que renombrar, R de mierda)

hoja_trabajo <-  hoja_trabajo %>% rename(TIEMPO_TTO = 'HACE_CUANTO_TIEMPO_QUE_INICIO_EL_TTO(MESES)')



tiempo_tto <- hoja_trabajo  %>% filter( DNI != "") %>% count(TIEMPO_TTO)


sheet_write(tiempo_tto,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "tiempo tto en meses")

# SABE_QUE_TTO_ESTA_RECIBIENDO:

sabe <- hoja_trabajo  %>% filter( DNI != "") %>% count(SABE_QUE_TTO_ESTA_RECIBIENDO)

sheet_write(sabe,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "sabe")

# SI_LA_RTA_FUE_SI_INDICAR_CUAL:

SI_LA_RTA_FUE_SI_INDICAR_CUAL <- hoja_trabajo  %>% filter( DNI != "") %>% count(SI_LA_RTA_FUE_SI_INDICAR_CUAL)

sheet_write(SI_LA_RTA_FUE_SI_INDICAR_CUAL,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_FUE_SI_INDICAR_CUAL")

# DONDE_REALIZA_EL_TTO

DONDE_REALIZA_EL_TTO <- hoja_trabajo  %>% filter( DNI != "") %>% count(DONDE_REALIZA_EL_TTO...20)

sheet_write(DONDE_REALIZA_EL_TTO,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "DONDE_REALIZA_EL_TTO")

# SI_LA_RTA_ES_ESTA_HOSPITALIZADX_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_SE_ENCUENTRA

SI_LA_RTA_ES_ESTA_HOSPITALIZADX_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_SE_ENCUENTRA <- hoja_trabajo  %>% filter( DNI != "") %>% count(SI_LA_RTA_ES_ESTA_HOSPITALIZADX_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_SE_ENCUENTRA)

sheet_write(SI_LA_RTA_ES_ESTA_HOSPITALIZADX_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_SE_ENCUENTRA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_ES_ESTA_HOSPITALIZADX_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_SE_ENCUENTRA")

# RTA_SI_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_ESTUVO_INTERNADX

RTA_SI_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_ESTUVO_INTERNADX <- hoja_trabajo  %>% filter( DNI != "") %>% count(SI_LA_RTA_ES_ESTA_HOSPITALIZADX_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_SE_ENCUENTRA)

sheet_write(RTA_SI_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_ESTUVO_INTERNADX,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "RTA_SI_EN_QUE_HOSPITAL_EFECTOR_DE_SALUD_ESTUVO_INTERNADX")

# SE_REALIZA_CONTROLES


controles <- hoja_trabajo %>% filter( DNI != "") %>% count(SE_REALIZA_CONTROLES)

sheet_write(controles,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "controles")

#SI_NO_SE_CONTROLA_O_NO_LO_HACE_CON_REGULARIDAD_INDICAR_MOTIVOS 


motivos_no_controles <- hoja_trabajo %>% filter( DNI != "") %>% filter(SE_REALIZA_CONTROLES == "No se controla" | SE_REALIZA_CONTROLES == "No se controla con regularidad") %>% count(SI_NO_SE_CONTROLA_O_NO_LO_HACE_CON_REGULARIDAD_INDICAR_MOTIVOS)


sheet_write(motivos_no_controles,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "motivos_no_controles")

# SI_SE_CONTROLA_TIENE_FECHA_DE_PROXIMO_CONTROL:


SI_SE_CONTROLA_TIENE_FECHA_DE_PROXIMO_CONTROL <-  hoja_trabajo %>% filter( DNI != "") %>% filter(SE_REALIZA_CONTROLES == "Cada 15 dÃ­as" | SE_REALIZA_CONTROLES == "Cada mes")  %>% filter( DNI != "") %>% count(SI_SE_CONTROLA_TIENE_FECHA_DE_PROXIMO_CONTROL)

sheet_write(SI_SE_CONTROLA_TIENE_FECHA_DE_PROXIMO_CONTROL ,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_SE_CONTROLA_TIENE_FECHA_DE_PROXIMO_CONTROL ")                                                                                                                                                                                                                


# CONVIVIENTES:

hoja_trabajo <- hoja_trabajo %>% rename(CONVIVIENTES = "NOMBRE, APELLIDO  Y DNI DE CONVIVENTES")



convivientes <- hoja_trabajo %>% filter( DNI != "") %>% count(CONVIVIENTES)

sheet_write(convivientes,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "convivientes")  

# ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC:

sintomas_convivientes <- hoja_trabajo %>% filter(DNI != "") %>% count(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC)

sheet_write(sintomas_convivientes,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "sintomas_convivientes")

# CONVIVIENTE_TRATAMIENTO

# cambiamos nombre columna

prueba_2 <- hoja_trabajo %>% rename(TTO_CONVIVIENTES = 'SI_LA_RTA_FUE_SI_REALIZA/REALIZO_TTO')

conviviente_tratamiento <- prueba_2 %>% filter(DNI != "") %>%  filter(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC == "SI") %>% count(TTO_CONVIVIENTES)

sheet_write(conviviente_tratamiento,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "conviviente_tratamiento")

# ¿Cual tratamiento?

colnames(prueba_2)

prueba_3 <- prueba_2 %>% rename(CUAL_TTO_CON = 'EN_CASO_DE_QUE_LA_RTA_SEA_SI_INDICAR _CUAL...37')

conviviente_cual_tratamiento <- prueba_3 %>% filter(DNI != "") %>%  filter(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC == "SI") %>% filter(TTO_CONVIVIENTES == "Si (tratamiento y control)" | TTO_CONVIVIENTES == "Si (solo tratamiento)" ) %>% count(CUAL_TTO_CON)

sheet_write(conviviente_cual_tratamiento,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "conviviente_cual_tratamiento")

# LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE


solicitud_datos <- hoja_trabajo  %>% filter(DNI != "") %>%  count(LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE)

sheet_write(solicitud_datos,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "solicitud_datos")

# INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE

menores <- hoja_trabajo  %>% filter(DNI != "") %>% count(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE)

sheet_write(menores,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "menores")

# ¿Los menores reciben tto o control?

colnames(hoja_trabajo)

control_menores <- hoja_trabajo %>% filter(DNI != "") %>% filter(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE != 0 & !is.na(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE)) %>% count(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES...41)

sheet_write(control_menores,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "control_menores")

# INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE

mayores_caso1 <- hoja_trabajo  %>% filter(DNI != "") %>% count(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE)

sheet_write(mayores_caso1,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "mayores_caso1")

# ¿Los mayores reciben tto o control?

colnames(hoja_trabajo)

control_mayores_caso1 <- hoja_trabajo %>% filter(DNI != "") %>% filter(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE != 0 & !is.na(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE)) %>% count(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES...43)

sheet_write(control_mayores_caso1,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "control_mayores_caso1")

# ¿Algun conviviente tiene sintomas respiratorios?

prueba_4 <- hoja_trabajo %>% rename(CONVI_RESP = '¿TIENE_SINTOMAS_RESPIRATORIOS_?')

convi_resp <- prueba_4 %>% filter(DNI != "") %>% filter(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC == "SI") %>% count(CONVI_RESP)

sheet_write(convi_resp,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "convi_resp")


# preguntas sobre el paciente 

# esquema covid

esquema_covid <- hoja_trabajo %>% filter(DNI != "") %>% count(RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID)

sheet_write(esquema_covid,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "esquema_covid")

# ¿Tiene O.S o prepaga'

os<- hoja_trabajo %>% filter(DNI != "") %>% count(TIENE_OOSS_O_PREPAGA)

sheet_write(os,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "os")

# ¿reside en BA hace mas de 2 años?

residencia <- hoja_trabajo %>% filter(DNI != "") %>% filter(TIENE_OOSS_O_PREPAGA == "No") %>% count(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA)

sheet_write(residencia,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "residencia")

# ¿esta gestionando el subsidio?

hoja_prueba5 <- hoja_trabajo %>% rename(SUBSIDIO = 'SI_LA_ RESPUESTA_ES_SI_LE_GESTIONARON_EL_SUBSIDIO_DE_TBC')

subsidio <- hoja_prueba5 %>% filter(DNI != "") %>% filter(TIENE_OOSS_O_PREPAGA == "No") %>% filter(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA == "Si") %>% count(SUBSIDIO)

sheet_write(subsidio,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "subsidio")


# preguntas sobre embarazo:

# GESTACION / PUERPERIO

hoja_prueba6 <- hoja_trabajo %>% rename(puerperio = 'GESTANCION / PUERPERIO') %>% rename(embarazo = '¿CURSA EMBARAZO?') %>% rename(tiempo_embarazo = '¿CUANTO TIEMPO LLEVA DE EMBARAZO? (completar en semana o trimestre)') %>% rename(controles_embarazo = '¿CUANTOS CONTROLES DE EMBARAZO REALIZO HASTA EL MOMENTO ?') %>% rename(complicaciones = '¿TUVO ALGUNA COMPLICACION DURANTE EL PUERPERIO?') %>%  rename(cual = 'SI LA RESPUESTA ES SI: INDICAR CUÁL') %>% rename (amantar = '¿ESTAS_AMAMANTANDO?')

puerperio <- hoja_prueba6 %>% filter(DNI != "") %>% count(puerperio)

sheet_write(puerperio,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "puerperio")

embarazo <- hoja_prueba6 %>% filter(DNI != "") %>% count(embarazo)

sheet_write(embarazo,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "embarazo")

tiempo_embarazo <- hoja_prueba6 %>% filter(DNI != "") %>% count(tiempo_embarazo)

sheet_write(tiempo_embarazo,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "tiempo_embarazo")

controles_embarazo <- hoja_prueba6 %>% filter(DNI != "") %>% count(controles_embarazo)

sheet_write(controles_embarazo,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "controles_embarazo")

complicaciones <- hoja_prueba6 %>% filter(DNI != "") %>% count(complicaciones)

sheet_write(complicaciones,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "complicaciones")

cual <- hoja_prueba6 %>% filter(DNI != "") %>% count(cual)

sheet_write(cual,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "cual")

amantar <- hoja_prueba6 %>% filter(DNI != "") %>% count(amantar)

sheet_write(amantar,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "amantar")

colnames(hoja_trabajo)


# segundo llamado:

# ESTADO_DEL_LLAMADO

colnames(hoja_trabajo)

hoja_trabajo <- hoja_trabajo %>% rename(ESTADO_LLAMADO_2 = 'ESTADO_DEL_LLAMADO...62' )

estado_llamado_2 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTADO_LLAMADO_2,CETEC)

fallidos_2 <- hoja_trabajo %>% filter(DNI != "") %>% count(CANTIDAD_DE_FALLIDOS...63,CETEC)

FINALIZO_EL_TRATAMIENTO <- hoja_trabajo %>% filter(DNI != "") %>% count(FINALIZO_EL_TRATAMIENTO...64)

TIENE_EL_ALTA_MEDICA <- hoja_trabajo %>% filter(DNI != "") %>% count(TIENE_EL_ALTA_MEDICA...65)

SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...65 == "NO") %>% filter(DNI != "") %>% count(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...67)

# No pudo asistir a la consulta de control --- Motivos

# hoja_trabajo <- hoja_trabajo %>% rename(NO_MOTIVO = 'SI_LA_RTA_FUE_NO_DETALLAR_MOTIVO_ Y_LLENAR_FORM_...68') Cambiar nombres en la hoja modelo y de trabajo. No lo reconoce el R

# SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...65 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...84 == "NO") %>%  filter(DNI != "") %>% count(NO_MOTIVO)

# si fue, calificar calidad de atenciÃ³n

SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...65 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...84 == "SI") %>%  filter(DNI != "") %>% count(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN...69)

# QUE_INDICACION_LE_DIERON_DESARROLLAR

QUE_INDICACION_LE_DIERON_DESARROLLAR <- hoja_trabajo %>% filter(DNI != "") %>% count(QUE_INDICACION_LE_DIERON_DESARROLLAR...70)

#  ESTA_TOMANDO_LA_MEDICACION_INDICADA

ESTA_TOMANDO_LA_MEDICACION_INDICADA <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTA_TOMANDO_LA_MEDICACION_INDICADA...71)

# TENES_TURNO_PARA_PROXIMO_CONTROL

TENES_TURNO_PARA_PROXIMO_CONTROL <- hoja_trabajo %>% filter(DNI != "") %>% count(TENES_TURNO_PARA_PROXIMO_CONTROL...73)

# SE_APLICO_VACUNAS_ANTIGRIPAL_ANTINEUMOCOCICA

SE_APLICO_VACUNAS_ANTIGRIPAL_ANTINEUMOCOCICA <- hoja_trabajo %>% filter(DNI != "") %>% count(SE_APLICO_VACUNAS_ANTIGRIPAL_ANTINEUMOCOCICA)

sheet_write(estado_llamado_2,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estado_llamado_2")

sheet_write(fallidos_2,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "fallidos_2")

sheet_write(FINALIZO_EL_TRATAMIENTO,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "FINALIZO_EL_TRATAMIENTO")

sheet_write(TIENE_EL_ALTA_MEDICA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_EL_ALTA_MEDICA")

sheet_write(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL")

sheet_write(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN")

sheet_write(QUE_INDICACION_LE_DIERON_DESARROLLAR,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "QUE_INDICACION_LE_DIERON_DESARROLLAR")

sheet_write(ESTA_TOMANDO_LA_MEDICACION_INDICADA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ESTA_TOMANDO_LA_MEDICACION_INDICADA")

sheet_write(TENES_TURNO_PARA_PROXIMO_CONTROL,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TENES_TURNO_PARA_PROXIMO_CONTROL")

sheet_write(SE_APLICO_VACUNAS_ANTIGRIPAL_ANTINEUMOCOCICA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SE_APLICO_VACUNAS_ANTIGRIPAL_ANTINEUMOCOCICA")

# Tercer llamado:


colnames(hoja_trabajo)

hoja_trabajo <- hoja_trabajo %>% rename(ESTADO_LLAMADO_3 = 'ESTADO_DEL_LLAMADO...79' )

estado_llamado_3 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTADO_LLAMADO_3,CETEC)

fallidos_3 <- hoja_trabajo %>% filter(DNI != "") %>% count(CANTIDAD_DE_FALLIDOS...80,CETEC)

FINALIZO_EL_TRATAMIENTO3 <- hoja_trabajo %>% filter(DNI != "") %>% count(FINALIZO_EL_TRATAMIENTO...81)

TIENE_EL_ALTA_MEDICA3 <- hoja_trabajo %>% filter(DNI != "") %>% count(TIENE_EL_ALTA_MEDICA...82)

SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL3 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...82 == "NO") %>% filter(DNI != "") %>% count(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...84)

# No pudo asistir a la consulta de control --- Motivos

# hoja_trabajo <- hoja_trabajo %>% rename(NO_MOTIVO = 'SI_LA_RTA_FUE_NO_DETALLAR_MOTIVO_ Y_LLENAR_FORM_...68') Cambiar nombres en la hoja modelo y de trabajo. No lo reconoce el R

#SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL3 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...82 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...84 == "NO") %>%  filter(DNI != "") %>% count(NO_MOTIVO)

# si fue, calificar calidad de atenciÃ³n

SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN3 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...82 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...84 == "SI") %>%  filter(DNI != "") %>% count(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN...86)

# QUE_INDICACION_LE_DIERON_DESARROLLAR

QUE_INDICACION_LE_DIERON_DESARROLLAR3 <- hoja_trabajo %>% filter(DNI != "") %>% count(QUE_INDICACION_LE_DIERON_DESARROLLAR...87)

#  ESTA_TOMANDO_LA_MEDICACION_INDICADA

ESTA_TOMANDO_LA_MEDICACION_INDICADA3 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTA_TOMANDO_LA_MEDICACION_INDICADA...88)

# TENES_TURNO_PARA_PROXIMO_CONTROL

TENES_TURNO_PARA_PROXIMO_CONTROL3 <- hoja_trabajo %>% filter(DNI != "") %>% count(TENES_TURNO_PARA_PROXIMO_CONTROL...90)

sheet_write(estado_llamado_3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estado_llamado_3")

sheet_write(fallidos_3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "fallidos_3")

sheet_write(FINALIZO_EL_TRATAMIENTO3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "FINALIZO_EL_TRATAMIENTO3")

sheet_write(TIENE_EL_ALTA_MEDICA3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_EL_ALTA_MEDICA3")

sheet_write(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL3")

sheet_write(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN3")

sheet_write(QUE_INDICACION_LE_DIERON_DESARROLLAR3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "QUE_INDICACION_LE_DIERON_DESARROLLAR3")

sheet_write(ESTA_TOMANDO_LA_MEDICACION_INDICADA3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ESTA_TOMANDO_LA_MEDICACION_INDICADA3")

sheet_write(TENES_TURNO_PARA_PROXIMO_CONTROL3,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TENES_TURNO_PARA_PROXIMO_CONTROL3")


# Cuarto llamado:


colnames(hoja_trabajo)

hoja_trabajo <- hoja_trabajo %>% rename(ESTADO_LLAMADO_4 = 'ESTADO_DEL_LLAMADO...95' )

estado_llamado_4 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTADO_LLAMADO_4,CETEC)

fallidos_4 <- hoja_trabajo %>% filter(DNI != "") %>% count(CANTIDAD_DE_FALLIDOS...96,CETEC)

FINALIZO_EL_TRATAMIENTO4 <- hoja_trabajo %>% filter(DNI != "") %>% count(FINALIZO_EL_TRATAMIENTO...97)

TIENE_EL_ALTA_MEDICA4 <- hoja_trabajo %>% filter(DNI != "") %>% count(TIENE_EL_ALTA_MEDICA...98)

SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL4 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...98 == "NO") %>% filter(DNI != "") %>% count(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...100)

# No pudo asistir a la consulta de control --- Motivos

# hoja_trabajo <- hoja_trabajo %>% rename(NO_MOTIVO = 'SI_LA_RTA_FUE_NO_DETALLAR_MOTIVO_ Y_LLENAR_FORM_...68') Cambiar nombres en la hoja modelo y de trabajo. No lo reconoce el R

#SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL4 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...98 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...100 == "NO") %>%  filter(DNI != "") %>% count(NO_MOTIVO)

# si fue, calificar calidad de atenciÃ³n

SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN4 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...98 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...100 == "SI") %>%  filter(DNI != "") %>% count(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN...102)

# QUE_INDICACION_LE_DIERON_DESARROLLAR

QUE_INDICACION_LE_DIERON_DESARROLLAR4 <- hoja_trabajo %>% filter(DNI != "") %>% count(QUE_INDICACION_LE_DIERON_DESARROLLAR...103)

#  ESTA_TOMANDO_LA_MEDICACION_INDICADA

ESTA_TOMANDO_LA_MEDICACION_INDICADA4 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTA_TOMANDO_LA_MEDICACION_INDICADA...104)

# TENES_TURNO_PARA_PROXIMO_CONTROL

TENES_TURNO_PARA_PROXIMO_CONTROL4 <- hoja_trabajo %>% filter(DNI != "") %>% count(TENES_TURNO_PARA_PROXIMO_CONTROL...106)

sheet_write(estado_llamado_4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estado_llamado_4")

sheet_write(fallidos_4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "fallidos_4")

sheet_write(FINALIZO_EL_TRATAMIENTO4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "FINALIZO_EL_TRATAMIENTO4")

sheet_write(TIENE_EL_ALTA_MEDICA4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_EL_ALTA_MEDICA4")

sheet_write(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL4")

sheet_write(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN4")

sheet_write(QUE_INDICACION_LE_DIERON_DESARROLLAR4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "QUE_INDICACION_LE_DIERON_DESARROLLAR4")

sheet_write(ESTA_TOMANDO_LA_MEDICACION_INDICADA4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ESTA_TOMANDO_LA_MEDICACION_INDICADA4")

sheet_write(TENES_TURNO_PARA_PROXIMO_CONTROL4,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TENES_TURNO_PARA_PROXIMO_CONTROL4")

# Quinto llamado:


colnames(hoja_trabajo)

hoja_trabajo <- hoja_trabajo %>% rename(ESTADO_LLAMADO_5 = 'ESTADO_DEL_LLAMADO...111' )

estado_llamado_5 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTADO_LLAMADO_5,CETEC)

fallidos_5 <- hoja_trabajo %>% filter(DNI != "") %>% count(CANTIDAD_DE_FALLIDOS...112,CETEC)

FINALIZO_EL_TRATAMIENTO5 <- hoja_trabajo %>% filter(DNI != "") %>% count(FINALIZO_EL_TRATAMIENTO...113)

TIENE_EL_ALTA_MEDICA5 <- hoja_trabajo %>% filter(DNI != "") %>% count(TIENE_EL_ALTA_MEDICA...114)

SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL5 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...114 == "NO") %>% filter(DNI != "") %>% count(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...116)

# No pudo asistir a la consulta de control --- Motivos

# hoja_trabajo <- hoja_trabajo %>% rename(NO_MOTIVO = 'SI_LA_RTA_FUE_NO_DETALLAR_MOTIVO_ Y_LLENAR_FORM_...68') Cambiar nombres en la hoja modelo y de trabajo. No lo reconoce el R

#SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL5 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...114 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...116 == "NO") %>%  filter(DNI != "") %>% count(NO_MOTIVO)

# si fue, calificar calidad de atenciÃ³n

SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN5 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...114 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...116 == "SI") %>%  filter(DNI != "") %>% count(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN...118)

# QUE_INDICACION_LE_DIERON_DESARROLLAR

QUE_INDICACION_LE_DIERON_DESARROLLAR5 <- hoja_trabajo %>% filter(DNI != "") %>% count(QUE_INDICACION_LE_DIERON_DESARROLLAR...119)

#  ESTA_TOMANDO_LA_MEDICACION_INDICADA

ESTA_TOMANDO_LA_MEDICACION_INDICADA5 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTA_TOMANDO_LA_MEDICACION_INDICADA...120)

# TENES_TURNO_PARA_PROXIMO_CONTROL

TENES_TURNO_PARA_PROXIMO_CONTROL5 <- hoja_trabajo %>% filter(DNI != "") %>% count(TENES_TURNO_PARA_PROXIMO_CONTROL...122)

sheet_write(estado_llamado_5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estado_llamado_5")

sheet_write(fallidos_5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "fallidos_5")

sheet_write(FINALIZO_EL_TRATAMIENTO5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "FINALIZO_EL_TRATAMIENTO5")

sheet_write(TIENE_EL_ALTA_MEDICA5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_EL_ALTA_MEDICA5")

sheet_write(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL5")

sheet_write(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN5")

sheet_write(QUE_INDICACION_LE_DIERON_DESARROLLAR5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "QUE_INDICACION_LE_DIERON_DESARROLLAR5")

sheet_write(ESTA_TOMANDO_LA_MEDICACION_INDICADA5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ESTA_TOMANDO_LA_MEDICACION_INDICADA5")

sheet_write(TENES_TURNO_PARA_PROXIMO_CONTROL5,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TENES_TURNO_PARA_PROXIMO_CONTROL5")

# Sexto llamado:


colnames(hoja_trabajo)

hoja_trabajo <- hoja_trabajo %>% rename(ESTADO_LLAMADO_6 = 'ESTADO_DEL_LLAMADO...127' )

estado_llamado_6 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTADO_LLAMADO_6,CETEC)

fallidos_6 <- hoja_trabajo %>% filter(DNI != "") %>% count(CANTIDAD_DE_FALLIDOS...128,CETEC)

FINALIZO_EL_TRATAMIENTO6 <- hoja_trabajo %>% filter(DNI != "") %>% count(FINALIZO_EL_TRATAMIENTO...129)

TIENE_EL_ALTA_MEDICA6 <- hoja_trabajo %>% filter(DNI != "") %>% count(TIENE_EL_ALTA_MEDICA...130)

SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL6 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...130 == "NO") %>% filter(DNI != "") %>% count(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...132)

# No pudo asistir a la consulta de control --- Motivos

# hoja_trabajo <- hoja_trabajo %>% rename(NO_MOTIVO = 'SI_LA_RTA_FUE_NO_DETALLAR_MOTIVO_ Y_LLENAR_FORM_...68') Cambiar nombres en la hoja modelo y de trabajo. No lo reconoce el R

#SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL6 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...130 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...132 == "NO") %>%  filter(DNI != "") %>% count(NO_MOTIVO)

# si fue, calificar calidad de atenciÃ³n

SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN6 <- hoja_trabajo %>% filter(TIENE_EL_ALTA_MEDICA...130 == "NO") %>% filter(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL...132 == "SI") %>%  filter(DNI != "") %>% count(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN...134)

# QUE_INDICACION_LE_DIERON_DESARROLLAR

QUE_INDICACION_LE_DIERON_DESARROLLAR6 <- hoja_trabajo %>% filter(DNI != "") %>% count(QUE_INDICACION_LE_DIERON_DESARROLLAR...135)

#  ESTA_TOMANDO_LA_MEDICACION_INDICADA

ESTA_TOMANDO_LA_MEDICACION_INDICADA6 <- hoja_trabajo %>% filter(DNI != "") %>% count(ESTA_TOMANDO_LA_MEDICACION_INDICADA...136)

# TENES_TURNO_PARA_PROXIMO_CONTROL

TENES_TURNO_PARA_PROXIMO_CONTROL6 <- hoja_trabajo %>% filter(DNI != "") %>% count(TENES_TURNO_PARA_PROXIMO_CONTROL...138)

sheet_write(estado_llamado_6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estado_llamado_6")

sheet_write(fallidos_6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "fallidos_6")

sheet_write(FINALIZO_EL_TRATAMIENTO6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "FINALIZO_EL_TRATAMIENTO6")

sheet_write(TIENE_EL_ALTA_MEDICA6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_EL_ALTA_MEDICA6")

sheet_write(SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_NO_TIENE_EL_ALTA_MEDICA_PUDISTE_ASISTIR_A_LA_CONSULTA_DE_CONTROL6")

sheet_write(SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_FUE_SI_CALIFICAR_LA_ATENCIÓN6")

sheet_write(QUE_INDICACION_LE_DIERON_DESARROLLAR6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "QUE_INDICACION_LE_DIERON_DESARROLLAR6")

sheet_write(ESTA_TOMANDO_LA_MEDICACION_INDICADA6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ESTA_TOMANDO_LA_MEDICACION_INDICADA6")

sheet_write(TENES_TURNO_PARA_PROXIMO_CONTROL6,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TENES_TURNO_PARA_PROXIMO_CONTROL6")


# ------------------------ CASO NUMERO 2: NUNCA ESTUVO EN TTO ------------------------

hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "NUNCA_ESTUVO_EN_TTO",
                          col_types = "c")


# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas

links <- read_sheet(ss = "1quDT_34ZTy9wtZUHHV0rNbilFdbLRv64CHTko-Aoztk",
                    sheet = "links",
                    col_types = "c")

# ahora usamos el for (comando de iteraciÃ³n) para bajar todas las hojas:

for (i in 1:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "NUNCA_ESTUVO_EN_TTO",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

hoja_trabajo <- hoja_modelo


# Preguntas de la tarea

cantidad_nunca_tto <- hoja_trabajo  %>% filter( DNI != "") %>% count(CETEC)

sheet_write(cantidad_nunca_tto,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "abandono_tto")


# Motivos:

motivos <- hoja_trabajo  %>% filter( DNI != "") %>% count(INDICAR_MOTIVOS)


sheet_write(motivos,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "motivos")

# NECESITA_TURNO_PARA_CONTROL_MEDICO

NECESITA_TURNO_PARA_CONTROL_MEDICO <- hoja_trabajo  %>% filter( DNI != "") %>% count(NECESITA_TURNO_PARA_CONTROL_MEDICO)


sheet_write(NECESITA_TURNO_PARA_CONTROL_MEDICO,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "NECESITA_TURNO_PARA_CONTROL_MEDICO")

# convivientes

NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES <- hoja_trabajo  %>% filter( DNI != "") %>% count(NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES)


sheet_write(NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES")

# ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC

ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC <- hoja_trabajo  %>% filter( DNI != "") %>% count(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC)


sheet_write(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC")

SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO <- hoja_trabajo %>% filter(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC == "SI") %>% count(SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO)


sheet_write(SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO")

# INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE


INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE <- hoja_trabajo  %>% filter( DNI != "") %>% count(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE)


sheet_write(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE")


colnames(hoja_trabajo)

#RECIBEN_TTO_PREVENTIVO_Y_CONTROLES

RECIBEN_TTO_PREVENTIVO_Y_CONTROLES <- hoja_trabajo  %>% filter( DNI != "") %>% count(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES...31)


sheet_write(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "RECIBEN_TTO_PREVENTIVO_Y_CONTROLES")

# INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE


INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE <- hoja_trabajo  %>% filter( DNI != "") %>% count(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE)


sheet_write(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE_caso2")


colnames(hoja_trabajo)

#RECIBEN_TTO_PREVENTIVO_Y_CONTROLES

RECIBEN_TTO_PREVENTIVO_Y_CONTROLES <- hoja_trabajo  %>% filter( DNI != "") %>% count(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES...29)


sheet_write(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "RECIBEN_TTO_PREVENTIVO_Y_CONTROLES_menores_caso2")



TIENE_SINTOMAS_RESPIRATORIOS <- hoja_trabajo  %>% filter( DNI != "") %>% count(TIENE_SINTOMAS_RESPIRATORIOS)

sheet_write(TIENE_SINTOMAS_RESPIRATORIOS,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_SINTOMAS_RESPIRATORIOS")

RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID <- hoja_trabajo %>% count(RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID)

TIENE_OOSS_O_PREPAGA <- hoja_trabajo %>% count(TIENE_OOSS_O_PREPAGA)


SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA <- hoja_trabajo %>% filter(TIENE_OOSS_O_PREPAGA == "No") %>% count(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA)

prueba <- hoja_trabajo %>% rename(subsidio = 'SI_LA_ RESPUESTA_ES_SI_LE_GESTIONARON_EL_SUBSIDIO_DE_TBC')

subsidio <- prueba %>% filter(TIENE_OOSS_O_PREPAGA == "No") %>% filter(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA == "Si") %>% count(subsidio)


sheet_write(RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID")

sheet_write(TIENE_OOSS_O_PREPAGA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_OOSS_O_PREPAGA")

sheet_write(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA")

sheet_write(subsidio,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "subsidio_2")


# -------------------------------------------- CASO 3 ESTUVO EN TTO Y DESCONTINUO ----------------------------

rm(hoja_modelo)

hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "ESTUVO_EN TTO_Y_DISCONTINUO",
                          col_types = "c")


# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas

links <- read_sheet(ss = "1quDT_34ZTy9wtZUHHV0rNbilFdbLRv64CHTko-Aoztk",
                    sheet = "links",
                    col_types = "c")

# ahora usamos el for (comando de iteraciÃ³n) para bajar todas las hojas:

for (i in 1:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "ESTUVO_EN TTO_Y_DISCONTINUO",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

hoja_trabajo <- hoja_modelo


# Preguntas de la tarea

cantidad_tto_descontinuo <- hoja_trabajo  %>% filter( DNI != "") %>% count(CETEC)

sheet_write(cantidad_tto_descontinuo,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estuvo_tto_descontinuo")


# Preguntas CASO 3:


#  INDICAR_MOTIVOS_POR_EL_CUAL_DISCONTINUO_EL_TRATAMIENTO

INDICAR_MOTIVOS_POR_EL_CUAL_DISCONTINUO_EL_TRATAMIENTO <- hoja_trabajo %>% filter( DNI != "") %>% count(INDICAR_MOTIVOS_POR_EL_CUAL_DISCONTINUO_EL_TRATAMIENTO)

sheet_write(INDICAR_MOTIVOS_POR_EL_CUAL_DISCONTINUO_EL_TRATAMIENTO,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "INDICAR_MOTIVOS_POR_EL_CUAL_DISCONTINUO_EL_TRATAMIENTO")

# EN_QUE_FECHA_COMENZASTE_EL_TRATAMIENTO

EN_QUE_FECHA_COMENZASTE_EL_TRATAMIENTO <- hoja_trabajo %>% filter( DNI != "") %>% count(EN_QUE_FECHA_COMENZASTE_EL_TRATAMIENTO)

sheet_write(EN_QUE_FECHA_COMENZASTE_EL_TRATAMIENTO,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "EN_QUE_FECHA_COMENZASTE_EL_TRATAMIENTO")

# DONDE_REALIZASTE_EL_TRATAMIENTO

DONDE_REALIZASTE_EL_TRATAMIENTO <- hoja_trabajo %>% filter( DNI != "") %>% count(DONDE_REALIZASTE_EL_TRATAMIENTO)

sheet_write(DONDE_REALIZASTE_EL_TRATAMIENTO,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "DONDE_REALIZASTE_EL_TRATAMIENTO")

# NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES

NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES_f <- hoja_trabajo %>% filter( DNI != "") %>% count(NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES)

sheet_write(NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES_f,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES_f")

# ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC:

sintomas_convivientes_disc <- hoja_trabajo %>% filter(DNI != "") %>% count(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC)

sheet_write(sintomas_convivientes_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "sintomas_convivientes_disc")

# CONVIVIENTE_TRATAMIENTO

colnames(hoja_trabajo)

# cambiamos nombre columna

prueba_2 <- hoja_trabajo %>% rename(TTO_CONVIVIENTES = 'SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO')

conviviente_tratamiento_disc <- prueba_2 %>% filter(DNI != "") %>%  filter(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC == "SI") %>% count(TTO_CONVIVIENTES)

sheet_write(conviviente_tratamiento_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "conviviente_tratamiento_disc")

# ¿Cual tratamiento?

colnames(prueba_2)

prueba_3 <- prueba_2 %>% rename(CUAL_TTO_CON = 'EN_CASO_DE_QUE_LA_RTA_SEA_SI_INDICAR _CUAL...26')

conviviente_cual_tratamiento_disc <- prueba_3 %>% filter(DNI != "") %>%  filter(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC == "SI") %>% filter(TTO_CONVIVIENTES == "Si (tratamiento y control)" | TTO_CONVIVIENTES == "Si (solo tratamiento)" ) %>% count(CUAL_TTO_CON)

sheet_write(conviviente_cual_tratamiento_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "conviviente_cual_tratamiento_disc")

# LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE


solicitud_datos_disc <- hoja_trabajo  %>% filter(DNI != "") %>%  count(LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE)

sheet_write(solicitud_datos_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "solicitud_datos_disc")

# INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE

menores_disc <- hoja_trabajo  %>% filter(DNI != "") %>% count(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE)

sheet_write(menores_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "menores_disc")

# ¿Los menores reciben tto o control?

colnames(hoja_trabajo)

control_menores_disc <- hoja_trabajo %>% filter(DNI != "") %>% filter(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE != 0 & !is.na(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE)) %>% count(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES...30)

sheet_write(control_menores_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "control_menores_disc")

# INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE

mayores_disc <- hoja_trabajo  %>% filter(DNI != "") %>% count(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE)

sheet_write(menores_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "mayores_disc")

# ¿Los mayores reciben tto o control?

colnames(hoja_trabajo)

control_mayores_disc <- hoja_trabajo %>% filter(DNI != "") %>% filter(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE != 0 & !is.na(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE)) %>% count(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES...32)

sheet_write(control_menores_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "control_mayores_disc")


# ¿Algun conviviente tiene sintomas respiratorios?

prueba_4 <- hoja_trabajo %>% rename(CONVI_RESP = 'TIENE_SINTOMAS_RESPIRATORIOS')

convi_resp_disc <- prueba_4 %>% filter(DNI != "") %>% filter(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC == "SI") %>% count(CONVI_RESP)

sheet_write(convi_resp_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "convi_resp_disc")


# preguntas sobre el paciente 

# esquema covid

esquema_covid_disc <- hoja_trabajo %>% filter(DNI != "") %>% count(RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID)

sheet_write(esquema_covid_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "esquema_covid_disc")

# Â¿Tiene O.S o prepaga'

os_disc<- hoja_trabajo %>% filter(DNI != "") %>% count(TIENE_OOSS_O_PREPAGA)

sheet_write(os_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "os_disc")

# Â¿reside en BA hace mas de 2 aÃ±os?

residencia_disc <- hoja_trabajo %>% filter(DNI != "") %>% filter(TIENE_OOSS_O_PREPAGA == "No") %>% count(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA)

sheet_write(residencia_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "residencia_disc")

# ¿esta gestionando el subsidio?

hoja_prueba5 <- hoja_trabajo %>% rename(SUBSIDIO = 'SI_LA_ RESPUESTA_ES_SI_LE_GESTIONARON_EL_SUBSIDIO_DE_TBC')

subsidio_disc <- hoja_prueba5 %>% filter(DNI != "") %>% filter(TIENE_OOSS_O_PREPAGA == "No") %>% filter(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA == "Si") %>% count(SUBSIDIO)

sheet_write(subsidio_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "subsidio_disc")

# preguntas sobre embarazo:


# GESTANCION / PUERPERIO

hoja_prueba6 <- hoja_trabajo %>% rename(puerperio = 'GESTANCION / PUERPERIO') %>% rename(embarazo = 'Â¿CURSA EMBARAZO?') %>% rename(tiempo_embarazo = 'Â¿CUANTO TIEMPO LLEVA DE EMBARAZO? (completar en semana o trimestre)') %>% rename(controles_embarazo = 'Â¿CUANTOS CONTROLES DE EMBARAZO REALIZO HASTA EL MOMENTO ?') %>% rename(complicaciones = 'Â¿TUVO ALGUNA COMPLICACION DURANTE EL PUERPERIO?') %>%  rename(cual = 'SI LA RESPUESTA ES SI: INDICAR CUÁL') %>% rename (amantar = 'Â¿ESTAS_AMAMANTANDO_?')

puerperio_disc <- hoja_prueba6 %>% filter(DNI != "") %>% count(puerperio)

sheet_write(puerperio_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "puerperio_disc")

embarazo_disc <- hoja_prueba6 %>% filter(DNI != "") %>% count(embarazo)

sheet_write(embarazo_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "embarazo_disc")

tiempo_embarazo_disc <- hoja_prueba6 %>% filter(DNI != "") %>% count(tiempo_embarazo)

sheet_write(tiempo_embarazo_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "tiempo_embarazo_disc")

controles_embarazo_disc <- hoja_prueba6 %>% filter(DNI != "") %>% count(controles_embarazo)

sheet_write(controles_embarazo_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "controles_embarazo_disc")

complicaciones_disc <- hoja_prueba6 %>% filter(DNI != "") %>% count(complicaciones)

sheet_write(complicaciones_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "complicaciones_disc")

cual_disc <- hoja_prueba6 %>% filter(DNI != "") %>% count(cual)

sheet_write(cual_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "cual_disc")

amantar_disc <- hoja_prueba6 %>% filter(DNI != "") %>% count(amantar)

sheet_write(amantar_disc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "amantar_disc")



# -------------------------------------------- CASO 4 ESTUVO_EN_TTO_Y_LE_DIERON_EL_ALTA ----------------------------


hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "ESTUVO_EN_TTO_Y_LE_DIERON_EL_ALTA",
                          col_types = "c")


# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas

links <- read_sheet(ss = "1quDT_34ZTy9wtZUHHV0rNbilFdbLRv64CHTko-Aoztk",
                    sheet = "links",
                    col_types = "c")

# ahora usamos el for (comando de iteraciÃ³n) para bajar todas las hojas:

for (i in 1:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "ESTUVO_EN_TTO_Y_LE_DIERON_EL_ALTA",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

hoja_trabajo <- hoja_modelo


# Preguntas de la tarea

cantidad_tto_alta <- hoja_trabajo  %>% filter( DNI != "") %>% count(CETEC)

sheet_write(cantidad_tto_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estuvo_tto_alta")


#LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA:

LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA <- hoja_trabajo %>% filter( DNI != "") %>% count(LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA)

sheet_write(LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA")

# FECHA_DE_FINALIZACION_DE_TRATAMIENTO/ALTA_MEDICA

LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA <- hoja_trabajo %>% filter( DNI != "") %>% count(LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA)

sheet_write(LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA")

prueba_alta <- hoja_trabajo %>% rename(fecha_alta = 'FECHA_DE_FINALIZACION_DE_TRATAMIENTO/ALTA_MEDICA')

FECHA_ALTA_MEDICA <- prueba_alta %>% filter(LE_PACIENTE_REFIERE_TENER_EL_ALTA_MEDICA == "SI") %>% filter( DNI != "") %>% count(fecha_alta)

sheet_write(FECHA_ALTA_MEDICA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "FECHA_ALTA_MEDICA")

# PPROFESIONAL_TRATANTE/CENTRO_DE_SALUD_DE_REFERENCIA

prueba_doc <- hoja_trabajo %>% rename(doc_doc = 'PPROFESIONAL_TRATANTE/CENTRO_DE_SALUD_DE_REFERENCIA')


doc <- prueba_doc %>% filter( DNI != "") %>% count(doc_doc)

sheet_write(doc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "PPROFESIONAL_TRATANTE/CENTRO_DE_SALUD_DE_REFERENCIA")

# NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES

NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES_ALTA <- hoja_trabajo %>% filter( DNI != "") %>% count(NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES)

sheet_write(NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES_ALTA,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "NOMBRE_APELLIDO_DNI_TELEFONO_DE_CONVIVENTES_ALTA")

# ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC

ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC_Alta <- hoja_trabajo %>% filter( DNI != "") %>% count(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC)

sheet_write(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC_Alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC_Alta")


#  SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO_alta

SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO_alta <- hoja_trabajo %>% filter( DNI != "") %>% filter(ALGUNX_DE_SUS_FAMILIARES_CONVIVIENTES_TIENE_TUVO_DX_O_SINTOMAS_DE_TBC == "SI") %>% count(SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO)

sheet_write(SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RTA_FUE_SI_REALIZA_REALIZO_TTO_alta")

# LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE

LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE_Alta <- hoja_trabajo %>% filter( DNI != "") %>% count(LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE)

sheet_write(LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE_Alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "LE_SOLICITARON_LOS_DATOS_DESDE_EL_EFECTOR_DE_SALUD_DONDE_USTED_SE_ATIENDE_Alta")


INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE_alta <- hoja_trabajo %>% filter( DNI != "") %>% count(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE)

sheet_write(INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "INDIQUE_CUANTOS_CONVIVIENTES_MENORES_DE_19_AÑOS_TIENE_alta")

colnames(hoja_trabajo)

RECIBEN_TTO_PREVENTIVO_Y_CONTROLES_alta <- hoja_trabajo %>% filter( DNI != "") %>% count(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES...29)

sheet_write(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "RECIBEN_TTO_PREVENTIVO_Y_CONTROLES_alta")

INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE_alta <- hoja_trabajo %>% filter( DNI != "") %>% count(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE)

sheet_write(INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "INDIQUE_CUANTOS_CONVIVIENTES_MAYORES_DE_19_AÑOS_TIENE_alta")

colnames(hoja_trabajo)

RECIBEN_TTO_PREVENTIVO_Y_CONTROLES_alta_mayores <- hoja_trabajo %>% filter( DNI != "") %>% count(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES...31)

sheet_write(RECIBEN_TTO_PREVENTIVO_Y_CONTROLES_alta_mayores,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "RECIBEN_TTO_PREVENTIVO_Y_CONTROLES_alta_mayores")

# TIENE_SINTOMAS_RESPIRATORIOS


TIENE_SINTOMAS_RESPIRATORIOS_alta <- hoja_trabajo %>% filter( DNI != "") %>% count(TIENE_SINTOMAS_RESPIRATORIOS)

sheet_write(TIENE_SINTOMAS_RESPIRATORIOS_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_SINTOMAS_RESPIRATORIOS_alta")

RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID_alta <- hoja_trabajo %>% filter( DNI != "") %>% count(RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID)

sheet_write(RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "RECIBIO_ESQUEMA_DE_VACUNA_CONTRA_COVID_alta")

TIENE_OOSS_O_PREPAGA_alta <- hoja_trabajo %>% filter( DNI != "") %>% count(TIENE_OOSS_O_PREPAGA)

sheet_write(TIENE_OOSS_O_PREPAGA_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "TIENE_OOSS_O_PREPAGA_alta")

SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA_alta <- hoja_trabajo %>% filter( DNI != "") %>% filter(TIENE_OOSS_O_PREPAGA == "No") %>% count(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA)  

sheet_write(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA_alta")

prueba_tto_subs <- hoja_trabajo %>% rename(subs_alta = 'SI_LA_ RESPUESTA_ES_SI_LE_GESTIONARON_EL_SUBSIDIO_DE_TBC')

#SI_LA_ RESPUESTA_ES_SI_LE_GESTIONARON_EL_SUBSIDIO_DE_TBC

subsidio_tbc_alta <- prueba_tto_subs %>% filter(TIENE_OOSS_O_PREPAGA == "No") %>% filter(SI_LA_RESPUESTA_ES_NO_TIENE_RESIDENCIA_DE_2_AÑOS_EN_PBA == "Si") %>% count(subs_alta)

sheet_write(subsidio_tbc_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "subsidio_tbc_alta")

# embarazo

hoja_prueba_emb_alta <- hoja_trabajo %>% rename(emb_alta = 'GESTANCION / PUERPERIO',emb_cursa_alta = '¿CURSA EMBARAZO?', complicacion_puerp_alta = '¿TUVO ALGUNA COMPLICACION DURANTE EL PUERPERIO?', amant_alta = '¿ESTAS_AMAMANTANDO_?')

emb_alta <- hoja_prueba_emb_alta %>% filter( DNI != "") %>% count(emb_alta)

sheet_write(emb_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "emb_alta")

emb_cursa_alta <- hoja_prueba_emb_alta %>% filter( DNI != "") %>% count(emb_cursa_alta)

sheet_write(emb_cursa_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "emb_cursa_alta")

complicacion_puerp_alta <- hoja_prueba_emb_alta %>% filter( DNI != "") %>% count(complicacion_puerp_alta)

sheet_write(complicacion_puerp_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "complicacion_puerp_alta")

amant_alta <- hoja_prueba_emb_alta %>% filter( DNI != "") %>% count(amant_alta)

sheet_write(amant_alta,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "amant_alta")


#-------------------------------------------------------- HOJAS CON CENTRAL REPARTIDO -------------------

# 


# Necesitamos hacer que los casos del central vayan a las RS correspondientes:

# Bajamos las hojas de las RS:


for (i in 1:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "1ER_LLAMADO",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

hoja_trabajo <- hoja_modelo

# bajamos hoja del central:

central <- read_sheet(ss = "1T4TFxmh3sXBLTvlJXpYehXcLx6s6baz-p01mshlxdbE",
                      sheet = "1ER_LLAMADO",
                      col_types = "c")

casos_RS_VI <- central %>% filter(Municipio_de_residencia == "Almirante Brown" | Municipio_de_residencia == "Avellaneda" | Municipio_de_residencia == "Berazategui" | Municipio_de_residencia == "Ezeiza" | Municipio_de_residencia == "Esteban EcheverrÃ­a" | Municipio_de_residencia == "Florencio Varela" | Municipio_de_residencia == "LanÃºs" | Municipio_de_residencia == "Quilmes" | Municipio_de_residencia == "Lomas de Zamora") %>% mutate(CETEC = "RS VI")

casos_RS_XI <- central %>% filter(Municipio_de_residencia == "La Plata" | Municipio_de_residencia == "Presidente PerÃ³n" | Municipio_de_residencia == "San Vicente" | Municipio_de_residencia == "Berisso" | Municipio_de_residencia == "CaÃ±uelas" | Municipio_de_residencia == "Ensenada") %>% mutate(CETEC = "RS XI")

casos_RS_XII <- central %>% filter(Municipio_de_residencia == "La Matanza") %>% mutate(CETEC = "RS XII")

casos_a_agregar <- bind_rows(casos_RS_VI,casos_RS_XI,casos_RS_XII)

hoja_trabajo <- bind_rows(casos_a_agregar,hoja_modelo)

hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_llamado = date(parse_date_time(hoja_trabajo$Fecha_llamado, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_proximo_llamado = date(parse_date_time(hoja_trabajo$Fecha_proximo_llamado, c("%d%m%Y", "%Y%m%d"))))

#Subimos los casos del central con  la RS correspondiente

equivalencias <- read_sheet(ss = "1T4TFxmh3sXBLTvlJXpYehXcLx6s6baz-p01mshlxdbE",
                            sheet = "equivalencias RS",
                            col_types = "c")

central_con_RS <- left_join(central,equivalencias,"Municipio_de_residencia")

sheet_write(central_con_RS,
            ss = "1T4TFxmh3sXBLTvlJXpYehXcLx6s6baz-p01mshlxdbE",
            sheet = "central_con_rs")

# Realizamos estadisticas de base de datos:

ESTUVO_O_ESTA_EN_TTO_PARA_TBC <- hoja_trabajo %>% filter(ESTUVO_O_ESTA_EN_TTO_PARA_TBC == "ESTA ACTUALMENTE EN TTO" | ESTUVO_O_ESTA_EN_TTO_PARA_TBC == "NUNCA ESTUVO EN TTO" | ESTUVO_O_ESTA_EN_TTO_PARA_TBC== "ESTUVO EN TTO Y DISCONTINUO" | ESTUVO_O_ESTA_EN_TTO_PARA_TBC == "ESTUVO EN TTO Y LE DIERON EL ALTA") %>% count(ESTUVO_O_ESTA_EN_TTO_PARA_TBC,CETEC)

sheet_write(ESTUVO_O_ESTA_EN_TTO_PARA_TBC,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "ESTUVO_O_ESTA_EN_TTO_PARA_TBC_2")

base_tbc <- hoja_trabajo %>% filter( DNI != "") %>% count(CETEC) %>% arrange(CETEC)

# para informe se necesita los casos del central repartidos en sus RS:


sheet_write(base_tbc,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "base_2")

# Personas sin llamar:

sin_llamar <- hoja_trabajo %>% filter(Fallecidx == "NO" | is.na(Fallecidx) | Fallecidx=="No") %>% filter(Estado_del_llamado == "Sin llamar")  %>% count(CETEC) 

sheet_write(sin_llamar,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Sin llamar_2")


# Estado del llamado:

contactados <- hoja_trabajo %>% filter( DNI != "") %>%  count(CETEC, Estado_del_llamado) %>%  arrange(CETEC)

sheet_write(contactados,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "contactados_2")


# Fallidos:

Fallidos_1er_contacto <- hoja_trabajo %>% count(CETEC, CANTIDAD_DE_FALLIDOS)

sheet_write(Fallidos_1er_contacto,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Fallidos_1er_contacto_2")


# Fallecidos

Fallecidos <- hoja_trabajo %>% filter (Fallecidx=="SI" ) %>%  count(CETEC)

sheet_write(Fallecidos,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "Fallecidxs_2")

tipo_tto <- hoja_trabajo %>% filter( DNI != "") %>% filter(Estado_del_llamado == "Contactadx(pasa a seguimiento)" | Estado_del_llamado == "Tiene el alta(caso finalizado)" ) %>% count(ESTUVO_O_ESTA_EN_TTO_PARA_TBC,CETEC)


sheet_write(tipo_tto,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "tipo_tto_2")


# ------------------- ACTUALMENTE EN TTO (SIN CENTRAL) ----------------------

hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "ACTUALMENTE_TTO",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas


for (i in 2:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "ACTUALMENTE_TTO",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}



central <- read_sheet(ss = "1T4TFxmh3sXBLTvlJXpYehXcLx6s6baz-p01mshlxdbE",
                      sheet = "ACTUALMENTE_TTO",
                      col_types = "c")

casos_RS_VI <- central %>% filter(Municipio_de_residencia == "Almirante Brown" | Municipio_de_residencia == "Avellaneda" | Municipio_de_residencia == "Berazategui" | Municipio_de_residencia == "Ezeiza" | Municipio_de_residencia == "Esteban EcheverrÃ­a" | Municipio_de_residencia == "Florencio Varela" | Municipio_de_residencia == "LanÃºs" | Municipio_de_residencia == "Quilmes" | Municipio_de_residencia == "Lomas de Zamora") %>% mutate(CETEC = "RS VI")

casos_RS_XI <- central %>% filter(Municipio_de_residencia == "La Plata" | Municipio_de_residencia == "Presidente PerÃ³n" | Municipio_de_residencia == "San Vicente" | Municipio_de_residencia == "Berisso" | Municipio_de_residencia == "CaÃ±uelas" | Municipio_de_residencia == "Ensenada") %>% mutate(CETEC = "RS XI")

casos_RS_XII <- central %>% filter(Municipio_de_residencia == "La Matanza") %>% mutate(CETEC = "RS XII")

casos_a_agregar <- bind_rows(casos_RS_VI,casos_RS_XI,casos_RS_XII)

hoja_trabajo <- bind_rows(casos_a_agregar,hoja_modelo)

hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_llamado = date(parse_date_time(hoja_trabajo$Fecha_llamado, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_proximo_llamado = date(parse_date_time(hoja_trabajo$Fecha_proximo_llamado, c("%d%m%Y", "%Y%m%d"))))

# ahora realizamos analisis de casos:

colnames(hoja_trabajo)


# cantidad de personas en este grupo:

cantidad_tto <- hoja_trabajo  %>% filter( DNI != "") %>% count(CETEC)

sheet_write(cantidad_tto,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "cantidad_tto_2")

# ------------------------ CASO NUMERO 2: NUNCA ESTUVO EN TTO SIN CENTRAL------------------------

hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "NUNCA_ESTUVO_EN_TTO",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas

links <- read_sheet(ss = "1quDT_34ZTy9wtZUHHV0rNbilFdbLRv64CHTko-Aoztk",
                    sheet = "links",
                    col_types = "c")

# ahora usamos el for (comando de iteraciÃ³n) para bajar todas las hojas:

for (i in 2:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "NUNCA_ESTUVO_EN_TTO",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

central <- read_sheet(ss = "1T4TFxmh3sXBLTvlJXpYehXcLx6s6baz-p01mshlxdbE",
                      sheet = "NUNCA_ESTUVO_EN_TTO",
                      col_types = "c")

casos_RS_VI <- central %>% filter(Municipio_de_residencia == "Almirante Brown" | Municipio_de_residencia == "Avellaneda" | Municipio_de_residencia == "Berazategui" | Municipio_de_residencia == "Ezeiza" | Municipio_de_residencia == "Esteban EcheverrÃ­a" | Municipio_de_residencia == "Florencio Varela" | Municipio_de_residencia == "LanÃºs" | Municipio_de_residencia == "Quilmes" | Municipio_de_residencia == "Lomas de Zamora") %>% mutate(CETEC = "RS VI")

casos_RS_XI <- central %>% filter(Municipio_de_residencia == "La Plata" | Municipio_de_residencia == "Presidente PerÃ³n" | Municipio_de_residencia == "San Vicente" | Municipio_de_residencia == "Berisso" | Municipio_de_residencia == "CaÃ±uelas" | Municipio_de_residencia == "Ensenada") %>% mutate(CETEC = "RS XI")

casos_RS_XII <- central %>% filter(Municipio_de_residencia == "La Matanza") %>% mutate(CETEC = "RS XII")

casos_a_agregar <- bind_rows(casos_RS_VI,casos_RS_XI,casos_RS_XII)

hoja_trabajo <- bind_rows(casos_a_agregar,hoja_modelo)

hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_llamado = date(parse_date_time(hoja_trabajo$Fecha_llamado, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_proximo_llamado = date(parse_date_time(hoja_trabajo$Fecha_proximo_llamado, c("%d%m%Y", "%Y%m%d"))))




# Preguntas de la tarea

cantidad_abandono <- hoja_trabajo  %>% filter( DNI != "") %>% count(CETEC)

sheet_write(cantidad_abandono,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "abandono_tto_2")

# -------------------------------------------- CASO 3 ESTUVO EN TTO Y DESCONTINUO (SIN CENTRAL) ----------------------------


hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "ESTUVO_EN TTO_Y_DISCONTINUO",
                          col_types = "c")


# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas

links <- read_sheet(ss = "1quDT_34ZTy9wtZUHHV0rNbilFdbLRv64CHTko-Aoztk",
                    sheet = "links",
                    col_types = "c")

# ahora usamos el for (comando de iteraciÃ³n) para bajar todas las hojas:

for (i in 2:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "ESTUVO_EN TTO_Y_DISCONTINUO",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

central <- read_sheet(ss = "1T4TFxmh3sXBLTvlJXpYehXcLx6s6baz-p01mshlxdbE",
                      sheet = "ESTUVO_EN TTO_Y_DISCONTINUO",
                      col_types = "c")

casos_RS_VI <- central %>% filter(Municipio_de_residencia == "Almirante Brown" | Municipio_de_residencia == "Avellaneda" | Municipio_de_residencia == "Berazategui" | Municipio_de_residencia == "Ezeiza" | Municipio_de_residencia == "Esteban EcheverrÃ­a" | Municipio_de_residencia == "Florencio Varela" | Municipio_de_residencia == "LanÃºs" | Municipio_de_residencia == "Quilmes" | Municipio_de_residencia == "Lomas de Zamora") %>% mutate(CETEC = "RS VI")

casos_RS_XI <- central %>% filter(Municipio_de_residencia == "La Plata" | Municipio_de_residencia == "Presidente PerÃ³n" | Municipio_de_residencia == "San Vicente" | Municipio_de_residencia == "Berisso" | Municipio_de_residencia == "CaÃ±uelas" | Municipio_de_residencia == "Ensenada") %>% mutate(CETEC = "RS XI")

casos_RS_XII <- central %>% filter(Municipio_de_residencia == "La Matanza") %>% mutate(CETEC = "RS XII")

casos_a_agregar <- bind_rows(casos_RS_VI,casos_RS_XI,casos_RS_XII)

hoja_trabajo <- bind_rows(casos_a_agregar,hoja_modelo)

hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_llamado = date(parse_date_time(hoja_trabajo$Fecha_llamado, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_proximo_llamado = date(parse_date_time(hoja_trabajo$Fecha_proximo_llamado, c("%d%m%Y", "%Y%m%d"))))




# Preguntas de la tarea

cantidad_abandono <- hoja_trabajo  %>% filter( DNI != "") %>% count(CETEC)

sheet_write(cantidad_abandono,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estuvo_tto_descontinuo_2")




#------------------------- CASO 4: ESTUVO EN TTO Y LE DIERON EL ALTA -----------------

hoja_modelo <- read_sheet(ss = "1U88_YCsFFPSxEFC1gGr6Aor2MqIororBDJDk1y_dW4s",
                          sheet = "ESTUVO_EN_TTO_Y_LE_DIERON_EL_ALTA",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas

links <- read_sheet(ss = "1quDT_34ZTy9wtZUHHV0rNbilFdbLRv64CHTko-Aoztk",
                    sheet = "links",
                    col_types = "c")

# ahora usamos el for (comando de iteraciÃ³n) para bajar todas las hojas:

for (i in 2:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "ESTUVO_EN_TTO_Y_LE_DIERON_EL_ALTA",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

central <- read_sheet(ss = "1T4TFxmh3sXBLTvlJXpYehXcLx6s6baz-p01mshlxdbE",
                      sheet = "ESTUVO_EN_TTO_Y_LE_DIERON_EL_ALTA",
                      col_types = "c")

casos_RS_VI <- central %>% filter(Municipio_de_residencia == "Almirante Brown" | Municipio_de_residencia == "Avellaneda" | Municipio_de_residencia == "Berazategui" | Municipio_de_residencia == "Ezeiza" | Municipio_de_residencia == "Esteban EcheverrÃ­a" | Municipio_de_residencia == "Florencio Varela" | Municipio_de_residencia == "LanÃºs" | Municipio_de_residencia == "Quilmes" | Municipio_de_residencia == "Lomas de Zamora") %>% mutate(CETEC = "RS VI")

casos_RS_XI <- central %>% filter(Municipio_de_residencia == "La Plata" | Municipio_de_residencia == "Presidente PerÃ³n" | Municipio_de_residencia == "San Vicente" | Municipio_de_residencia == "Berisso" | Municipio_de_residencia == "CaÃ±uelas" | Municipio_de_residencia == "Ensenada") %>% mutate(CETEC = "RS XI")

casos_RS_XII <- central %>% filter(Municipio_de_residencia == "La Matanza") %>% mutate(CETEC = "RS XII")

casos_a_agregar <- bind_rows(casos_RS_VI,casos_RS_XI,casos_RS_XII)

hoja_trabajo <- bind_rows(casos_a_agregar,hoja_modelo)

hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_llamado = date(parse_date_time(hoja_trabajo$Fecha_llamado, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(Fecha_proximo_llamado = date(parse_date_time(hoja_trabajo$Fecha_proximo_llamado, c("%d%m%Y", "%Y%m%d"))))




# Preguntas de la tarea

cantidad_abandono <- hoja_trabajo  %>% filter( DNI != "") %>% count(CETEC)

sheet_write(cantidad_abandono,
            ss = "1HR2RlBAjW0TBl4u1XuoOTRToGjDvY9X6Yy8x5b9-pQw",
            sheet = "estuvo_tto_alta_2")

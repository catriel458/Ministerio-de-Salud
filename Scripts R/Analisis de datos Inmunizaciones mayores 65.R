# Script Inmunizaciones mayores 65: Centralización y analisis de los datos.

# Se propone la bajada y analisis de los datos de la tarea de inmunizaciones

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

hoja_modelo <- read_sheet(ss = "1DHfm2IxUGOeOH--UyaJgtZSRq5TBYLPf7KAvmGSKN3Q",
                          sheet = "Base de datos",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas de AMBA:

links_AMBA <- read_sheet(ss = "1tWqQlmzIrtaOcgQ8KuWBJfqL9uAs1hqaIU-fZq7rID4",
                         sheet = "links",
                         col_types = "c")

# descargamos planillas de interior

links_interior <- read_sheet(ss = "1gvKG_WRVBOEmJBy6PT85P8XfZwbBqisCRw31qFRfxbk",
                             sheet = "links",
                             col_types = "c")

# las unimos en solo dataset con rbind:


links_total <- rbind(links_AMBA,links_interior)

# ahora usamos el for (comando de iteraci?n) para bajar todas las hojas:

for (i in 1:nrow(links_total)) {
  datos_1 <- range_read(ss = links_total$LINK[i],
                        sheet = "Base de datos",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links_total$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}




central_segunda_base <- read_sheet(ss = "1vLw1tU0zStCk4J7At_mH0t2F12Bbc9pOGfaStfU_P8Q",
                                   sheet = "CENTRAL",
                                   col_types = "c")

central_tercera_base <- read_sheet(ss = "1ak33blkjyEkDDcfMaGFA_fAuvPZzNOKppTwfjxPAYMc",
                                   sheet = "Base de datos Agosto",
                                   col_types = "c")

hoja_trabajo <- hoja_modelo 

# La hoja del central tiene 2 columnas extras. Se agregaran al drive de la suma de todas las hojas para el bind.

DIA_Y_HORARIO_DISPONIBLE_PARA_EL_LLAMADO  <- NA
INFORMACION_OBTENIDA_DE_SISA <- NA

colnames(central_tercera_base)

hoja_trabajo <- hoja_trabajo %>% mutate(DIA_Y_HORARIO_DISPONIBLE_PARA_EL_LLAMADO,INFORMACION_OBTENIDA_DE_SISA)

hoja_trabajo <- hoja_trabajo %>% select(
  OPERADXR,
  DIA_Y_HORARIO_DISPONIBLE_PARA_EL_LLAMADO,
  FALLECIDX,
  ESTADO_DE_CASO,
  CONTACTADX,
  FECHA_DE_PRIMER_CONTACTO,
  CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,
  FECHA_DE_SEGUNDO_CONTACTO,
  CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,
  NOMBRE_Y_APELLIDO,
  DNI,
  FECHA_DE_NACIMIENTO,
  EDAD,
  TELEFONO_1,
  TELEFONO_2,
  MUNICIPIO,
  OBRA_SOCIAL,
  INFORMACION_OBTENIDA_DE_SISA,
  SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO,
  SE_APLICO_LA_DOBLE_BACTERIANA_DIFTERIA_TETANOS_CADA_10_AÑOS,
  SE_APLICO_VACUNA_COVID,
  ASISTIRA_A_VACUNARSE_CON_LA_VACUNA_FALTANTE,
  USTED_FUMA,
  LE_GUSTARÍA_DEJAR_DE_FUMAR,
  OBSERVACIONES,
  INICIO_DE_SEGUNDO_CONTACTO,
  ASISTIO_A_VACUNARSE,
  FUE_VACUNADX_EFECTIVAMENTE,
  MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LA_VACUNARON,
  A_QUE_VACUNATORIO_CONCURRIO,
  COMO_CONSIDERA_QUE_LX_ATENDIERON,
  MOTIVO_DETALLADO_MALA_ATENCION,
  SI_NO_LE_ADMINISTRARON_TODAS_LAS_VACUNAS_QUE_INDICACION_LE_DIERON,
  LO_VACUNARON_CON_TURNO_FUERA_DEL_DIA_O_ERA_POR_DEMANDA,
  FUE_ASESORADX_ACERCA_DEL_CALENDARIO_DE_VACUNACION,
  LE_PIDIERON_LIBRETA_SANITARIA_O_CARNET_DE_VACUNACION,
  LE_PIDIERON_EL_DNI_EN_EL_VACUNATORIO,
  LE_OTORGARON_UN_COMPROBANTE_DE_VACUNACION_O_PEGATINA_DE_VACUNA_O_OTRO,
  OBSERVACIONES_GENERALES,
  TIENE_GATO_PERRO_EN_SU_CASA,
  ESTA_INTERESADX_EN_RECIBIR_INFORMACION_SOBRE_CUIDADO_DE_MASCOTAS,
  CETEC)

# ahora cambiamos la de segunda base:

central_segunda_base <- central_segunda_base %>% mutate(DIA_Y_HORARIO_DISPONIBLE_PARA_EL_LLAMADO,INFORMACION_OBTENIDA_DE_SISA) %>% 
  
select(OPERADXR,
       DIA_Y_HORARIO_DISPONIBLE_PARA_EL_LLAMADO,
       FALLECIDX,
       ESTADO_DE_CASO,
       CONTACTADX,
       FECHA_DE_PRIMER_CONTACTO,
       CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,
       FECHA_DE_SEGUNDO_CONTACTO,
       CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,
       NOMBRE_Y_APELLIDO,
       DNI,
       FECHA_DE_NACIMIENTO,
       EDAD,
       TELEFONO_1,
       TELEFONO_2,
       MUNICIPIO,
       OBRA_SOCIAL,
       INFORMACION_OBTENIDA_DE_SISA,
       SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO,
       SE_APLICO_LA_DOBLE_BACTERIANA_DIFTERIA_TETANOS_CADA_10_AÑOS,
       SE_APLICO_VACUNA_COVID,
       ASISTIRA_A_VACUNARSE_CON_LA_VACUNA_FALTANTE,
       USTED_FUMA,
       LE_GUSTARÍA_DEJAR_DE_FUMAR,
       OBSERVACIONES,
       INICIO_DE_SEGUNDO_CONTACTO,
       ASISTIO_A_VACUNARSE,
       FUE_VACUNADX_EFECTIVAMENTE,
       MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LA_VACUNARON,
       A_QUE_VACUNATORIO_CONCURRIO,
       COMO_CONSIDERA_QUE_LX_ATENDIERON,
       MOTIVO_DETALLADO_MALA_ATENCION,
       SI_NO_LE_ADMINISTRARON_TODAS_LAS_VACUNAS_QUE_INDICACION_LE_DIERON,
       LO_VACUNARON_CON_TURNO_FUERA_DEL_DIA_O_ERA_POR_DEMANDA,
       FUE_ASESORADX_ACERCA_DEL_CALENDARIO_DE_VACUNACION,
       LE_PIDIERON_LIBRETA_SANITARIA_O_CARNET_DE_VACUNACION,
       LE_PIDIERON_EL_DNI_EN_EL_VACUNATORIO,
       LE_OTORGARON_UN_COMPROBANTE_DE_VACUNACION_O_PEGATINA_DE_VACUNA_O_OTRO,
       OBSERVACIONES_GENERALES,
       TIENE_GATO_PERRO_EN_SU_CASA,
       ESTA_INTERESADX_EN_RECIBIR_INFORMACION_SOBRE_CUIDADO_DE_MASCOTAS,
       CETEC)

#  transformamos base para trabajar:


hoja_trabajo <- rbind(hoja_trabajo,central_segunda_base,central_tercera_base)

colnames(hoja_trabajo)
colnames(central_segunda_base)
colnames(central_tercera_base)




hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_DE_PRIMER_CONTACTO = date(parse_date_time(hoja_trabajo$FECHA_DE_PRIMER_CONTACTO, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_DE_SEGUNDO_CONTACTO = date(parse_date_time(hoja_trabajo$FECHA_DE_SEGUNDO_CONTACTO, c("%d%m%Y", "%Y%m%d"))))

# Realizamos estadisticas de base de datos:

base_inmunizaciones <- hoja_trabajo %>% count(CETEC) %>% arrange(CETEC)

sheet_write(base_inmunizaciones,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "base")

# Personas sin llamar (no nos contactamos nunca, sin llamar, no se finaliza el caso, diferente a FIN(OTROS)) son personas que nunca intentamos llamar, son casos pendientes , no estan finalizados.

sin_llamar <- hoja_trabajo %>% filter(FALLECIDX == "No" | is.na(FALLECIDX)) %>% filter(CONTACTADX == "Sin llamar") %>% filter(ESTADO_DE_CASO != 'Fin (OTROS)') %>% count(CETEC)

sheet_write(sin_llamar,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Sin llamar")

# Estado del caso cetec:

estado_del_caso <- hoja_trabajo %>% count(ESTADO_DE_CASO,CETEC)

sheet_write(estado_del_caso,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "estado del caso cetec")

# Contactados:

contactados <- hoja_trabajo %>% count(CETEC,CONTACTADX) 

sheet_write(contactados,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "contactados")

# Fallidos:

fallidos_1er_contacto <- hoja_trabajo %>% count(CETEC,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO)

sheet_write(fallidos_1er_contacto,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "fallidos_1er_contacto")

fallidos_2do_contacto <- hoja_trabajo %>% count(CETEC,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

sheet_write(fallidos_2do_contacto,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "fallidos_2do_contacto")

colnames(hoja_trabajo)

# vacunas:

antigripal <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>%  count(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO)

doble_b <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_APLICO_LA_DOBLE_BACTERIANA_DIFTERIA_TETANOS_CADA_10_AÑOS)

vacuna_covid <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_APLICO_VACUNA_COVID) # (filtro con contactados para sacar casos de mas)?)

antigripal_y_doble_b <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% filter(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO == "SI" & SE_APLICO_LA_DOBLE_BACTERIANA_DIFTERIA_TETANOS_CADA_10_AÑOS == "SI") %>% count()

antigripal_doble_b_y_covid <- hoja_trabajo %>% filter(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO == "SI" & SE_APLICO_LA_DOBLE_BACTERIANA_DIFTERIA_TETANOS_CADA_10_AÑOS == "SI" &  SE_APLICO_VACUNA_COVID != "Primera" &  SE_APLICO_VACUNA_COVID != "Ninguna") %>% count()

asistencia <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(ASISTIRA_A_VACUNARSE_CON_LA_VACUNA_FALTANTE)


# ----------------------- CRUZAS COMPLEMENTARIAS -------------------------

# personas que no estaban vacunadas con ninguna vacuna

no_se_vacuno_ninguna_asistencia <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO == "NO" & SE_APLICO_LA_DOBLE_BACTERIANA_DIFTERIA_TETANOS_CADA_10_AÑOS == "NO") %>% count(ASISTIO_A_VACUNARSE,FUE_VACUNADX_EFECTIVAMENTE)

# personas vacunadas con antigripal unicamente asistencia  a vacunación:

vacunada_con_antigripal_asistencia <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO == "SI" & SE_APLICO_LA_DOBLE_BACTERIANA_DIFTERIA_TETANOS_CADA_10_AÑOS == "NO") %>% count(ASISTIO_A_VACUNARSE,FUE_VACUNADX_EFECTIVAMENTE)

# subimos:

sheet_write(no_se_vacuno_ninguna_asistencia,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "no_se_vacuno_ninguna_asistencia")

sheet_write(vacunada_con_antigripal_asistencia,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "vacunada_con_antigripal_asistencia")

# subimos todos:

sheet_write(antigripal,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "antigripal")


sheet_write(doble_b,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "doble_b")

sheet_write(vacuna_covid,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "vacuna_covid")

sheet_write(antigripal_y_doble_b,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "antigripal_y_doble_b")

sheet_write(antigripal_doble_b_y_covid,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "antigripal_doble_b_y_covid")

sheet_write(asistencia,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "asistencia")


# hacemos estadisticas segundo parte de la tarea:

# asistencia a vacunarse:

ASISTIO_A_VACUNARSE <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>%  count(ASISTIO_A_VACUNARSE,CETEC)

sheet_write(ASISTIO_A_VACUNARSE,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "ASISTIO_A_VACUNARSE")

# vacunación efectiva:

VACUNACION_EFECTIVA <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(FUE_VACUNADX_EFECTIVAMENTE,CETEC)

sheet_write(VACUNACION_EFECTIVA,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "VACUNACION_EFECTIVA")


# Motivos de ausentismo:

motivo_ausentismo__de_los_que_no_fueron <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "No asistió") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LA_VACUNARON)

sheet_write(motivo_ausentismo__de_los_que_no_fueron,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "motivo_ausentismo__de_los_que_no_fueron")

# motivos de los que fueron a vacunarse y no los vacunaron

motivo_ausentismo_fue_y_no_lo_vacunaron <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE == "NO") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LA_VACUNARON)

sheet_write(motivo_ausentismo_fue_y_no_lo_vacunaron,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "motivo_ausentismo_fue_y_no_lo_vacunaron")

#  vacunatorio 

vacunatorio <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió") %>% count(A_QUE_VACUNATORIO_CONCURRIO) %>% arrange(desc(n))

sheet_write(vacunatorio,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "vacunatorio")

# tipo de atencion

atencion <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(COMO_CONSIDERA_QUE_LX_ATENDIERON,CETEC)

sheet_write(atencion,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "atencion")

# mala atencion 

mala_atencion <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & COMO_CONSIDERA_QUE_LX_ATENDIERON == "Mal" | COMO_CONSIDERA_QUE_LX_ATENDIERON == "Muy mal") %>% count(MOTIVO_DETALLADO_MALA_ATENCION)

sheet_write(mala_atencion,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "mala_atencion")

# si no lo vacunaron que le dijeron:

indicaciones <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & (FUE_VACUNADX_EFECTIVAMENTE ==  "Si con algunas dosis" | FUE_VACUNADX_EFECTIVAMENTE == "NO")) %>% count(SI_NO_LE_ADMINISTRARON_TODAS_LAS_VACUNAS_QUE_INDICACION_LE_DIERON)

sheet_write(indicaciones,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "indicaciones")


# Turno o demanda:

turno_demanda <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(LO_VACUNARON_CON_TURNO_FUERA_DEL_DIA_O_ERA_POR_DEMANDA)

sheet_write(turno_demanda,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "turno_demanda")


# asesoria 

asesoria <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(FUE_ASESORADX_ACERCA_DEL_CALENDARIO_DE_VACUNACION)

sheet_write(asesoria,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "asesoria")

# carnet

carnet <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(LE_PIDIERON_LIBRETA_SANITARIA_O_CARNET_DE_VACUNACION) 

sheet_write(carnet,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "carnet")

# dni

dni <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(LE_PIDIERON_EL_DNI_EN_EL_VACUNATORIO) 

sheet_write(dni,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "dni")

# comprobante

comprobante <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(LE_OTORGARON_UN_COMPROBANTE_DE_VACUNACION_O_PEGATINA_DE_VACUNA_O_OTRO)

sheet_write(comprobante,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "comprobante")

#  hacemos estadisticas de fecha de primer llamado:

Mayo <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-05-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-05-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

junio <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-06-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-06-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)
                                 
julio <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-07-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-07-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

agosto <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-08-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-08-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

Septiembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-09-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-09-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

Octubre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-10-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-10-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

Noviembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-11-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-11-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

Dicembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-12-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-12-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)


sheet_write(Mayo,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Mayo")

sheet_write(junio,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Junio")

sheet_write(julio,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Julio")

sheet_write(Septiembre,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Septiembre")

sheet_write(Octubre,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Octubre")

sheet_write(Noviembre,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Noviembre")

sheet_write(Dicembre,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Dicembre")



Mayo_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-05-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-05-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Junio_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-06-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-06-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Julio_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-07-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-07-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Agosto_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-08-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-08-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Septiembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-09-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-09-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Octubre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-10-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-10-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Noviembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-11-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-11-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Diciembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-12-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-12-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)


sheet_write(Mayo_cetec,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Mayo_cetec")

sheet_write(Junio_cetec,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Junio_cetec")

sheet_write(Julio_cetec,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Julio_cetec")

sheet_write(Agosto_cetec,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Agosto_cetec")

sheet_write(Septiembre_cetec,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Septiembre_cetec")

sheet_write(Octubre_cetec,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Octubre_cetec")

sheet_write(Noviembre_cetec,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Noviembre_cetec")

sheet_write(Diciembre_cetec,
            ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
            sheet = "Diciembre_cetec")


# subimos actualizacion

actualizacion <- as_tibble(as.character(Sys.time()))

actualizacion %>% 
  sheet_write(
    ss = "1tuvCmMKbYs1Z2sjJWHjySlgjBctFdhPR2o0rPn7rIdM",
    sheet = "actualizacion")

# Derivaciones

# --------------------------- Derivacion a TBQ --------------------------------

hoja_trabajo <- hoja_modelo

base_TBQ_1 <- hoja_trabajo %>% filter(USTED_FUMA == "SI" & LE_GUSTARÍA_DEJAR_DE_FUMAR == "SI" )

base_TBQ_2 <- hoja_trabajo %>% filter(USTED_FUMA == "Si" & LE_GUSTARÍA_DEJAR_DE_FUMAR == "Si" )

base_TBQ <- rbind(base_TBQ_1,base_TBQ_2)

base_tbq_filtrada <- base_TBQ %>% mutate(ASIGNADX_A_CETEC = NA_character_) %>% select(ASIGNADX_A_CETEC,NOMBRE_Y_APELLIDO,DNI,FECHA_DE_NACIMIENTO,EDAD,TELEFONO_1,TELEFONO_2,MUNICIPIO,OBRA_SOCIAL,CETEC) %>% arrange(CETEC)

# bajamos historicos 

tbq_historico <- read_sheet(ss = "1DNWmClfUXxVmqmcfi-Rq-9FixWf50BNSv93j5LvjD7k",
                            sheet = "base",
                            col_types = "c")

# hacemos cruza con historico para no subir casos ya subidos (no repetir personas)

lista_tbq <- anti_join(base_tbq_filtrada,tbq_historico,"DNI")

# ?Como se reparten los casos?

casos_a_subir <- lista_tbq %>% count(CETEC)

# ?hay casos nuevos para subir?

if (nrow(lista_tbq) == 0) {
  print("No hay pacientes para l?nea TBQ nuevos")
} else {
  lista_tbq %>% 
    sheet_append(
      ss = "1DNWmClfUXxVmqmcfi-Rq-9FixWf50BNSv93j5LvjD7k",
      sheet = "base"
    )
}


# derivación a lineas mascotas:

hoja_trabajo <- hoja_modelo

base_mascotas <- hoja_trabajo %>% filter(ESTA_INTERESADX_EN_RECIBIR_INFORMACION_SOBRE_CUIDADO_DE_MASCOTAS == "Si")

base_mascotas_filtrada <- base_mascotas %>% mutate(ASIGNADX_A_CETEC = NA_character_) %>% select(ASIGNADX_A_CETEC,TIENE_GATO_PERRO_EN_SU_CASA,NOMBRE_Y_APELLIDO,DNI,FECHA_DE_NACIMIENTO,EDAD,TELEFONO_1,TELEFONO_2,MUNICIPIO,OBRA_SOCIAL,CETEC) %>% arrange(CETEC)

# bajamos historicos 

mascotas_historico <- read_sheet(ss = "1Zoxr5YcIq9Gyj_8iIttGtJV_i8_ambZhPkU_t272T4U",
                                 sheet = "base",
                                 col_types = "c")

# hacemos cruza con historico para no subir casos ya subidos (no repetir personas)

lista_mascotas <- anti_join(base_mascotas_filtrada,mascotas_historico,"DNI")

# ¿Cuantos casos hay para subir?

casos_a_subir <- lista_mascotas %>% count(CETEC)

# ?hay casos nuevos para subir?

if (nrow(lista_mascotas) == 0) {
  print("No hay pacientes para l?nea mascotas nuevos")
} else {
  lista_mascotas %>% 
    sheet_append(
      ss = "1Zoxr5YcIq9Gyj_8iIttGtJV_i8_ambZhPkU_t272T4U",
      sheet = "base"
    )
}



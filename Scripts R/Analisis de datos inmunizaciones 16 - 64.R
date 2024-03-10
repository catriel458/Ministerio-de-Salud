# Estadisticas inmunizaciones 16 - 64 años (Personas con factores de riesgo)

# Script Inmunizaciones: Centralización y analisis de los datos.

# Se propone la bajada y analisis de los datos de la tarea de inmunizaciones 16 - 64 años

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

hoja_modelo <- read_sheet(ss = "1Dk6WIfXz-r_fP28ntCPs4-W-_inEeCwu4-hYm8ney9I",
                          sheet = "Base de datos",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas de AMBA:

links_AMBA <- read_sheet(ss = "1zerN_5gasGK5SDPxZJeYANpUYwEpDL-YPQiD68swvMw",
                         sheet = "links",
                         col_types = "c")

# descargamos planillas de interior

links_interior <- read_sheet(ss = "1wHfEt-LUIr2HfVFQv0ck6hxUdxcDSgQaQzETXHYTfpU",
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

hoja_trabajo <- hoja_modelo

#  transformamos base para trabajar:

hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_DE_PRIMER_CONTACTO = date(parse_date_time(hoja_trabajo$FECHA_DE_PRIMER_CONTACTO, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_DE_SEGUNDO_CONTACTO = date(parse_date_time(hoja_trabajo$FECHA_DE_SEGUNDO_CONTACTO, c("%d%m%Y", "%Y%m%d"))))

# Realizamos estadisticas de base de datos:


estado_de_caso_cetec <- hoja_trabajo %>% filter(ESTADO_DE_CASO == "Para primer contacto" | ESTADO_DE_CASO == "Para segundo contacto") %>% count(ESTADO_DE_CASO,CETEC)

sheet_write(estado_de_caso_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "estado_de_caso_cetec")

base_inmunizaciones <- hoja_trabajo %>% count(CETEC) %>% arrange(CETEC)

sheet_write(base_inmunizaciones,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "base")

#%>% filter(FALLECIDX == "No" | is.na(FALLECIDX)) %>%

# Personas sin llamar:

sin_llamar <- hoja_trabajo %>% filter(FALLECIDX == "No" | is.na(FALLECIDX)) %>% filter(CONTACTADX == "Sin llamar") %>% filter(ESTADO_DE_CASO != 'Fin (OTROS)') %>% count(CETEC)


sheet_write(sin_llamar,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Sin llamar")

# Estado del caso:

estado_del_caso <- hoja_trabajo %>% count(ESTADO_DE_CASO)

sheet_write(estado_del_caso,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "estado del caso")

# Contactados:

contactados <- hoja_trabajo %>% count(CETEC,CONTACTADX) 

sheet_write(contactados,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "contactados")

# Fallidos:

fallidos_1er_contacto <- hoja_trabajo %>% count(CETEC,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO)

sheet_write(fallidos_1er_contacto,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "fallidos_1er_contacto")

fallidos_2do_contacto <- hoja_trabajo %>% count(CETEC,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

sheet_write(fallidos_2do_contacto,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "fallidos_2do_contacto")

colnames(hoja_trabajo)

# vacunas:

antigripal <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO)

doble_b <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_VACUNO_CON_DOBLE_BACTERIANA_DIFTERIA_TETANOS)

vacuna_covid <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_APLICO_VACUNA_COVID) # (filtro con contactados para sacar casos de mas)?)

antigripal_y_doble_b <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% filter(SE_VACUNO_CON_DOBLE_BACTERIANA_DIFTERIA_TETANOS == "SI") %>% count()

antigripal_doble_b_y_covid <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% filter(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO == "SI" & SE_VACUNO_CON_DOBLE_BACTERIANA_DIFTERIA_TETANOS == "SI" &  SE_APLICO_VACUNA_COVID != "Primera" &  SE_APLICO_VACUNA_COVID != "Ninguna") %>% count()

# pendiente (hacer los filtros cuando se pueda)
 
asistencia <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(ASISTIRA_A_VACUNARSE_CON_LA_VACUNA_FALTANTE)


# subimos todos:

sheet_write(antigripal,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "antigripal")


sheet_write(doble_b,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "doble_b")

sheet_write(vacuna_covid,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "vacuna_covid")

sheet_write(antigripal_y_doble_b,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "antigripal_y_doble_b")

sheet_write(antigripal_doble_b_y_covid,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "antigripal_doble_b_y_covid")

sheet_write(asistencia,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "asistencia")


# hacemos estadisticas segundo parte de la tarea:

# asistencia a vacunarse:

ASISTIO_A_VACUNARSE <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>%  count(ASISTIO_A_VACUNARSE,CETEC)

sheet_write(ASISTIO_A_VACUNARSE,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "ASISTIO_A_VACUNARSE")

# vacunaci?n efectiva:

VACUNACION_EFECTIVA <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(FUE_VACUNADX_EFECTIVAMENTE,CETEC)

sheet_write(VACUNACION_EFECTIVA,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "VACUNACION_EFECTIVA")


# Motivos de ausentismo:

motivo_ausentismo__de_los_que_no_fueron <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "No asistió") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LX_VACUNARON)

sheet_write(motivo_ausentismo__de_los_que_no_fueron,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "motivo_ausentismo__de_los_que_no_fueron")

# motivos de los que fueron a vacunarse y no los vacunaron

motivo_ausentismo_fue_y_no_lo_vacunaron <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE == "NO") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LX_VACUNARON)

sheet_write(motivo_ausentismo_fue_y_no_lo_vacunaron,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "motivo_ausentismo_fue_y_no_lo_vacunaron")

# tipo de atencion

atencion <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(COMO_CONSIDERA_QUE_LX_ATENDIERON,CETEC)

sheet_write(atencion,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "atencion")

# mala atencion 

mala_atencion <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & COMO_CONSIDERA_QUE_LX_ATENDIERON == "Mal" | COMO_CONSIDERA_QUE_LX_ATENDIERON == "Muy mal") %>% count(MOTIVO_DETALLADO_MALA_ATENCION)

sheet_write(mala_atencion,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "mala_atencion")

# si no lo vacunaron que le dijeron:

indicaciones <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & (FUE_VACUNADX_EFECTIVAMENTE ==  "Si con algunas dosis" | FUE_VACUNADX_EFECTIVAMENTE == "NO")) %>% count(SI_NO_LE_ADMINISTRARON_TODAS_LAS_VACUNAS_QUE_INDICACION_LE_DIERON)

sheet_write(indicaciones,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "indicaciones")


# Turno o demanda:

turno_demanda <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(LO_VACUNARON_CON_TURNO_FUERA_DEL_DIA_O_ERA_POR_DEMANDA)

sheet_write(turno_demanda,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "turno_demanda")


# asesoria 

asesoria <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(FUE_ASESORADX_ACERCA_DEL_CALENDARIO_DE_VACUNACION)

sheet_write(asesoria,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "asesoria")

# carnet

carnet <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(LE_PIDIERON_LIBRETA_SANITARIA_O_CARNET_DE_VACUNACION) 

sheet_write(carnet,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "carnet")

# dni

dni <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(LE_PIDIERON_EL_DNI_EN_EL_VACUNATORIO) 

sheet_write(dni,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "dni")

# comprobante

comprobante <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(LE_OTORGARON_UN_COMPROBANTE_DE_VACUNACION_O_PEGATINA_DE_VACUNA_O_OTRO)

sheet_write(comprobante,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "comprobante")

#  hacemos estadisticas de fecha de primer llamado:

Mayo <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-05-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-05-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

junio <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-06-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-06-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

julio <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-07-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-07-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

agosto <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-08-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-08-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

septiembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-09-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-09-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

octubre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-10-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-10-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

noviembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-11-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-11-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

diciembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-12-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-12-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)


sheet_write(Mayo,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Mayo")

sheet_write(junio,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Junio")

sheet_write(julio,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Julio")

sheet_write(agosto,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Agosto")

sheet_write(septiembre,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "septiembre")

sheet_write(octubre,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "octubre")

sheet_write(noviembre,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "noviembre")

sheet_write(diciembre,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "diciembre")


Mayo_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-05-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-05-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Junio_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-06-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-06-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

Julio_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-07-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-07-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

agosto_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-08-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-08-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

septiembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-09-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-09-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

octubre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-10-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-10-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

noviembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-11-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-11-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

diciembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO  >= "2022-12-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-12-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)


sheet_write(Mayo_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Mayo_cetec")

sheet_write(Junio_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Junio_cetec")

sheet_write(Julio_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Julio_cetec")

sheet_write(agosto_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "Agosto_cetec")

sheet_write(septiembre_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "septiembre_cetec")


sheet_write(octubre_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "octubre_cetec")


sheet_write(noviembre_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "noviembre_cetec")

sheet_write(diciembre_cetec,
            ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
            sheet = "diciembre_cetec")

# subimos actualizacion:

actualizacion <- as_tibble(as.character(Sys.time()))

actualizacion %>% 
  sheet_write(
    ss = "1U1nchAWnh-itXPnpTTkuuaeRXhldG8yH-MAfP8Dwr6U",
    sheet = "actualizacion")

# Derivaciones

# --------------------------- Derivacion a TBQ --------------------------------

hoja_trabajo <- hoja_modelo

base_TBQ_1 <- hoja_trabajo %>% filter(USTED_FUMA == "SI" & LE_GUSTARIA_DEJAR_DE_FUMAR == "SI" )

base_TBQ_2 <- hoja_trabajo %>% filter(USTED_FUMA == "SI" & LE_GUSTARIA_DEJAR_DE_FUMAR == "Si" )

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


# Analisis de datos vacunatorios:

hoja_trabajo <- hoja_modelo

vacunatorios_muni <- hoja_trabajo %>% filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO)) %>% count(A_QUE_VACUNATORIO_CONCURRIO,MUNICIPIO) %>% arrange(desc(n))

vacunatorio_NO_dosis <- hoja_trabajo %>% filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO)) %>% filter(!is.na(FUE_VACUNADX_EFECTIVAMENTE)) %>% filter(FUE_VACUNADX_EFECTIVAMENTE =="NO" ) %>% count(A_QUE_VACUNATORIO_CONCURRIO,MUNICIPIO,FUE_VACUNADX_EFECTIVAMENTE) %>% arrange(MUNICIPIO) %>% arrange(desc(n))

vacunatorio_NO_todas_dosis <- hoja_trabajo %>% filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO)) %>% filter(!is.na(FUE_VACUNADX_EFECTIVAMENTE)) %>% filter(FUE_VACUNADX_EFECTIVAMENTE =="Si con algunas dosis" ) %>% count(A_QUE_VACUNATORIO_CONCURRIO,MUNICIPIO,FUE_VACUNADX_EFECTIVAMENTE) %>% arrange(MUNICIPIO) %>% arrange(desc(n)) 

vacunatorio_calidad_atencion <- hoja_trabajo %>% filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO) & !is.na(COMO_CONSIDERA_QUE_LX_ATENDIERON)) %>% count(A_QUE_VACUNATORIO_CONCURRIO,MUNICIPIO,COMO_CONSIDERA_QUE_LX_ATENDIERON) %>% arrange(desc(n))

motivos_mala_atencion_por_vacunatorio <- hoja_trabajo %>% filter(MOTIVO_DETALLADO_MALA_ATENCION != "No corresponde") %>%  filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO) & !is.na(MOTIVO_DETALLADO_MALA_ATENCION)) %>% count(A_QUE_VACUNATORIO_CONCURRIO,MUNICIPIO,MOTIVO_DETALLADO_MALA_ATENCION) %>% arrange(desc(n))

# vacunatorios en los que no dieron ninguna indicación respecto a vacunas faltantes (% sobre total y recuento de casos)

vacunatorio_indicacion <- hoja_trabajo %>% filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO)) %>% filter(SI_NO_LE_ADMINISTRARON_TODAS_LAS_VACUNAS_QUE_INDICACION_LE_DIERON != "No corresponde") %>% filter(!is.na(SI_NO_LE_ADMINISTRARON_TODAS_LAS_VACUNAS_QUE_INDICACION_LE_DIERON))  %>% count(A_QUE_VACUNATORIO_CONCURRIO,MUNICIPIO,SI_NO_LE_ADMINISTRARON_TODAS_LAS_VACUNAS_QUE_INDICACION_LE_DIERON) %>% arrange(desc(n))

# vacunatorios que piden libreta sanitaria y no vacunaron con ninguna dosis:

pide_libreta_y_no_vacuno <- hoja_trabajo %>% filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO)) %>% filter(FUE_VACUNADX_EFECTIVAMENTE =="NO" ) %>% filter(LE_PIDIERON_LIBRETA_SANITARIA_O_CARNET_DE_VACUNACION == "SI") 

# vacunatorios que no vacunaron con ninguna dosis y solicitan DNI (ordenado por region)


pide_DNI_y_no_vacuno <- hoja_trabajo %>% filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO)) %>% filter(FUE_VACUNADX_EFECTIVAMENTE =="NO" ) %>% filter(LE_PIDIERON_EL_DNI_EN_EL_VACUNATORIO == "SI") 

#vacunatorios que no entregaron comprobante de vacunación (ordenado por region)

no_entregaron_comprobante <- hoja_trabajo %>% filter(!is.na(A_QUE_VACUNATORIO_CONCURRIO)) %>% filter(LE_OTORGARON_UN_COMPROBANTE_DE_VACUNACION_O_PEGATINA_DE_VACUNA_O_OTRO =="No" ) %>% count(A_QUE_VACUNATORIO_CONCURRIO,MUNICIPIO,LE_OTORGARON_UN_COMPROBANTE_DE_VACUNACION_O_PEGATINA_DE_VACUNA_O_OTRO) %>% arrange(MUNICIPIO)


sheet_write(vacunatorios_muni,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "vacunatorios_muni_16_64")
  
sheet_write(vacunatorio_NO_dosis,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "vacunatorio_NO_dosis_16_64")

sheet_write(vacunatorio_NO_todas_dosis,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "vacunatorio_NO_todas_dosis_16_64")

sheet_write(vacunatorio_calidad_atencion,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "vacunatorio_calidad_atencion_16_64")

sheet_write(vacunatorio_indicacion,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "vacunatorio_indicacion")


sheet_write(pide_libreta_y_no_vacuno,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "pide_libreta_y_no_vacuno")

sheet_write(pide_DNI_y_no_vacuno,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "pide_DNI_y_no_vacuno")

sheet_write(pide_DNI_y_no_vacuno,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "pide_DNI_y_no_vacuno")


sheet_write(no_entregaron_comprobante,
            ss = "1ZgdVsR2Hmk-EEc8LyREFJMxT4NlwWGaHUQHkXY5E-5Y",
            sheet = "no_entregaron_comprobante")
  

  
  
  

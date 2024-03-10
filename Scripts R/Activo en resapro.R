# Script de DBT - ACTIVXS EN RESAPRO:

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


#-------------------------- Base seguimiento pacientes que participan -----------------

# bajamos la hoja modelo:

hoja_modelo <- read_sheet(ss = "1pKtIYjzWfhlHozra0bdxQ8Bdi72-LikWRCm2Zg371Gk",
                          sheet = "llamado seguimiento",
                          col_types = "c")

v_selec_cols <- colnames(hoja_modelo)

# Bajamos hojas con los links:

links_total <- read_sheet(ss = "1Vyzpb3BAkst5tft-2QQYDpJ74UQjwn585becN8gJKlE",
                          sheet = "links",
                          col_types = "c")

# con el for bajamos hoja de seguimiento (mismo drive, diferente hoja, reciclamos comando)

for (i in 1:nrow(links_total)) {
  datos_1 <- range_read(ss = links_total$LINK[i],
                        sheet = "llamado seguimiento 1",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols))
  
  datos_1$CETEC <- links_total$CETEC[i]
  
  hoja_modelo<- bind_rows(hoja_modelo, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 10
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

hoja_trabajo <- hoja_modelo

hoja_seguimiento_activos <- hoja_trabajo %>% filter(llamado_filtrar_SI == "SI")


# descargamos historicos

historicos_seguimiento <- read_sheet(ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
                                     sheet = "Base personas  participantes en SEGUIMIENTO",
                                     col_types = "c")


# hacemos cruce para solo quedarnos con personas nuevas

lista_seguimiento <- anti_join(hoja_seguimiento_activos,historicos_seguimiento,"DNI")

# hacemos el if else para subir:


if (nrow(lista_seguimiento) == 0) {
  print("No hay pacientes seguimiento ACTIVOS DBT nuevos")
} else {
  lista_seguimiento %>% 
    sheet_append(
      ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
      sheet = "Base personas  participantes en SEGUIMIENTO"
      )
    }


# Descargamos la lista actualizada de pacientes en seguimiento

Lista_historicos_seguimiento_actualizado <- read_sheet(ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
                                     sheet = "Base personas  participantes en SEGUIMIENTO",
                                     col_types = "c")


# Filtramos por los que asistieron al turno y que la fecha de turno sea hace mas de un mes y seleccionamos las columnas que queremos

Lista_historicos_seguimiento_actualizado <- Lista_historicos_seguimiento_actualizado %>% select(FECHA_DE_TURNO, RS:EDAD) %>% arrange(FECHA_DE_TURNO)


# Descargamos la lista de personas en seguimiento de la hoja de trabajo y cruzamos con la lista actualizada de seguimiento


Seguimiento_hoja_trabajo <- read_sheet(ss = "1vnyr5bvc41A8skvOVrFUPYCHMxAq3CwdFGWz7B1rD0k",
                                                       sheet = "Lista historicos seguimiento",
                                                       col_types = "c")


para_subir <- anti_join(Lista_historicos_seguimiento_actualizado,Seguimiento_hoja_trabajo,"DNI")


# Subimos los casos nuevos


if (nrow(para_subir) == 0) {
  print("No hay pacientes seguimiento nuevos")
} else {
  para_subir %>% 
    sheet_append(
      ss = "1vnyr5bvc41A8skvOVrFUPYCHMxAq3CwdFGWz7B1rD0k",
      sheet = "Lista historicos seguimiento")
    
}


#------------------- Descargamos las personas que no aceptaron el turno x autogestión -------------

# limpia ambiente

rm(list = ls()) 
options(error = beep)

fecha_actualizacion <- today()


# bajamos hoja modelo: 

hoja_modelo <- read_sheet(ss = "1pKtIYjzWfhlHozra0bdxQ8Bdi72-LikWRCm2Zg371Gk",
                          sheet = "llamado otorgamiento",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Bajamos hojas con los links:

links_total <- read_sheet(ss = "1Vyzpb3BAkst5tft-2QQYDpJ74UQjwn585becN8gJKlE",
                          sheet = "links",
                          col_types = "c")

# ahora usamos el for (comando de iteracion) para bajar todas las hojas de DBT:

for (i in 1:nrow(links_total)) {
  datos_1 <- range_read(ss = links_total$LINK[i],
                        sheet = "llamado otorgamiento",
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

# parseamos fechas:

hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_LLAMADO = date(parse_date_time(hoja_trabajo$FECHA_LLAMADO, c("%d%m%Y", "%Y%m%d"))))


# Filtramos por los que se autogestionaron el turno

DBT_AUTOGESTION <- hoja_trabajo %>% filter(SI_NO_SE_LE_ASIGNO_UN_TURNO_BARRERAS == "AUTOGESTION DE TURNO")

# Filtramos donde la fecha de llamado fue hace mas de un mes y nos quedamos con las columnas que queremos

DBT_AUTOGESTION_LISTA <- DBT_AUTOGESTION %>% select(FECHA_LLAMADO, RS:EDAD) 



# Descargamos la lista para cruzarla y actualizarla en caso de haber nuevos casos

Lista_autogestion_turno <- read_sheet(ss = "1vnyr5bvc41A8skvOVrFUPYCHMxAq3CwdFGWz7B1rD0k",
                                      sheet = "Autogestion de turno",
                                      col_types = "c")


para_subir_autog <- anti_join(DBT_AUTOGESTION_LISTA,Lista_autogestion_turno,"DNI")


# Subimos los casos nuevos


if (nrow(para_subir_autog) == 0) {
  print("No hay pacientes seguimiento nuevos")
} else {
  para_subir_autog %>% 
    sheet_append(
      ss = "1vnyr5bvc41A8skvOVrFUPYCHMxAq3CwdFGWz7B1rD0k",
      sheet = "Autogestion de turno")
}


# fecha de actualizacion:

actualizacion <- as_tibble(as.character(Sys.time()))

actualizacion %>% 
  sheet_write(
    ss = "1fJkWK-M4D72kfjLx4qWBuQ-p1Zr94BXGUPlRC-WQWQI",
    sheet = "ultima actualizacion")




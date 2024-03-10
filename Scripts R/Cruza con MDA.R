# Cruce MDA tickets 



# plantilla básica:

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

gs4_auth(email = "cami.vazqc@gmail.com")
drive_auth(email = "cami.vazqc@gmail.com")

fecha_actualizacion <- today()

# descargamos drive de trabajo: tickets nación:

tickets_nacion <- read_sheet(ss = "1u2V_DxOqiVykMx21Inwkj7GSZyQR9wzn_DLUn8CYB50",
                             sheet = "TOTAL tickets",
                             col_types = "c")

# ----------- primera cruza --------------- # 

# importamos excel de tickets totales abiertos:

tickets_totales <- read.csv("C:/Users/Administrador/Desktop/Cruza con MDA/Abiertos Tickets - 20230302.csv",sep=";")

?read.csv

# 1) tickets abiertos que NO estan en nuestro drive: usamos anti_join

# transformamos bases para la cruza


tickets_nacion_solo_ID <- tickets_nacion %>% select(ID) 

prueba <- as.character(tickets_nacion_solo_ID$ID)

tickets_nacion_solo_ID <- tickets_nacion_solo_ID %>% transmute(prueba) %>% rename(ID = prueba)

prueba_2 <- as.character(tickets_totales$Número.de.Ticket)

tickets_totales_abiertos <- tickets_totales %>% mutate(prueba_2) %>% select(-Número.de.Ticket) %>% rename(ID = prueba_2)

# hacemos la cruza:

tickets_nuevos <- anti_join(tickets_totales_abiertos,tickets_nacion_solo_ID,"ID")

# sacamos repetidos

tickets_nuevos <- tickets_nuevos %>%  distinct(ID,.keep_all = TRUE)


# subimos a la pagina: arnar codigo para subir automatico a la pagina

colnames(tickets_nuevos)

tickets_nuevos_a_subir <- tickets_nuevos %>% mutate( 
  Fecha = NA_character_, 
  ope = NA_character_, 
  URGENTE = NA_character_, 
  `Abierto/Cerrado` = "ABIERTOS" ,
  Estado = NA_character_, 
  `Última revisión` = NA_character_,
  Observaciones = NA_character_,
  `REABIERTO POR CIUDADANO` = NA_character_,
)

# ordenamos segun el orden de la planilla:

titckes_nuevos_a_subir_final <- tickets_nuevos_a_subir %>% select(
  Fecha,
  ope,
  ID,
  Estado,
  `Abierto/Cerrado`,
  Observaciones,
  `Última revisión`,
  Fecha.de.creación,
  Asunto,
  De,
  De.correo.electrónico,
  Temas.de.ayuda,
  Departamento,
  Estado.actual,
  Fecha.de.cierre,
  Respondió,
  Agente.asignado,
  Última.actualización,
  Atrasado,
  Info,
  CUILbis,
  Provincia,
  Localidad,
  URGENTE,
  `REABIERTO POR CIUDADANO`
  
)

# Subimos casos nuevos: 


if (nrow(titckes_nuevos_a_subir_final) != 0) {
  titckes_nuevos_a_subir_final %>% sheet_append(
    ss = "1u2V_DxOqiVykMx21Inwkj7GSZyQR9wzn_DLUn8CYB50",
    sheet = "TOTAL tickets") 
} else {
  print("No hay casos Nuevos para subir")
  
}



# guardamos ticktes nuevos (Opcional porque ya se suben automatico)

# write_xlsx(tickets_nuevos,"C:/Users/Usuario/Desktop/Cruza con MDA/tickets_nuevos.xlsx")


# ahora veamos los tickets que estan la base descargada y en nuestro drive de tickets_nacion como cerrados. (tickets reabiertos)

tickets_descargados_ID <- tickets_totales_abiertos %>% select(ID)

class(tickets_descargados_ID$ID)
class(tickets_nacion$ID)

tickets_ambas_bases <- inner_join(tickets_nacion,tickets_descargados_ID,"ID")

tickets_ambas_bases <- tickets_ambas_bases %>% rename(Abiertos_cerrados = `Abierto/Cerrado`) %>% filter(Abiertos_cerrados =="CERRADO")

colnames(tickets_ambas_bases)

# Descargamos base:

tickets_historico <- read_sheet(ss = "1u2V_DxOqiVykMx21Inwkj7GSZyQR9wzn_DLUn8CYB50",
                                sheet = "reabiertos a revisar",
                                col_types = "c")


nuevos_a_subir_reabiertos <- anti_join(tickets_ambas_bases,tickets_historico,"ID") %>% rename(`Abierto/Cerrado` = Abiertos_cerrados) %>% mutate(check = NA_character_ , ope = NA_character_) %>% 
  
  select(
    check,
    ope,
    Fecha,
    ID,
    Estado,
    `Abierto/Cerrado`,
    Observaciones,
    `Última revisión`,
    Fecha.de.creación,
    Asunto,
    De,
    De.correo.electrónico,
    Temas.de.ayuda,
    Departamento,
    Estado.actual,
    Fecha.de.cierre,
    Respondió,
    Agente.asignado,
    Última.actualización,
    Atrasado,
    Info,
    CUILbis,
    Provincia,
    Localidad,
    URGENTE,
  )


if (nrow(nuevos_a_subir_reabiertos) != 0) {
  nuevos_a_subir_reabiertos %>% sheet_append(
    ss = "1u2V_DxOqiVykMx21Inwkj7GSZyQR9wzn_DLUn8CYB50",
    sheet = "reabiertos a revisar") 
} else {
  print("No hay casos Nuevos  REABIERTOS  para subir")
  
}



#Por ahora esta parte no haría falta

# primer cruza completada con exito!!

# 2) Vamos con la segunda cruza: Asignar prioridad. Todos los tickets que esten en el csv de viajeros son URGENTES:

# cargamos base de viajeros:

# tickets_viajeros_urgentes <- read.csv("C:/Users/Usuario/Desktop/Cruza con MDA/TICKETS VIAJERES URGENTES Tickets - 20220906.csv",header = TRUE)

# transformamos base de datos para trabajar:

# tickets_viajeros_urgentes_2 <- tickets_viajeros_urgentes %>% select(Número.de.Ticket)

# le pegamos una columna al lado para mas visualización:

# URGENTE <- "SI"

# tickets_viajeros_urgentes_3 <- tickets_viajeros_urgentes_2 %>% mutate(URGENTE) %>% rename(ID = Número.de.Ticket)

# prueba_3 <- as.character(tickets_viajeros_urgentes_3$ID)

# tickets_viajeros_final <- tickets_viajeros_urgentes_3 %>% mutate(prueba_3) %>% select(prueba_3,URGENTE) %>% rename(ID = prueba_3)

# hacemos cruza para urgentes, un left_join quedaría bien:

# tickets_abiertos_nuevos_final_con_urgencia <- left_join(tickets_nuevos,tickets_viajeros_final,"ID")

# Borramos aquellos que tengan ID duplicado (repetidos)

# tickets_final <- tickets_abiertos_nuevos_final_con_urgencia %>% distinct(ID, .keep_all = TRUE)

# remplazar valores nulos: "replace(is.na(.), 0)" (Ver como usar esta mierda, me toma todo el df yo solo quiero una columna) 
# Lo hago con excel ya fue.

# exportamos

# write_xlsx(tickets_final,"C:/Users/Usuario/Desktop/tickets_final.xlsx")


# son personas con un turno asignado de la RS VI que central (RS XI) llama para
# Recordar el turno y reforzar asistencia

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

gs4_auth(email = "soporte.cetec.pba@gmail.com")
drive_auth(email = "soporte.cetec.pba@gmail.com")

fecha_actualizacion <- today()

# Descargamos hojas:


beraza <- read_sheet(ss = "1qr8JGUHIiaMgsu6TI8bUs9QaR-ywtf5K9p1G6-ejA3I",
                     sheet = "Hoja 1",
                     col_types = "c")

quilmes <- read_sheet(ss = "1HL_CiNfsOJlBwNk17FeHflzmDyXCPXDGPlYR982Ud8w",
                     sheet = "Hoja 1",
                     col_types = "c")

echeverria <- read_sheet(ss = "1qnUJJ33-wJ5wWEK4vId0Pio61i_8Iv9mWUWAEMpBgxg",
                         sheet = "Hoja 1",
                         col_types = "c")

lanus <- read_sheet(ss = "1V52pAmN6Vqh0mn09V-8l7auucmwuVtvMltS3e1-jt-k",
                    sheet = "Hoja 1",
                    col_types = "c")

lomas <- read_sheet(ss = "1qp_5hc3h4fSazvf5Tz29y-TDrenBCiKM8d5jEopNRA0",
                    sheet = "Hoja 1",
                    col_types = "c")

varela <- read_sheet(ss = "1gt56riCDUrAx2RkFZUKPUa4EedcDPAWbU0zSVhaC9R4",
                    sheet = "Hoja 1",
                    col_types = "c")

#datos basicos, tipo de dbt,fecha,horario,efector,profesional, le avisaron,observaciones

prueba <- NA

echeverria <- echeverria %>% mutate(prueba) %>% rename('TIPO DE DIABETES' = prueba) %>% rename(FECHA = 'FECHA DE TURNO')

colnames(beraza)

beraza_f <- beraza %>% select('APELLIDO Y NOMBRE','DNI','EDAD','TELEFONO 1','TELEFONO 2','MAIL','MUNICIPIO','LOCALIDAD','TIPO DE DIABETES','FECHA','HORARIO','EFECTOR','PROFESIONAL','¿Le avisaron a la persona del turno asignado?','OBSERVACIONES')

quilmes_f <- quilmes %>% select('APELLIDO Y NOMBRE','DNI','EDAD','TELEFONO 1','TELEFONO 2','MAIL','MUNICIPIO','LOCALIDAD','TIPO DE DIABETES','FECHA','HORARIO','EFECTOR','PROFESIONAL','¿Le avisaron a la persona del turno asignado?','OBSERVACIONES')

echeverria_f <- echeverria %>% select('APELLIDO Y NOMBRE','DNI','EDAD','TELEFONO 1','TELEFONO 2','MAIL','MUNICIPIO','LOCALIDAD','TIPO DE DIABETES','FECHA','HORARIO','EFECTOR','PROFESIONAL','¿Le avisaron a la persona del turno asignado?','OBSERVACIONES')

lomas_f <- lomas %>% select('APELLIDO Y NOMBRE','DNI','EDAD','TELEFONO 1','TELEFONO 2','MAIL','MUNICIPIO','LOCALIDAD','TIPO DE DIABETES','FECHA','HORARIO','EFECTOR','PROFESIONAL','¿Le avisaron a la persona del turno asignado?','OBSERVACIONES')

lanus_f <- lanus %>% select('APELLIDO Y NOMBRE','DNI','EDAD','TELEFONO 1','TELEFONO 2','MAIL','MUNICIPIO','LOCALIDAD','TIPO DE DIABETES','FECHA','HORARIO','EFECTOR','PROFESIONAL','¿Le avisaron a la persona del turno asignado?','OBSERVACIONES')

varela_f <- varela %>% select('APELLIDO Y NOMBRE','DNI','EDAD','TELEFONO 1','TELEFONO 2','MAIL','MUNICIPIO','LOCALIDAD','TIPO DE DIABETES','FECHA','HORARIO','EFECTOR','PROFESIONAL','¿Le avisaron a la persona del turno asignado?','OBSERVACIONES')


derivaciones_total <- bind_rows(beraza_f,quilmes_f,echeverria_f,lomas_f,lanus_f, varela_f)


ESTADO_LLAMADO <- NA
FECHA_LLAMADO <- NA
SE_HIZO_RECORDATORIO_DEL_TURNO <- NA


derivaciones_con_turno <- derivaciones_total %>% filter(!is.na(FECHA))

derivaciones_con_turno <- derivaciones_con_turno %>% mutate(ESTADO_LLAMADO,FECHA_LLAMADO,SE_HIZO_RECORDATORIO_DEL_TURNO)

# agregamos casos nuevos 

# descargamos historicos:

historico <- read_sheet(ss = "1fuNl67N0ORwcbILbpon-AjJCRLxhHZN_1GlAs_GGHJ8",
                        sheet = 'Recordatorio',
                        col_types = "c")

lista_nuevos <- anti_join(derivaciones_con_turno,historico,"DNI")


# Subimos casos nuevos:


if (nrow(lista_nuevos) != 0) {
  lista_nuevos %>% sheet_append(
    ss = "1fuNl67N0ORwcbILbpon-AjJCRLxhHZN_1GlAs_GGHJ8",
    sheet = "Recordatorio") 
} else {
  print("No hay recordatorios nuevos para subir")
  
}


# Analisis de datos RENAL

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

gs4_auth(email = "cami.vazqc@gmail.com")
drive_auth(email = "cami.vazqc@gmail.com")

fecha_actualizacion <- today()

############################ HOJA OFICIAL ##############################

# descargamos la hoja modelo (donde ir bajando los datos)

hoja_modelo <- read_sheet(ss = "11XeIRzmIWPYhKfxmal0EoAj1-n-JT9UVB116HuWvtrk",
                          sheet = "HOJA OFICIAL",
                          col_types = "c",
                          skip = 1)

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas

links <- read_sheet(ss = "1eif8m9ajOdTZUfJ3T-C9diFtCimu8YOdAuYXZkf3p_g",
                         sheet = "links",
                         col_types = "c")

# ahora usamos el for (comando de iteraci?n) para bajar todas las hojas:

for (i in 1:nrow(links)) {
        datos_1 <- range_read(ss = links$Link[i],
                              sheet = "HOJA OFICIAL",
                              col_types = "c",
                              skip=1)
        
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

hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_DE_LLAMADO = date(parse_date_time(hoja_trabajo$FECHA_DE_LLAMADO, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_DE_NACIMIENTO = date(parse_date_time(hoja_trabajo$FECHA_DE_NACIMIENTO, c("%d%m%Y", "%Y%m%d"))))

# Realizamos estadisticas de base de datos:

# Categorización

CATEGORIZACION <- hoja_trabajo %>% filter(CONTACTADX=="SI 1° CONTACTO") %>%  count(CATEGORIZACION)
 
sheet_write(CATEGORIZACION,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "CATEGORIZACION")

base_renal <- hoja_trabajo %>% filter( DNI != "") %>% count(CETEC) 

sheet_write(base_renal,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "base")

# Personas sin llamar:

sin_llamar <- hoja_trabajo %>% filter(FALLECIDX == "NO" | is.na(FALLECIDX) | FALLECIDX=="No") %>% filter(CONTACTADX == "SIN LLAMAR")  %>% count(CETEC) 


sheet_write(sin_llamar,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "Sin llamar")

# Estado del llamado:

contactados <- hoja_trabajo %>% filter( DNI != "") %>%  count(CETEC, CONTACTADX) %>%  arrange(CETEC)

sheet_write(contactados,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "contactados")


# Fallidos:

Fallidos_1er_contacto <- hoja_trabajo %>% count(CETEC, CANTIDAD_DE_FALLIDXS)

sheet_write(Fallidos_1er_contacto,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "Fallidos_1er_contacto")


# Fallecidos

Fallecidos <- hoja_trabajo %>% filter (FALLECIDX=="SI" ) %>%  count(CETEC)

sheet_write(Fallecidos,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "Fallecidxs")

# Llamados por mes por CETEC

diciembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_LLAMADO >= "2022-12-01" & FECHA_DE_LLAMADO <= "2022-12-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDXS,CETEC)

sheet_write(diciembre_cetec,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "Diciembre_cetec")

enero_cetec <- hoja_trabajo %>% filter(FECHA_DE_LLAMADO  >= "2023-01-01" & FECHA_DE_LLAMADO <= "2023-01-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDXS,CETEC)

sheet_write(enero_cetec,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "Enero_cetec")

febrero_cetec <- hoja_trabajo %>% filter(FECHA_DE_LLAMADO  >= "2023-02-01" & FECHA_DE_LLAMADO <= "2023-02-28") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDXS,CETEC)

sheet_write(febrero_cetec,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "Febrero_cetec")

entrevista_categoria <- hoja_trabajo %>% filter(!is.na(DNI)) %>%  count(FUISTE_A_LA_1RA_ENTREVISTA_CON_NEFROLOGX_DE_CT,CATEGORIZACION)


sheet_write(entrevista_categoria,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "entrevista_categoria")

# fecha de actualizacion:

actualizacion <- as_tibble(as.character(Sys.time()))

actualizacion %>%  sheet_write(
  ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
  sheet = "ultima actualizacion")


############################## PACIENTE 0 ##############################

# En este caso es al pedo usar un FOR cuando tenemos una sola planilla. Descargamos directamente.

# descargamos la hoja modelo (donde ir bajando los datos)

# hoja_modelo2 <- read_sheet(ss = "11XeIRzmIWPYhKfxmal0EoAj1-n-JT9UVB116HuWvtrk",
                         # sheet = "PACIENTE_0_(no asistió a 1° entrevista)",
                          #col_types = "c") #

# creamos vector con nombre de las columnas

# v_selec_cols2 <- colnames(hoja_modelo2)


# ahora usamos el for (comando de iteraci?n) para bajar todas las hojas:

# for (i in 1:nrow(links)) {
 # datos_1 <- range_read(ss = links$Link[i],
                     #   sheet = "PACIENTE_0_(no asistió a 1° entrevista)",
                      #  col_types = "c")
  
#  datos_1 <- datos_1 %>% 
  #  select(all_of(v_selec_cols2))
  
 # datos_1$CETEC <- links$CETEC[i]
  
#  hoja_modelo2<- bind_rows(hoja_modelo2, 
                #          datos_1)
  
 # rm(datos_1)
  
 # pausa <- 5
#  print(str_c("pausando por ", pausa))
 # Sys.sleep(pausa)
  
# }


# descargamos hoja de trabajo:


# hoja_trabajo2 <- hoja_modelo2


hoja_trabajo2 <- read_sheet(ss = "1wbW2MQZvQkHIarcpVD_NTax309w_Ma6NlWk0nazxhN4",
                  sheet = "PACIENTE_0_(no asistió a 1° entrevista)",
                   col_types = "c")



# LLAMADO PRESENACION

mail_a_CT_turno_1er_ent <- hoja_trabajo2 %>% count(SE_ENVIA_MAIL_A_CT_SOLICITANDO_TURNO_1er_ENTREVISTA)

sheet_write(mail_a_CT_turno_1er_ent,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "mail_a_CT_turno_1er_ent")

cobertura_social <- hoja_trabajo2 %>% count(POSEE_COBERTURA_SOCIAL)

sheet_write(cobertura_social,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "cobertura_social")

año_inicio_dialisis <- hoja_trabajo2 %>% count(AÑO_QUE_INICIO_DIALISIS)

sheet_write(año_inicio_dialisis,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "año_inicio_dialisis")

medico_de_cabecera <- hoja_trabajo2 %>% count(TIENE_MEDICX_DE_CABECERA)

sheet_write(medico_de_cabecera,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "medico_de_cabecera")

toma_medicacion_indicada <- hoja_trabajo2 %>% count(TOMA_LA_MEDICACION_INDICADA)

sheet_write(toma_medicacion_indicada,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "toma_medicacion_indicada")

como_obtiene_medicacion <- hoja_trabajo2 %>% count(COMO_OBTIENE_LA_MEDICACION)

sheet_write(como_obtiene_medicacion,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "como_obtiene_medicacion")

atencion_CD <- hoja_trabajo2 %>% count(COMO_CALIFICARIA_LA_ATENCION_DEL_CENTRO_DE_DIALISIS)

sheet_write(atencion_CD,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "atencion_CD")

referente_en_CD <- hoja_trabajo2 %>% count(TIENE_UNX_REFERENTE_EN_EL_CENTRO_DE_DIALISIS)

sheet_write(referente_en_CD,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "referente_en_CD")

transporte_al_CD <- hoja_trabajo2 %>% count(EN_QUE_SE_TRASLADA_AL_CENTRO_DE_DIALISIS)

sheet_write(transporte_al_CD,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "transporte_al_CD")

dias_dialisis <- hoja_trabajo2 %>% count(DIAS_QUE_SE_DIALIZA)

sheet_write(dias_dialisis,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "dias_dialisis")

ausentismo_ultimo_mes <- hoja_trabajo2 %>% count(SE_HA_TENIDO_QUE_AUSENTAR_AL_TRATAMIENTO_DE_DIALISIS_EN_EL_ULTIMO_MES)

sheet_write(ausentismo_ultimo_mes,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "ausentismo_ultimo_mes")

motivos_ausentismo <- hoja_trabajo2 %>% count(SI_LA_RTA_ANTERIOR_FUE_SI_DETALLE_MOTIVOS)

sheet_write(motivos_ausentismo,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "motivos_ausentismo")

## ACOMPAÑAMIENTO

acompañamiento_desde_CD <- hoja_trabajo2 %>% count(RECIBE_ACOMPAÑAMIENTO_DESDE_EL_CENTRO_DE_DIALISIS)

sheet_write(acompañamiento_desde_CD,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "acompañamiento_desde_CD")

otro_tipo_acompañamiento <- hoja_trabajo2 %>% count(RECIBE_ALGUN_OTRO_TIPO_DE_ACOMPAÑAMIENTO)

sheet_write(otro_tipo_acompañamiento,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "otro_tipo_acompañamiento")

acompañamiento_desde_CETEC <- hoja_trabajo2 %>% count(LE_GUSTARIA_RECIBIR_ACOMPAÑAMIENTO_DESDE_CENTRO_DE_TELESALUD_Y_CUIDADOS)

sheet_write(acompañamiento_desde_CETEC,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "acompañamiento_desde_CETEC")

# SEGUNDO LLAMADO

puede_asistir_turno <- hoja_trabajo2 %>% count(PUEDE_ASISTIR_AL_TURNO...47)

sheet_write(puede_asistir_turno,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "puede_asistir_turno")

motivos_inasistencia_1ra_entrevista <- hoja_trabajo2 %>% count(SI_NO_PUEDE_INDICAR_MOTIVOS_Y_REPROGRAMAR_TURNO...48)

sheet_write(motivos_inasistencia_1ra_entrevista,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "motivos_inasistencia_1ra_entrevista")

mail_CT_reprogramando_1er_ent <- hoja_trabajo2 %>% count(SE_ENVIA_MAIL_A_CT_SOLICITANDO_REPROGRAMACION_PARA_1ER_ENTREVISTA...50)

sheet_write(mail_CT_reprogramando_1er_ent,
            ss = "1iReRB5U3DwdXl3TSu9uyDyjagJT6NJSyiNh3IVrkJYM",
            sheet = "mail_CT_reprogramando_1er_ent")

# TERCER LLAMADO (seguir)




############################# PACIENTE BIS #############################

# descargamos la hoja modelo (donde ir bajando los datos)

hoja_modelo3 <- read_sheet(ss = "11XeIRzmIWPYhKfxmal0EoAj1-n-JT9UVB116HuWvtrk",
                          sheet = "PACIENTE_BIS_(asistió a 1° entrevista)",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols3 <- colnames(hoja_modelo3)


# ahora usamos el for (comando de iteraci?n) para bajar todas las hojas:

for (i in 1:nrow(links)) {
  datos_1 <- range_read(ss = links$Link[i],
                        sheet = "PACIENTE_BIS_(asistió a 1° entrevista)",
                        col_types = "c")
  
  datos_1 <- datos_1 %>% 
    select(all_of(v_selec_cols3))
  
  datos_1$CETEC <- links$CETEC[i]
  
  hoja_modelo3<- bind_rows(hoja_modelo3, 
                          datos_1)
  
  rm(datos_1)
  
  pausa <- 5
  print(str_c("pausando por ", pausa))
  Sys.sleep(pausa)
  
}

hoja_trabajo3 <- hoja_modelo3



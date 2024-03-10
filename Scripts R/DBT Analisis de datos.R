# Script de diabetes - bajada -  analisis de los datos:

library(dplyr)
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


# limpia ambiente
rm(list = ls()) 
options(error = beep)

# definir variables globales
fecha_actualizacion <- today()

# autorizar usuario

gs4_auth(email = "soporte.cetec.pba@gmail.com")
drive_auth(email = "soporte.cetec.pba@gmail.com")

gs4_auth(email = "cami.vazqc@gmail.com")
drive_auth(email = "cami.vazqc@gmail.com")



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

for (i in 20:nrow(links_total)) {
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

#--------------------- Necesitamos una base de personas participantes ------------------

DBT_activo <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(CONTACTADX == "SI") %>% arrange(CETEC) 

# bajamos historico

DBT_activo_historico <- read_sheet(ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
                                   sheet = "Base personas  participantes en OTORGAMIENTO TURNO",
                                   col_types = "c")

lista_DBT_activo <- anti_join(DBT_activo,DBT_activo_historico,"DNI")

# Usamos el if condicional para subir


if (nrow(lista_DBT_activo) == 0) {
  print("No hay pacientes ACTIVOS DBT nuevos")
} else {
  lista_DBT_activo %>% 
    sheet_append(
      ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
      sheet = "Base personas  participantes en OTORGAMIENTO TURNO"
    )
}


# -----------------------------------------------------------------------------#

# estadisticas de la base:

base <- hoja_trabajo %>% filter(!is.na(DNI)) %>% count(CETEC) %>% arrange(CETEC)

sheet_write(base,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "base" )

# promedio de edad de las personas

edad <- hoja_trabajo %>% filter(!is.na(DNI) & !is.na(EDAD)) %>% filter(EDAD != "#VALUE!") %>% filter(EDAD != "122.7")

edad_a <- as.numeric(edad$EDAD) 

edad_final <- round(edad_a) 

prueba <- mean(edad_final,trim = 0,na.rm = TRUE)

edad <- "EDAD"
edad_a <- prueba

edad_final_final <- data.frame(edad,edad_a)

sheet_write(edad_final_final,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Edad")

# Estadisticas de llamados:

# Fallecidos:

fallecidos <- hoja_trabajo %>% filter(FALLECIDX == "SI") %>% count(CETEC)

sheet_write(fallecidos,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "fallecidos" )

# Contactados:

contactados <- hoja_trabajo %>% filter(!is.na(DNI)) %>% count(CONTACTADX,CETEC)

sheet_write(contactados,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "contactados")


# Llamados fallidos:

fallidos <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% count(FALLIDX,CETEC)

sheet_write(fallidos,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "fallidos")

#  Tipo de DBT:

tipo_dbt <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(!is.na(DNI)) %>% count(TIPO_DBT)

sheet_write(tipo_dbt,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "tipo_dbt")

# activo en el resapro 

activo_resapro <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% filter(!is.na(DNI)) %>% count(ACTIVX_EN_RESAPRO)

sheet_write(activo_resapro,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "activo_resapro")

# Obra social

os <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% filter(!is.na(DNI)) %>% count(OBRA_SOCIAL)

sheet_write(os,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "os")

# fecha ultimo control:

ultimo_control <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% filter(!is.na(DNI)) %>% count(FECHA_DE_ULTIMO_CONTROL,PRIORIZACION_PARA_ASIGANCION_DE_TURNO_PROTEGIDO)

sheet_write(ultimo_control,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "ultimo_control")

# Año diagnostico:

año_diagnostico <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(SOLO_AÑO_DE_DIAGNOSTICO_DE_DBT != "(RECORDAR PONER SOLO EL AÑO)") %>% count(SOLO_AÑO_DE_DIAGNOSTICO_DE_DBT) %>% arrange(desc(n))

sheet_write(año_diagnostico,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "año_diagnostico")


# TIEMPO_DE_EVOLUCION_DE_LA_DBT

TIEMPO_DE_EVOLUCION_DE_LA_DBT <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% count(TIEMPO_DE_EVOLUCION_DE_LA_DBT) %>% arrange(desc(n))

sheet_write(TIEMPO_DE_EVOLUCION_DE_LA_DBT,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "TIEMPO_DE_EVOLUCION_DE_LA_DBT")


# tipo de insulina:

tipo_insulina <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% filter(TRATAMIENTO_INSULINA == "SI") %>% count(TIPO_DE_INSULINA)

sheet_write(tipo_insulina,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "tipo_insulina")


# tipo de pastillas

tipos_pastillas <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% filter(TRATAMIENTO_PASTILLAS == "SI") %>% count(CUALES_PASTILLAS)

sheet_write(tipos_pastillas,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "tipos_pastillas")

#  otro medicamentos

otros_medic <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(OTROS_MEDICAMENTOS) %>% filter(!is.na(OTROS_MEDICAMENTOS)) %>% arrange(desc(n))

sheet_write(otros_medic,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "otros_medic")


#  CONTROLES_PERIODICOS

controles_periodicos <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(CONTROLES_PERIODICOS,PRIORIZACION_PARA_ASIGANCION_DE_TURNO_PROTEGIDO)

sheet_write(controles_periodicos,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "controles_periodicos")

# Motivo de suspensión de tratamiento:

MOTIVO_DE_SUSPENSION <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(MOTIVO_DE_SUSPENSION_DE_CONTROLES_PERIODICOS_Y_O_TRATAMIENTOS) %>% arrange(desc(n))

sheet_write(MOTIVO_DE_SUSPENSION,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "MOTIVO_DE_SUSPENSION")

# AÑO_DE_ULTIMO_LABORATORIO 

AÑO_DE_ULTIMO_LABORATORIO <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% filter(!is.na(DNI)) %>% filter(AÑO_DE_ULTIMO_LABORATORIO != "(RECORDAR PONER SOLO EL AÑO)") %>% count(AÑO_DE_ULTIMO_LABORATORIO) %>% arrange(desc(n))

sheet_write(AÑO_DE_ULTIMO_LABORATORIO,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "AÑO_DE_ULTIMO_LABORATORIO")

#  VIAS_ALTERNATIVAS_DE_OBTENCION_DE_MEDICAMENTOS_ O_INSUMOS

VIAS_ALTERNATIVAS_DE_OBTENCION_DE_MEDICAMENTOS_O_INSUMOS <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% filter(!is.na(DNI)) %>% count(VIAS_ALTERNATIVAS_DE_OBTENCION_DE_MEDICAMENTOS_O_INSUMOS)

sheet_write(VIAS_ALTERNATIVAS_DE_OBTENCION_DE_MEDICAMENTOS_O_INSUMOS,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "VIAS_ALTERNATIVAS_DE_OBTENCION_DE_MEDICAMENTOS_O_INSUMOS")

#  ENFERMEDADES_CONCOMITANTES

ENFERMEDADES_CONCOMITANTES <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(ENFERMEDADES_CONCOMITANTES)

sheet_write(ENFERMEDADES_CONCOMITANTES,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "ENFERMEDADES_CONCOMITANTES")
 # PIES

PIES <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(PIES)

# OJO

OJOS <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(OJOS)

# PIEL

PIEL <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(PIEL)


sheet_write(PIES,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "PIES")

sheet_write(OJOS,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "OJOS")

sheet_write(PIEL,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "PIEL")

# --------------------------- TBQ ---------------------------

FUMA <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(FUMA)

EDAD_COMIENZO <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(FUMA == "SI") %>% count(EDAD_DE_COMIENZO)

CANTIDAD_DE_AÑOS_DE_FUMADOR <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(FUMA == "SI") %>% count(CANTIDAD_DE_AÑOS_DE_FUMADOR) %>% arrange(desc(n))

CANTIDAD_DE_CIGARRILLOS_POR_DIA <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(FUMA == "SI") %>% count(CANTIDAD_DE_CIGARRILLOS_POR_DIA) %>% arrange(desc(n))

PACKYEAR <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(FUMA == "SI") %>% count(PACKYEAR_sale_calculo_por_formula) %>% arrange(desc(n))

# deseo dejar de fumar

DESEO_DE_DEJAR_DE_FUMAR <- hoja_trabajo %>% filter(FALLECIDX != "SI") %>% filter(FUMA == "SI") %>% count(DESEO_DE_DEJAR_DE_FUMAR)

sheet_write(FUMA,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "FUMA")

sheet_write(EDAD_COMIENZO,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "EDAD_COMIENZO")

sheet_write(CANTIDAD_DE_AÑOS_DE_FUMADOR,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "CANTIDAD_DE_AÑOS_DE_FUMADOR")

sheet_write(PACKYEAR,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "PACKYEAR")

sheet_write(CANTIDAD_DE_CIGARRILLOS_POR_DIA,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "CANTIDAD_DE_CIGARRILLOS_POR_DIA")

sheet_write(DESEO_DE_DEJAR_DE_FUMAR,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "DESEO_DE_DEJAR_DE_FUMAR")


# Derivacion a TBQ

base_TBQ_1 <- hoja_trabajo %>% filter(FUMA == "SI" & DESEO_DE_DEJAR_DE_FUMAR == "SI" )

base_TBQ_2 <- hoja_trabajo %>% filter(FUMA == "Si" & DESEO_DE_DEJAR_DE_FUMAR == "Si" )

base_TBQ <- rbind(base_TBQ_1,base_TBQ_2)

base_tbq_seleccionada <- base_TBQ  %>% select(RS:OBRA_SOCIAL) %>% mutate(ASIGNADX_A_CETEC = NA_character_)

base_tbq_historico <- read_sheet(ss = "1iS3zhLQ7y0It4Y0AbtNtCMDvOHM57RsNxo2OgfSdJBg",
                                 sheet = "TBQ DBT",
                                 col_types = "c")

lista_tbq <- anti_join(base_tbq_seleccionada,base_tbq_historico,"DNI")

casos_a_subir <- lista_tbq %>% count(MUNICIPIO)

# ¿hay casos nuevos para subir?

if (nrow(lista_tbq) == 0) {
  print("No hay pacientes de DBT para linea TBQ nuevos")
} else {
  lista_tbq %>% 
    sheet_append(
      ss = "1iS3zhLQ7y0It4Y0AbtNtCMDvOHM57RsNxo2OgfSdJBg",
      sheet = "TBQ DBT"
    )
}

# --------------------------------- SEGUNDA PARTE ---------------- #

vacuna_covid <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(CANTIDAD_DE_DOSIS)

motivo_no_Se_vacuno <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% filter(CANTIDAD_DE_DOSIS == "NINGUNA" | CANTIDAD_DE_DOSIS == "1") %>%  count(EN_EL_CASO_DE_QUE_NO_SE_HAYA_APLICADO_la_VACUNA_contra_COVID_PREGUNTAR_MOTIVO_POR_EL_CUAL_NO_SE_LA_ADMINSTRO)

antigripal <- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(INMUNIZACIONES_Antigripal_anual)

neumo<- hoja_trabajo %>% filter(!is.na(DNI)) %>% filter(FALLECIDX != "SI") %>% filter(CONTACTADX == "SI") %>% count(INMUNIZACIONES_Neumonia)

sheet_write(vacuna_covid,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "vacuna_covid")

sheet_write(motivo_no_Se_vacuno,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "motivo_no_Se_vacuno")

sheet_write(antigripal,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "antigripal")

sheet_write(neumo,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "neumo")


# ------------------------------ TERCERA PARTE -------------------------------# 

# analisis de turnos/efectores

estado_tratamiento <- hoja_trabajo %>% filter(CONTACTADX == "SI") %>% filter(FALLECIDX != "SI") %>%  filter(!is.na(DNI)) %>% count(ESTADO_DEL_TRATAMIENTO_Y_CONTROLES_completa_operador,OBRA_SOCIAL)

# Priorizacion

priorizacion_insulina_pastillas <- hoja_trabajo %>% filter(CONTACTADX == "SI") %>% filter(FALLECIDX != "SI") %>%  filter(!is.na(DNI)) %>% count(TRATAMIENTO_INSULINA,TRATAMIENTO_PASTILLAS,PRIORIZACION_PARA_ASIGANCION_DE_TURNO_PROTEGIDO)

# FECHA control y priorizacion

fecha_control_priorizacion <- hoja_trabajo %>% filter(CONTACTADX == "SI") %>% filter(FALLECIDX != "SI") %>%  filter(!is.na(DNI)) %>% count(FECHA_DE_ULTIMO_CONTROL,PRIORIZACION_PARA_ASIGANCION_DE_TURNO_PROTEGIDO)

# Turno

turno <- hoja_trabajo %>% filter(CONTACTADX == "SI") %>% filter(FALLECIDX != "SI") %>%  filter(!is.na(DNI)) %>% count(OBRA_SOCIAL,ASIGNACION_DE_TURNO)

# BARRERAS DE NO ASIGNACION DE TURNO (A PERSONAS SIN COBERTURA)

barreras_turno <- hoja_trabajo %>% filter(CONTACTADX == "SI") %>% filter(FALLECIDX != "SI") %>%  filter(!is.na(DNI)) %>% filter(ASIGNACION_DE_TURNO == "NO") %>% filter(OBRA_SOCIAL == "NO") %>% count(SI_NO_SE_LE_ASIGNO_UN_TURNO_BARRERAS)

CENTRO_DE_SALUD_DONDE_SE_ASIGNA_EL_TURNO <- hoja_trabajo %>% filter(CONTACTADX == "SI") %>% filter(FALLECIDX != "SI") %>%  filter(!is.na(DNI)) %>% filter(ASIGNACION_DE_TURNO == "SI") %>% count(CENTRO_DE_SALUD_DONDE_SE_ASIGNA_EL_TURNO) %>% arrange(desc(n))

salud_mental <- hoja_trabajo %>% filter(CONTACTADX == "SI") %>% filter(FALLECIDX != "SI") %>%  filter(!is.na(DNI)) %>% count(ACEPTO_EL_OFRECIMIENTO_DEL_ESPACIO_DE_ACOMPAÑAMIENTO_DE_SALUD_MENTAL) 

charlas <- hoja_trabajo %>% filter(CONTACTADX == "SI") %>% filter(FALLECIDX != "SI") %>%  filter(!is.na(DNI)) %>% count(DESEA_SER_CONTACTADX_PARA_PARTICIPAR_DE_ACTIVIDADES_GRUPALES_O_TALLERES_O_CHARLAS)

sheet_write(estado_tratamiento,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "estado_tratamiento")

sheet_write(priorizacion_insulina_pastillas,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "priorizacion")

sheet_write(fecha_control_priorizacion,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "fecha_control_priorizacion")

sheet_write(turno,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "turno")

sheet_write(barreras_turno,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "barreras_turno")

sheet_write(CENTRO_DE_SALUD_DONDE_SE_ASIGNA_EL_TURNO ,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "CENTRO_DE_SALUD_DONDE_SE_ASIGNA_EL_TURNO ")

sheet_write(salud_mental,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "salud_mental")

sheet_write(charlas,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "charlas")




#-------------------- LLAMADOS POR MES POR CETEC ---------------------------------------



Mayo_cetec <- hoja_trabajo %>% filter(FECHA_LLAMADO  >= "2022-05-01" & FECHA_LLAMADO <= "2022-05-31") %>% count(CONTACTADX,FALLIDX,CETEC)

Junio_cetec <- hoja_trabajo %>% filter(FECHA_LLAMADO  >= "2022-06-01" & FECHA_LLAMADO <= "2022-06-30") %>% count(CONTACTADX,FALLIDX,CETEC)

Julio_cetec <- hoja_trabajo %>% filter(FECHA_LLAMADO  >= "2022-07-01" & FECHA_LLAMADO <= "2022-07-31") %>% count(CONTACTADX,FALLIDX,CETEC)

Agosto_cetec <- hoja_trabajo %>% filter(FECHA_LLAMADO  >= "2022-08-01" & FECHA_LLAMADO <= "2022-08-31") %>% count(CONTACTADX,FALLIDX,CETEC)

Septiembre_cetec <- hoja_trabajo %>% filter(FECHA_LLAMADO  >= "2022-09-01" & FECHA_LLAMADO <= "2022-09-30") %>% count(CONTACTADX,FALLIDX,CETEC)

Octubre_cetec <- hoja_trabajo %>% filter(FECHA_LLAMADO  >= "2022-10-01" & FECHA_LLAMADO <= "2022-10-31") %>% count(CONTACTADX,FALLIDX,CETEC)

Noviembre_cetec <- hoja_trabajo %>% filter(FECHA_LLAMADO  >= "2022-11-01" & FECHA_LLAMADO <= "2022-11-30") %>% count(CONTACTADX,FALLIDX,CETEC)

Diciembre_cetec <- hoja_trabajo %>% filter(FECHA_LLAMADO  >= "2022-12-01" & FECHA_LLAMADO <= "2022-12-31") %>% count(CONTACTADX,FALLIDX,CETEC)


sheet_write(Mayo_cetec,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Mayo_cetec")

sheet_write(Junio_cetec,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Junio_cetec")

sheet_write(Julio_cetec,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Julio_cetec")

sheet_write(Agosto_cetec,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Agosto_cetec")

sheet_write(Septiembre_cetec,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Septiembre_cetec")

sheet_write(Octubre_cetec,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Octubre_cetec")

sheet_write(Noviembre_cetec,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Noviembre_cetec")

sheet_write(Diciembre_cetec,
            ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
            sheet = "Diciembre_cetec")


#-------------------------- Necesitamos consolidado base seguimiento que participa -----------------

# bajamos la otra hoja modelo:

hoja_modelo <- read_sheet(ss = "1pKtIYjzWfhlHozra0bdxQ8Bdi72-LikWRCm2Zg371Gk",
                          sheet = "llamado seguimiento",
                          col_types = "c")

v_selec_cols <- colnames(hoja_modelo)

# Bajamos hojas con los links:

links_total <- read_sheet(ss = "1Vyzpb3BAkst5tft-2QQYDpJ74UQjwn585becN8gJKlE",
                          sheet = "links",
                          col_types = "c")

# con el for bajamos hoja de seguimiento (mismo drive, diferente hoja, reciclamos comando)

for (i in 19:nrow(links_total)) {
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

colnames(hoja_seguimiento_activos)

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

# FECHA ACTUALIZACION

actualizacion <- as_tibble(as.character(Sys.time()))

actualizacion %>% 
        sheet_write(
                ss = "1xXhwNe1bvro3zJZZzdyn9WrG36_4_FsjYtXWqdZzTEY",
                sheet = "actualizacion")



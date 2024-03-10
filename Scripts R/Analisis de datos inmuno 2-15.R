# Script Inmunizaciones 2- 15 años: Centralización y analisis de los datos.

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

hoja_modelo <- read_sheet(ss = "1RlwZ_MGvmVr8xYoVQQv_kCYv7N2GNJHGauTtTnIfJbM",
                          sheet = "Base de datos",
                          col_types = "c")

# creamos vector con nombre de las columnas

v_selec_cols <- colnames(hoja_modelo)

# Descargamos las planillas de AMBA:

links_AMBA <- read_sheet(ss = "1dXhwuElAq2TUIBGHgsmmIkDLo3ZiHRLG5JxokjuhadI",
                         sheet = "links",
                         col_types = "c")

# descargamos planillas de interior

links_interior <- read_sheet(ss = "13fwn8G3LcnCI0SKsCAGnnM26KoT7H_kdpWS1PwCoPfI",
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



hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_DE_PRIMER_CONTACTO = date(parse_date_time(hoja_trabajo$FECHA_DE_PRIMER_CONTACTO, c("%d%m%Y", "%Y%m%d"))))
hoja_trabajo <- hoja_trabajo %>%  mutate(FECHA_DE_SEGUNDO_CONTACTO = date(parse_date_time(hoja_trabajo$FECHA_DE_SEGUNDO_CONTACTO, c("%d%m%Y", "%Y%m%d"))))


# estado de caso:

estado_de_caso_cetec <- hoja_trabajo %>% filter(ESTADO_DE_CASO == "Para primer contacto" | ESTADO_DE_CASO == "Para segundo contacto") %>% count(ESTADO_DE_CASO,CETEC)

sheet_write(estado_de_caso_cetec,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "estado_de_caso_cetec")


# Realizamos estadisticas de base de datos:


base_inmunizaciones <- hoja_trabajo %>% count(CETEC) %>% arrange(CETEC)

sheet_write(base_inmunizaciones,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "base")

# Personas sin llamar (no nos contactamos nunca, sin llamar sin fallidos)

sin_llamar <- hoja_trabajo %>% filter(FALLECIDX == "No" | is.na(FALLECIDX)) %>% filter(CONTACTADX == "Sin llamar") %>% filter(ESTADO_DE_CASO != 'Fin (OTROS)') %>% count(CETEC)

sheet_write(sin_llamar,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "Sin llamar")

# Estado del caso:

estado_del_caso <- hoja_trabajo %>% count(ESTADO_DE_CASO)

sheet_write(estado_del_caso,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "estado del caso")

# Contactados:

contactados <- hoja_trabajo %>% count(CETEC,CONTACTADX) 

sheet_write(contactados,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "contactados")

# Fallidos:

fallidos_1er_contacto <- hoja_trabajo %>% count(CETEC,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO)

sheet_write(fallidos_1er_contacto,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "fallidos_1er_contacto")

fallidos_2do_contacto <- hoja_trabajo %>% count(CETEC,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

sheet_write(fallidos_2do_contacto,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "fallidos_2do_contacto")

colnames(hoja_trabajo)

# vacunas:

SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>%  count(SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL)

VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB)

SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO) # (filtro con contactados para sacar casos de mas)?)

SE_VACUNO_CON_ANTINEUMOCOCCICA_13V <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_VACUNO_CON_ANTINEUMOCOCCICA_13V)

SE_APLICO_VACUNA_COVID <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto")  %>% count(SE_APLICO_VACUNA_COVID)

asistencia <- hoja_trabajo %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(ASISTIRA_A_VACUNARSE_CON_LA_VACUNA_FALTANTE)


# subimos todos:

sheet_write(SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL")


sheet_write(VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB")

sheet_write(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO")

sheet_write(SE_VACUNO_CON_ANTINEUMOCOCCICA_13V,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_CON_ANTINEUMOCOCCICA_13V")

sheet_write(SE_APLICO_VACUNA_COVID,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_APLICO_VACUNA_COVID")

sheet_write(asistencia,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "asistencia")


# hacemos estadisticas segundo parte de la tarea:

# asistencia a vacunarse:

ASISTIO_A_VACUNARSE <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>%  count(ASISTIO_A_VACUNARSE,CETEC)

sheet_write(ASISTIO_A_VACUNARSE,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "ASISTIO_A_VACUNARSE")

# vacunaci?n efectiva:

VACUNACION_EFECTIVA <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(FUE_VACUNADX_EFECTIVAMENTE,CETEC)

sheet_write(VACUNACION_EFECTIVA,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "VACUNACION_EFECTIVA")


# Motivos de ausentismo:

motivo_ausentismo__de_los_que_no_fueron <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "No asistió") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LX_VACUNARON)

sheet_write(motivo_ausentismo__de_los_que_no_fueron,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "motivo_ausentismo__de_los_que_no_fueron")

# motivos de los que fueron a vacunarse y no los vacunaron

motivo_ausentismo_fue_y_no_lo_vacunaron <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE == "NO") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LX_VACUNARON)

sheet_write(motivo_ausentismo_fue_y_no_lo_vacunaron,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "motivo_ausentismo_fue_y_no_lo_vacunaron")

#  vacunatorio 

vacunatorio <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió") %>% count(A_QUE_VACUNATORIO_CONCURRIO) %>% arrange(desc(n))

sheet_write(vacunatorio,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "vacunatorio")

# tipo de atencion

atencion <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(COMO_CONSIDERA_QUE_LX_ATENDIERON,CETEC)

sheet_write(atencion,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "atencion")

# mala atencion 

mala_atencion <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & COMO_CONSIDERA_QUE_LX_ATENDIERON == "Mal" | COMO_CONSIDERA_QUE_LX_ATENDIERON == "Muy mal") %>% count(MOTIVO_DETALLADO_MALA_ATENCION)

sheet_write(mala_atencion,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "mala_atencion")

# si no lo vacunaron que le dijeron:

indicaciones <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & (FUE_VACUNADX_EFECTIVAMENTE ==  "Si con algunas dosis" | FUE_VACUNADX_EFECTIVAMENTE == "NO")) %>% count(SI_NO_LE_ADMINISTRARON_TODAS_LAS_VACUNAS_QUE_INDICACION_LE_DIERON)

sheet_write(indicaciones,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "indicaciones")


# Turno o demanda:

turno_demanda <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(LO_VACUNARON_CON_TURNO_FUERA_DEL_DIA_O_ERA_POR_DEMANDA)

sheet_write(turno_demanda,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "turno_demanda")


# asesoria 

asesoria <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(FUE_ASESORADX_ACERCA_DEL_CALENDARIO_DE_VACUNACION)

sheet_write(asesoria,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "asesoria")

# carnet

carnet <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(LE_PIDIERON_LIBRETA_SANITARIA_O_CARNET_DE_VACUNACION) 

sheet_write(carnet,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "carnet")

# dni

dni <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(LE_PIDIERON_EL_DNI_EN_EL_VACUNATORIO) 

sheet_write(dni,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "dni")

# comprobante

comprobante <- hoja_trabajo %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE != "NO") %>% count(LE_OTORGARON_UN_COMPROBANTE_DE_VACUNACION_O_PEGATINA_DE_VACUNA_O_OTRO)

sheet_write(comprobante,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "comprobante")

#  hacemos estadisticas de fecha de primer llamado:


septiembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-09-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-09-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

octubre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-10-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-10-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

noviembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-11-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-11-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)

diciembre <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-12-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-12-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO)




sheet_write(septiembre,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "septiembre")

sheet_write(octubre,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "octubre")

sheet_write(noviembre,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "noviembre")

sheet_write(diciembre,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "diciembre")


# por cetec:


#  hacemos estadisticas de fecha de primer llamado:


septiembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-09-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-09-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

octubre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-10-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-10-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

noviembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-11-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-11-30") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)

diciembre_cetec <- hoja_trabajo %>% filter(FECHA_DE_PRIMER_CONTACTO >= "2022-12-01" & FECHA_DE_PRIMER_CONTACTO <= "2022-12-31") %>% count(CONTACTADX,CANTIDAD_DE_FALLIDOS_PRIMER_CONTACTO,CANTIDAD_DE_FALLIDX_SEGUNDO_CONTACTO,CETEC)


sheet_write(septiembre_cetec,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "septiembre_cetec")

sheet_write(octubre_cetec,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "octubre_cetec")

sheet_write(noviembre_cetec,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "noviembre_cetec")

sheet_write(diciembre_cetec,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "diciembre_cetec")




# subimos actualizacion

actualizacion <- as_tibble(as.character(Sys.time()))

actualizacion %>% 
  sheet_write(
    ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
    sheet = "actualizacion")

# Derivaciones


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


# Separamos por rango etario para armar el informe:


# 2-10 años:

#Tenemos que darle formato edad a los datos para poder usar operadores de comparacion.

edad_numero <- as.numeric(hoja_trabajo$EDAD)

hoja_trabajo_2_10 <- hoja_trabajo %>% mutate(edad_numero) %>% select(-EDAD)  

hoja_trabajo_2_10 <- hoja_trabajo_2_10 %>% filter(edad_numero >= 2 & edad_numero <= 10)
                     

class(hoja_trabajo_2_10$edad_numero)



# vacunas: #Le asignamos nuevos valores a las variables:

SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 1°contacto") %>%  count(SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL)

VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB)

SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO) # (filtro con contactados para sacar casos de mas)?)

SE_VACUNO_CON_ANTINEUMOCOCCICA_13V <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_VACUNO_CON_ANTINEUMOCOCCICA_13V)

SE_APLICO_VACUNA_COVID <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 1°contacto")  %>% count(SE_APLICO_VACUNA_COVID)

asistencia <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(ASISTIRA_A_VACUNARSE_CON_LA_VACUNA_FALTANTE)


# subimos todos:

sheet_write(SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL_2_10")


sheet_write(VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB_2_10")

sheet_write(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO_2_10")

sheet_write(SE_VACUNO_CON_ANTINEUMOCOCCICA_13V,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_CON_ANTINEUMOCOCCICA_13V_2_10")

sheet_write(SE_APLICO_VACUNA_COVID,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_APLICO_VACUNA_COVID_2_10")

sheet_write(asistencia,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "asistencia_2_10")


# hacemos estadisticas segundo parte de la tarea:

# asistencia a vacunarse:

ASISTIO_A_VACUNARSE <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 2° contacto") %>%  count(ASISTIO_A_VACUNARSE,CETEC)

sheet_write(ASISTIO_A_VACUNARSE,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "ASISTIO_A_VACUNARSE_2_10")

# vacunaci?n efectiva:

VACUNACION_EFECTIVA <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(FUE_VACUNADX_EFECTIVAMENTE,CETEC)

sheet_write(VACUNACION_EFECTIVA,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "VACUNACION_EFECTIVA_2_10")


# Motivos de ausentismo:

motivo_ausentismo__de_los_que_no_fueron <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "No asistió") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LX_VACUNARON)

sheet_write(motivo_ausentismo__de_los_que_no_fueron,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "motivo_ausentismo__de_los_que_no_fueron_2_10")

# motivos de los que fueron a vacunarse y no los vacunaron

motivo_ausentismo_fue_y_no_lo_vacunaron <- hoja_trabajo_2_10 %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE == "NO") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LX_VACUNARON)

sheet_write(motivo_ausentismo_fue_y_no_lo_vacunaron,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "motivo_ausentismo_fue_y_no_lo_vacunaron_2_10")


# 11-15 años:

# Tenemos que darle formato edad a los datos para poder usar operadores de comparacion.

edad_numero <- as.numeric(hoja_trabajo$EDAD)

hoja_trabajo_11_15 <- hoja_trabajo %>% mutate(edad_numero) %>% select(-EDAD)  

hoja_trabajo_11_15 <- hoja_trabajo_11_15 %>% filter(edad_numero >= 11 & edad_numero <= 15)


class(hoja_trabajo_11_15$edad_numero)


# vacunas: #Le asignamos nuevos valores a las variables:

SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 1°contacto") %>%  count(SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL)

VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB)

SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO) # (filtro con contactados para sacar casos de mas)?)

SE_VACUNO_CON_ANTINEUMOCOCCICA_13V <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(SE_VACUNO_CON_ANTINEUMOCOCCICA_13V)

SE_APLICO_VACUNA_COVID <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 1°contacto")  %>% count(SE_APLICO_VACUNA_COVID)

asistencia <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 1°contacto") %>% count(ASISTIRA_A_VACUNARSE_CON_LA_VACUNA_FALTANTE)


# subimos todos:

sheet_write(SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_AL_INGRESO_ESCOLAR_CON_SALK_DTP_TRIPLE_VIRAL_11_15")


sheet_write(VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "VACUNAS_11_AÑOS_ANTIMENINGOCOCCICA_DTPA_TRIPLE_VIRAL_HPV_HEPATITISB_11_15")

sheet_write(SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_CON_ANTIGRIPAL_ESTE_AÑO_11_15")

sheet_write(SE_VACUNO_CON_ANTINEUMOCOCCICA_13V,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_VACUNO_CON_ANTINEUMOCOCCICA_13V_11_15")

sheet_write(SE_APLICO_VACUNA_COVID,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "SE_APLICO_VACUNA_COVID_11_15")

sheet_write(asistencia,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "asistencia_11_15")


# hacemos estadisticas segundo parte de la tarea:

# asistencia a vacunarse:

ASISTIO_A_VACUNARSE <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 2° contacto") %>%  count(ASISTIO_A_VACUNARSE,CETEC)

sheet_write(ASISTIO_A_VACUNARSE,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "ASISTIO_A_VACUNARSE_11_15")

# vacunaci?n efectiva:

VACUNACION_EFECTIVA <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 2° contacto") %>% filter(ASISTIO_A_VACUNARSE == "Asistió") %>% count(FUE_VACUNADX_EFECTIVAMENTE,CETEC)

sheet_write(VACUNACION_EFECTIVA,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "VACUNACION_EFECTIVA_11_15")


# Motivos de ausentismo:

motivo_ausentismo__de_los_que_no_fueron <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "No asistió") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LX_VACUNARON)

sheet_write(motivo_ausentismo__de_los_que_no_fueron,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "motivo_ausentismo__de_los_que_no_fueron_11_15")

# motivos de los que fueron a vacunarse y no los vacunaron

motivo_ausentismo_fue_y_no_lo_vacunaron <- hoja_trabajo_11_15 %>% filter(CONTACTADX == "Si- 2° contacto" & ASISTIO_A_VACUNARSE == "Asistió" & FUE_VACUNADX_EFECTIVAMENTE == "NO") %>% count(MOTIVO_POR_EL_CUAL_NO_ASISTIO_A_VACUNARSE_O_NO_LX_VACUNARON)

sheet_write(motivo_ausentismo_fue_y_no_lo_vacunaron,
            ss = "14hdhoe1IMPIwCzpc9nE33gc-hIYXkD4zlAWkZI75dLA",
            sheet = "motivo_ausentismo_fue_y_no_lo_vacunaron_11_15")







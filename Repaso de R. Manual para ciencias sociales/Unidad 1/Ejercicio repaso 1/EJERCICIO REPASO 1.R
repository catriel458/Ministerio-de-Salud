# EJERCICIO UNIDAD 1:
# 1) cargamos dataframe
rm(dataset_reporte_covid_sitio_gobierno)
dim(dataset_reporte_covid_sitio_gobierno)
names(dataset_reporte_covid_sitio_gobierno)
head(dataset_reporte_covid_sitio_gobierno)
summary(dataset_reporte_covid_sitio_gobierno)
library(tidyverse)
id<-filter(dataset_reporte_covid_sitio_gobierno, VALOR == 548.00)
